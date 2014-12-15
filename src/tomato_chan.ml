(*
Copyright (c) 2014, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Main} *)

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

module Msg = TomatoMessage
module Irc = Irc_client_lwt.Client

(** {2 Helpers} *)

(* prefix of s? *)
let rec str_is_prefix s prefix i =
  i = String.length prefix
  ||
  ( i < String.length s &&
    i < String.length prefix &&
    s.[i] = prefix.[i] &&
    str_is_prefix s prefix (i+1)
  )

let section = Lwt_log.Section.make "tomato_chan"

let () =
  Lwt.async_exception_hook := (fun e ->
    print_endline ("async error: "^ Printexc.to_string e);
    flush stdout
  )

(** {2 Core code} *)

type params = {
  port : int;
  host : string;
  nick : string;
  chans : string list;
  plugin_dirs : ([`Prefix of string | `All] * string) list;
    (** list of directories to watch. If [`Prefix s] is provided
        only files starting with this prefix will be considered as plugins. *)
}

(* connect to server *)
let connect params =
  Irc.connect_by_name ~server:params.host ~port:params.port
    ~username:"tomato-chan" ~mode:0
    ~realname:"bender" ~nick:params.nick ()
  >>= function
  | Some conn -> Lwt.return conn
  | None -> Lwt.fail (Failure "could not connect")

module PluginManager = struct
  type plugin = {
    pid : int; (* shortcut *)
    name : string;  (* plugin named by its file path *)
    proc : Lwt_process.process;
    mutable threads : unit Lwt.t list;  (* reading/answering thread+termination thread *)
    mutable registered_all : bool;
    mutable registered : string list; (* set of registered chans *)
  }

  type t = {
    conn : Irc.connection_t;
    inotify : Lwt_inotify.t;
    chans : (string, unit) Hashtbl.t; (* chan *)
    mutable state : [`Init | `Stop | `Loop];
    mutable dirs : ([`Prefix of string | `All] * string) list;
    mutable plugins : plugin list;
  }

  let make conn dirs =
    Lwt_inotify.create () >>= fun inotify ->
    Lwt.return {
      conn; inotify; state=`Init; dirs;
      chans=Hashtbl.create 16; plugins = [];
    }

  let kill_plugin plugin =
    Lwt_log.ign_debug_f "kill plugin %s (pid %d)" plugin.name plugin.proc#pid;
    List.iter Lwt.cancel plugin.threads;
    plugin.proc#terminate;
    ()

  let remove_plugin m name =
    m.plugins <- List.filter
      (fun p ->
        let keep = p.name <> name in
        if not keep then kill_plugin p;
        keep
      ) m.plugins

  (* watch the given process and communicate with it *)
  let rec handle_plugin m plugin =
    Lwt_io.read_line_opt plugin.proc#stdout >>=
    function
    | None ->
        Lwt_log.ign_debug_f "could not read from plugin %s (pid %d)"
          plugin.name plugin.proc#pid;
        remove_plugin m plugin.name;
        Lwt.return_unit
    | Some line ->
        let json = Yojson.Safe.from_string line in
        match Msg.plugin_to_core_of_yojson json with
        | `Ok (Msg.Register chan) -> assert false (* TODO *)
        | `Ok (Msg.Unregister chan) ->  assert false (* TODO *)
        | `Ok Msg.RegisterAll ->
            plugin.registered_all <- true;
            handle_plugin m plugin
        | `Ok (Msg.Join chan) ->
            if not (Hashtbl.mem m.chans chan)
            then (
              Lwt_log.ign_info_f ~section "join chan %s" chan;
              Irc.send_join ~connection:m.conn ~channel:chan >>= fun () ->
              handle_plugin m plugin
            ) else handle_plugin m plugin
        | `Ok (Msg.SendMessage msg) ->
            Lwt_log.ign_debug_f ~section "send message %s" (Msg.show_irc_message msg);
            Irc.send_privmsg ~connection:m.conn
              ~target:msg.Msg.dest ~message:msg.Msg.content >>= fun () ->
            handle_plugin m plugin
        | `Error msg ->
            Lwt_log.ign_error_f "plugin error: %s" msg;
            remove_plugin m plugin.name;
            Lwt.return_unit

  (* send [msg] to the given plugin *)
  let send_plugin p msg =
    let json = Yojson.Safe.to_string (Msg.core_to_plugin_to_yojson msg) in
    Lwt_log.ign_debug_f "json for plugin: %s" json;
    Lwt_io.write_line p.proc#stdin json >>= fun () ->
    Lwt_io.flush p.proc#stdin

  (* wait for the process to terminate, and then remove the plugin *)
  let watch_plugin_process m plugin =
    plugin.proc#status >>= fun _ ->
    Lwt_log.ign_debug_f "remove terminated plugin %s" plugin.name;
    remove_plugin m plugin.name;
    Lwt.return_unit

  (* start given file, assuming it's a plugin. If it's started, remove
      it and restart it *)
  let start_plugin m file =
    Lwt_log.ign_info_f ~section "try to start %s as a plugin" file;
    (* first remove the plugin, if present *)
    remove_plugin m file;
    try
      let cmd = (file, [| file |]) in
      (* open process, redirecting stdin/stdout *)
      let proc = Lwt_process.open_process cmd in
      let plugin = {
        pid=proc#pid; proc; threads=[]; name=file;
        registered=[]; registered_all=false;
      } in
      plugin.threads <- [handle_plugin m plugin; watch_plugin_process m plugin];
      m.plugins <- plugin :: m.plugins;
      Lwt_log.ign_info_f ~section "started plugin %s (pid %d)" file proc#pid
    with e ->
      Lwt_log.ign_error_f ~section "error trying to start plugin %s: %s"
        file (Printexc.to_string e);
      ()

  (* is the filename a candidate plugin, based on its name and
    permissions? *)
  let is_potentially_plugin prefix filename =
    not (Sys.is_directory filename)
    && (
      let s = Unix.stat filename in
      s.Unix.st_kind = Unix.S_REG
      && (s.Unix.st_perm land 0o111 <> 0)
    ) && (
      match prefix with
      | `All -> true
      | `Prefix p -> str_is_prefix filename p 0
    )

  (* transform "foo!bar" into "foo" *)
  let left_of_bang s =
    try
      let i = String.index s '!' in
      String.sub s 0 i
    with Not_found -> s

  let opt_map f = function
    | None -> None
    | Some x -> Some (f x)

  (* main thread for incoming IRC events *)
  let listen_irc m =
    Irc.listen ~connection:m.conn
      ~callback:(fun ~connection ~result -> match result with
        | Irc_message.Parse_error (a,b) ->
            Lwt_log.ign_error_f ~section "irc error: %s %s" a b;
            Lwt.return_unit
        | Irc_message.Message
          {Irc_message.command="PRIVMSG"; prefix=from;
          params=dest::_; trail=Some content} ->
            (* a privmsg *)
            let from = opt_map left_of_bang from in
            let msg = {Msg.from; dest; content} in
            Lwt_log.ign_debug_f ~section "received message %s" (Msg.show_irc_message msg);
            Lwt_list.iter_p
              (fun p ->
                if p.registered_all || List.mem dest p.registered
                  then send_plugin p (Msg.MessageReceived msg)
                  else Lwt.return_unit
              ) m.plugins
        | Irc_message.Message
          {Irc_message.command="JOIN"; prefix=Some joined_who;
            params=joined_chan::_ ; _ } ->
            (* join event *)
            let joined_who = left_of_bang joined_who in
            let msg = {Msg.joined_who; joined_chan} in
            Lwt_log.ign_debug_f ~section "received %s" (Msg.show_joined_msg msg);
            Lwt_list.iter_p
              (fun p ->
                send_plugin p Msg.(Joined msg)
              ) m.plugins
        | Irc_message.Message {Irc_message.prefix;command;params;trail} ->
            Lwt_log.debug_f ~section
              "received: {prefix=%s; command=%s;params=%s;trail=%s}"
              ([%show: string option] prefix) command
              ([%show: string list] params) ([%show: string option] trail)
      )

  (* start plugins, in the initial state, and also setup watches and
    listen to irc events *)
  let really_start m =
    Lwt_log.ign_info ~section "start plugins...";
    m.state <- `Loop;
    Lwt_list.iter_p
      (fun (prefix, dir) ->
        Lwt_log.ign_debug_f ~section "start plugins in dir %s" dir;
        (* setup watch *)
        Lwt_inotify.add_watch m.inotify dir
          [Inotify.S_Create; Inotify.S_Modify; Inotify.S_Delete_self; Inotify.S_Close_write]
        >>= fun _watch ->
        (* start plugins in this dir *)
        let files = Sys.readdir dir in
        Array.iter
          (fun f ->
            if is_potentially_plugin prefix f
              then start_plugin m (Filename.concat dir f)
          ) files;
        Lwt.return_unit
      ) m.dirs

  let start m =
    match m.state with
    | `Loop
    | `Stop -> failwith "plugin manager already started"
    | `Init -> really_start m

  (* find whether the file is a potential plugin, based on its directory *)
  let is_potentially_plugin_from_dir m filename =
    let dirname = Filename.dirname filename in
    let rec find = function
      | [] -> failwith ("could not find where file " ^ filename ^ " comes from")
      | (p, d) :: _ when d=dirname -> is_potentially_plugin p filename
      | _ :: tail -> find tail
    in
    find m.dirs

  (* listen for events on the filesystem *)
  let rec watch_inotify m =
    Lwt_inotify.read m.inotify
    >>= function
    | (_,_,_, None) ->
        watch_inotify m
    | (_,_,_, Some file) ->
        Lwt_log.ign_debug_f ~section "FS event: file %s" file;
        if is_potentially_plugin_from_dir m file
          then start_plugin m file;
        watch_inotify m

  let loop m =
    let t1 = listen_irc m
    and t2 = watch_inotify m in
    Lwt.join [t1; t2]

  let close m =
    Lwt_inotify.close m.inotify >>= fun () ->
    List.iter kill_plugin m.plugins;
    Lwt.return_unit
end

(* start plugins in the given directories *)
let start_plugins dirs =
  Lwt.return_unit

let main params =
  Lwt.catch
    (fun () ->
      connect params >>= fun conn ->
      Lwt_log.ign_info ~section "connected.";
      Lwt_unix.sleep 3. >>= fun () ->
      Lwt_list.iter_p
        (fun channel ->
          Lwt_log.ign_info_f ~section "join chan %s" channel;
          Irc.send_join ~connection:conn ~channel
        ) params.chans
      >>= fun () ->
      PluginManager.make conn params.plugin_dirs >>= fun m ->
      PluginManager.start m >>= fun () ->
      PluginManager.loop m >>= fun () ->
      Lwt_log.info_f "exit main loop"
    )
    (fun e ->
      Lwt_log.ign_error_f ~section "error: %s" (Printexc.to_string e);
      exit 1
    )

(** {2 Main} *)

let params =
  let progdir = Filename.dirname Sys.argv.(0) in
  let absdir = if Filename.is_relative progdir
    then Filename.concat (Sys.getcwd ()) progdir
    else progdir
  in
  ref {
    port = 6667;
    host = "irc.freenode.net";
    nick = "TomatoChan";
    chans = [];
    plugin_dirs = [ `Prefix "tomato_plugin_", absdir ]
  }

let usage = "tomato_chan [options]"
let do_nothing _ = ()
let add_dir s =
  params := {!params with plugin_dirs = (`All, s) :: !params.plugin_dirs}
let add_chan s =
  params := {!params with chans = s :: !params.chans}

let options =
  [ "-p", Arg.Int (fun port -> params := {!params with port}), " IRC server port"
  ; "-host", Arg.String (fun host -> params := {!params with host}), " IRC server hostname"
  ; "-nick", Arg.String (fun nick -> params := {!params with nick}), " nickname"
  ; "-dir", Arg.String add_dir, " add directory of plugins to watch"
  ; "-chan", Arg.String add_chan, " add chan to connect to"
  ; "-debug", Arg.Unit (fun () -> Lwt_log.add_rule "*" Lwt_log.Debug), " enable debug"
  ]

let () =
  Arg.parse (Arg.align options) do_nothing usage;
  Lwt_main.run (main !params)
