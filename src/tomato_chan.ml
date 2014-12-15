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
    proc : Lwt_process.process_full;
    mutable thread : unit Lwt.t;  (* reading/answering thread *)
    mutable registered_all : bool;
    mutable registered : string list; (* set of registered chans *)
  }

  type t = {
    conn : Irc.connection_t;
    inotify : Lwt_inotify.t;
    chans : (string, unit) Hashtbl.t; (* chan *)
    mutable state : [`Init | `Stop | `Loop];
    mutable dirs : ([`Prefix of string | `All] * string) list;
    mutable plugins : (string,plugin) Hashtbl.t;
  }

  let make conn dirs =
    Lwt_inotify.create () >>= fun inotify ->
    Lwt.return {
      conn; inotify; state=`Init; dirs;
      chans=Hashtbl.create 16; plugins = Hashtbl.create 16;
    }

  let join m chan =
    if not (Hashtbl.mem m.chans chan)
    then Irc.send_join ~connection:m.conn ~channel:chan
    else Lwt.return_unit

  (* watch the given process and communicate with it *)
  let watch_plugin m plugin =
    let lines = Lwt_io.read_lines plugin.proc#stdout in
    Lwt_stream.iter_s
      (fun line ->
        let json = Yojson.Safe.from_string line in
        match Msg.plugin_to_core_of_yojson json with
        | `Ok (Msg.Register chan) -> assert false (* TODO *)
        | `Ok (Msg.Unregister chan) ->  assert false (* TODO *)
        | `Ok Msg.RegisterAll ->
            plugin.registered_all <- true;
            Lwt.return_unit
        | `Ok (Msg.Join chan) -> join m chan
        | `Ok (Msg.SendMessage msg) ->
            Lwt_log.ign_debug_f ~section "send message %s" (Msg.show_irc_message msg);
            Irc.send_privmsg ~connection:m.conn
              ~target:msg.Msg.dest ~message:msg.Msg.content
        | `Error msg -> Lwt.fail (Failure msg)
      ) lines

  (* send [msg] to the given plugin *)
  let send_plugin p msg =
    let json = Yojson.Safe.to_string (Msg.core_to_plugin_to_yojson msg) in
    Lwt_io.write_line p.proc#stdin json >>= fun () ->
    Lwt_io.flush p.proc#stdin

  (* start given file, assuming it's a plugin and it's not started already *)
  let start_plugin m file =
    Lwt_log.ign_info_f ~section "try to start %s as a plugin" file;
    if Hashtbl.mem m.plugins file
    then ()
    else try
      let cmd = (file, [| |]) in
      let proc = Lwt_process.open_process_full cmd in
      let plugin = {
        pid=proc#pid; proc; thread=Lwt.return_unit;
        registered=[]; registered_all=false;
      } in
      plugin.thread <- watch_plugin m plugin;
      Hashtbl.add m.plugins file plugin;
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
            let msg = {Msg.from; dest; content} in
            Lwt_log.ign_debug_f ~section "received message %s" (Msg.show_irc_message msg);
            Hashtbl.iter
              (fun _ p ->
                if p.registered_all || List.mem dest p.registered
                  then Lwt.async (fun () -> send_plugin p (Msg.MessageReceived msg))
              ) m.plugins;
            Lwt.return_unit
        | Irc_message.Message
          {Irc_message.command="JOIN"; prefix=Some joined_who;
            params=joined_chan::_ ; _ } ->
            (* join event *)
            let msg = {Msg.joined_who; joined_chan} in
            Lwt_log.ign_debug_f ~section "received %s" (Msg.show_joined_msg msg);
            Hashtbl.iter
              (fun _ p ->
                Lwt.async (fun () -> send_plugin p Msg.(Joined msg))
              ) m.plugins;
            Lwt.return_unit
        | Irc_message.Message _ ->
            Lwt.return_unit
      )

  (* start plugins, in the initial state, and also setup watches and
    listen to irc events *)
  let really_start m =
    Lwt_log.ign_info ~section "start plugins...";
    m.state <- `Loop;
    Lwt_list.iter_p
      (fun (prefix, dir) ->
        (* setup watch *)
        Lwt_inotify.add_watch m.inotify dir
          [Inotify.S_Create; Inotify.S_Delete_self; Inotify.S_Close_write]
        >>= fun _watch ->
        (* start plugins in this dir *)
        let files = Sys.readdir dir in
        Array.iter
          (fun f ->
            if is_potentially_plugin prefix f
              then start_plugin m f
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
        if is_potentially_plugin_from_dir m file
          then start_plugin m file;
        watch_inotify m

  let loop m =
    let t1 = listen_irc m
    and t2 = watch_inotify m in
    Lwt.join [t1; t2]

  let close m =
    Lwt_inotify.close m.inotify >>= fun () ->
    Hashtbl.iter
      (fun _ plugin ->
        Lwt.cancel plugin.thread;
        plugin.proc#terminate;
      ) m.plugins;
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

let params = ref {
  port = 6667;
  host = "irc.freenode.net";
  nick = "TomatoChan";
  chans = [];
  plugin_dirs = [ `Prefix "tomato_", Filename.dirname Sys.argv.(0) ]
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
