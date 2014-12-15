
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

(** {1 "echo" plugin}

Prints received messages *)

let main () =
  let stream = Yojson.Safe.stream_from_channel stdin in
  Stream.iter
    (fun json ->
      match TomatoMessage.core_to_plugin_of_yojson json with
      | `Ok (TomatoMessage.MessageReceived {TomatoMessage.from; dest; content}) ->
          Printf.eprintf "message from %s to %s: %s\n"
            ([%show:string option] from) dest content;
          flush stderr
      | `Ok (TomatoMessage.Joined {TomatoMessage.joined_chan; joined_who}) ->
          Printf.eprintf "%s joined chan %s\n" joined_who joined_chan;
          flush stderr;
      | `Error msg ->
          Printf.eprintf "error in tomato_plugin_echo: %s" msg;
          flush stderr;
          failwith msg
    ) stream

let () =
  main ()
