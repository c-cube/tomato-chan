
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

(** {1 Messages between core and plugins} *)

(** An IRC message (a single line sent on a channel or in PV) *)
type irc_message = {
  from : string option  [@key "from"];
  dest : string         [@key "dest"];
  content : string      [@key "content"];
} [@@deriving yojson, show, eq]

(** Someone ("who") connected to the channel ("chan") *)
type joined_msg = {
  joined_chan : string  [@key "chan"];
  joined_who : string   [@key "who"];
} [@@deriving yojson, show, eq]

(** A message sent by the core to a plugin *)
type core_to_plugin =
  | Joined of joined_msg [@name "joined"] (** Notify: a channel was joined *)
  | MessageReceived of irc_message [@name "received"] (** Message received *)
  [@@deriving yojson, show, eq]

(** A message sent by a plugin to the core *)
type plugin_to_core =
  | Register of string [@name "register"] (** channel *)
  | Unregister of string [@name "unregister"] (** channel *)
  | RegisterAll (** register to all channels *)
  | Join of string [@name "join"] (** Ask to join a channel *)
  | SendMessage of irc_message [@name "send"] (** Send a message *)
  [@@deriving yojson, show, eq]


