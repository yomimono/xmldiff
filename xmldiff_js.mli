(*********************************************************************************)
(*                Xmldiff                                                        *)
(*                                                                               *)
(*    Copyright (C) 2014 Institut National de Recherche en Informatique          *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU Lesser General Public License version        *)
(*    3 as published by the Free Software Foundation.                            *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU Lesser General Public           *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*                                                                               *)
(*********************************************************************************)

(** Applying patches to DOM. To use with [js_of_ocaml].*)

val log : string -> unit

(** Create a DOM node tree from the given XML tree.
  @param doc Default is [Dom_html.document].
*)
val dom_of_xml : ?doc:Dom_html.document Js.t ->
   Xmldiff.xmltree -> Dom.node Js.t

(** [dom_node_by_path path] returns the DOM node corresponding to
 the given [path] or raise [Not_found].
   @param skip_node is a function taking the current node and returning [true]
   if the node must not be taken into account during the lookup. Default is
   to not skip any node (a function returning always [false]).
  @param doc Default is [Dom_html.document]. *)
val dom_node_by_path :
  ?doc:Dom_html.document Js.t ->
  ?skip_node: (Dom.node Js.t -> bool) -> Xmldiff.patch_path -> Dom.node Js.t

(** [apply_dom_patch patch] applies the given patch to the DOM.
  @param skip_node see {!dom_node_by_path}.
  @param doc Default is [Dom_html.document].
*)
val apply_dom_patch :
  ?doc:Dom_html.document Js.t ->
  ?skip_node: (Dom.node Js.t -> bool) ->
    (Xmldiff.patch_path * Xmldiff.patch_operation) list -> unit
