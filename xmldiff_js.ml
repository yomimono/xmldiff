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

(** Applying patches to DOM.*)

module Xdiff = Xmldiff

let log s = Firebug.console##log (Js.string s);;



let dom_of_xml =
  let rec map (doc : Dom_html.document Js.t) = function
    `D s ->
      let n = doc##createTextNode (Js.string s) in
      (n :> Dom.node Js.t)
  | `E (name, atts, subs) ->
      let n =
        match name with
          ("", tag) -> doc##createElement (Js.string tag)
        | (uri, tag) -> doc##createElementNS (Js.string uri, Js.string tag)
      in
      Xdiff.Nmap.iter
        (fun name v ->
          let v = Js.string v in
           match name with
             ("", att) -> ignore (n##setAttribute (Js.string att, v))
           | (uri, att) ->
               try
                 ignore (Js.Unsafe.meth_call n "setAttributeNS"
                  (Array.map Js.Unsafe.inject [| Js.string uri ; Js.string att ; v |]))
                   (* FIXME: use setAttributeNS when will be available *)
               with _ ->
                   log ("could not add attribute "^(Xdiff.string_of_name name))
        )
        atts;
      let subs = List.map (map doc) subs in
      List.iter (Dom.appendChild n) subs;
      (n :> Dom.node Js.t)
  in
  fun t ->
    let doc = Dom_html.document in
    map doc t
;;

let dom_node_by_path skip_node path =
  let doc = Dom_html.document in
  let rec next node path =
    let node = Js.Opt.get (node##nextSibling)
      (fun _ -> log ((Js.to_string node##nodeName)^" has no sibling"); raise Not_found)
    in
    iter node path
  and on_child node path =
    let node = Js.Opt.get (node##firstChild)
      (fun _ -> log ((Js.to_string node##nodeName)^" has no child"); raise Not_found) in
    iter node path
  and iter node path =
    if skip_node node then
      next node path
    else
      match path with
        Xdiff.Path_cdata n when (node##nodeType) = Dom.TEXT ->
          if n = 0 then
            node
          else
            next node (Xdiff.Path_cdata (n-1))
      | Xdiff.Path_node (name, n, more) when node##nodeType = Dom.ELEMENT ->
          let s_name = String.lowercase (Xdiff.string_of_name name) in
          let node_name = Js.to_string node##nodeName in
          (*log ("name="^s_name^", nodeName="^node_name^", n="^(string_of_int n));*)
          let node_name = String.lowercase node_name in
          if s_name = node_name then
            if n = 0 then
              match more with
                None -> node
              | Some p -> on_child node p
            else
              next node (Xdiff.Path_node (name, n-1, more))
          else
            next node (Xdiff.Path_node (name, n, more))
      | p -> next node p
  in
  on_child (doc:>Dom.node Js.t) path

let apply_patch_operation skip_node (path, op) =
  log (Xmldiff.string_of_patch_operation (path, op));
  let parent node = Js.Opt.get (node##parentNode) (fun _ -> assert false) in
  let apply node op =
    match op with
    | Xdiff.PReplace tree ->
        let parent = parent node in
        ignore(parent##replaceChild (dom_of_xml tree, node))
    | Xdiff.PInsert (tree, `FirstChild) ->
        ignore(node##insertBefore (dom_of_xml tree, (node##firstChild)))
    | Xdiff.PInsert (tree, `After) ->
        let parent = parent node in
        ignore(parent##insertBefore (dom_of_xml tree, (node##nextSibling)))
    | Xdiff.PDelete ->
        let parent = parent node in
        ignore(parent##removeChild(node))
    | Xdiff.PUpdateCData s ->
        let parent = parent node in
        let text = Dom_html.document##createTextNode (Js.string s) in
        ignore(parent##replaceChild ((text :> Dom.node Js.t), node))
    | Xdiff.PUpdateNode (name, atts) when node##nodeType = Dom.TEXT ->
        let n = dom_of_xml (`E(name,atts,[])) in
        let parent = parent node in
        ignore(parent##replaceChild (n, node))
    | Xdiff.PUpdateNode (name, atts) when node##nodeType = Dom.ELEMENT ->
        let parent = parent node in
        let n = dom_of_xml (`E(name,atts,[])) in
        let children = node##childNodes in
        for i=0 to children##length-1 do
          Js.Opt.iter (node##firstChild) (fun node -> Dom.appendChild n node) ;
        done;
        ignore(parent##replaceChild (n, node))
    | Xdiff.PUpdateNode _ -> assert false
    | Xdiff.PMove (newpath, pos) ->
        let parent_node = parent node in
        let removed_node = parent_node##removeChild(node) in
        let new_loc = dom_node_by_path skip_node newpath in
        match pos with
        | `FirstChild ->
             ignore(new_loc##insertBefore(removed_node, (new_loc##firstChild)))
        | `After ->
            let new_parent = parent new_loc in
            ignore(new_parent##insertBefore(removed_node, (new_loc##nextSibling)));
  in
  let node = dom_node_by_path skip_node path in
  apply (node:>Dom.node Js.t) op
;;

let apply_dom_patch skip_node l = List.iter (apply_patch_operation skip_node) l ;;
