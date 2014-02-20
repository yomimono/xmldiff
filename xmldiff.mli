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

(** Computing diffs on XML trees.

  Algorithm adapted from
  "Tree to tree correction for document trees"
  by Barnart, Clarke and Duncan. Technical report.

  We implement the first extension of Zhang and Shasha and remove the
  InsertNode and DeleteNode: they can be replaced by InsertTree and
  DeleteTree (even if not exactly the same) and the semantic of InsertNode
  is not clear: where do we insert ? Besides, it is easier to write
  the cost function if there is only one operation to insert and
  one to delete.
*)

type name = Xmlm.name
module Nmap : Map.S with type key = name

type 'a xmlt = E of name * string Nmap.t * 'a list | D of string
type xmltree = xmltree xmlt

type label = Node of string | Text of string
type node = {
  number : int ;
  leftmost : int ;
  keyroot : bool ;
  child : int array ;
  parent : int option ;
  xml : xmltree ;
  size : int ;
  label : label ;
  }
type cost = int

type operation =
  | Replace of node * int
  | InsertTree of node * int (* insert tree from t2 as a right sibling of [int] in t1 *)
  | DeleteTree of node (* delete tree from t1 *)
  | Edit of node * node (* change label of node from t1 to label of node from t2 *)

type patch_path =
    Path_cdata of int
  | Path_node of Xmlm.name * int * patch_path option

type patch_operation =
    PInsertTree of xmltree
  | PDeleteTree
  | PUpdateCData of string
  | PUpdateNode of Xmlm.name * string Nmap.t * xmltree list
  | PReplace of xmltree

type patch = (patch_path * patch_operation) list

val xml_of_string : string -> xmltree
val xml_of_file : string -> xmltree

val atts_of_map : string Nmap.t -> (name * string) list

val string_of_xml : ?cut:bool -> xmltree -> string

val string_of_name : string * string -> string

val string_of_atts : string Nmap.t -> string

val string_of_path : patch_path -> string
val string_of_patch_operation : patch_path * patch_operation -> string
val string_of_patch : (patch_path * patch_operation) list -> string

val default_costs : operation -> int
val diff :
  ?fcost:(operation -> int) ->
  xmltree -> xmltree -> int * (patch_path * patch_operation) list
