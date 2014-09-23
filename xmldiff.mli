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
  by Barnart, Clarke and Duncan. Technical report available
  {{:http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.62.6907}here}.

  We implement the first extension of Zhang and Shasha and remove the
  InsertNode and DeleteNode operations: they can be replaced by InsertBefore,
  InsertAfter and DeleteTree (even if not exactly the same) and the semantic of InsertNode
  is not clear in the article (where do we insert ?).
  Besides, it is easier to write the cost function if there is only
  one operation to insert (before or after) and one to delete.
*)

type name = Xmlm.name
  (** [= string * string] . Representation
     of tags and attribute names. *)

(** Mapping over names. *)
module Nmap : Map.S with type key = name

(** XML tree. The type is parametrized because it is shared
  with an internal richer representation.
*)
type 'a xmlt = [
| `E of name * string Nmap.t * 'a list (** A node [(tag, attributes, children)] *)
| `D of string (** CDATA leaf *)
]
;;

(** Simple XML tree. *)
type xmltree = xmltree xmlt

type label = Node of string | Text of string

(** A path to a node in an XML tree where to perform an operation. *)
type patch_path =
    Path_cdata of int (** [Path_cdata n] refers to the [n]th CData leaf. *)
  | Path_node of Xmlm.name * int * patch_path option
    (** [Path_node (tag, n, more)] refers to the [n]th element with tag [tag].
      If [more <> None] then the path goes on into the children of the referenced node. *)

type position = [`FirstChild | `After]

(** The patch operations. Each operation is to be performed at a
  given node (position) in the tree, referenced by a {!patch_path}. *)
type patch_operation =
  | PInsert of xmltree * position (** Insert the given XML tree as a right sibling of the referenced node. *)
  | PDelete (** Delete the referenced node. *)
  | PUpdateCData of string (** Change the referenced node to a CData with the given contents. *)
  | PUpdateNode of Xmlm.name * string Nmap.t
      (** Update the referenced node to be a tag with the given attributes and no child. *)
  | PReplace of xmltree
      (** Replace the referenced node by the given tree. *)
  | PMove of patch_path * position

type patch = (patch_path * patch_operation) list

(** [diff t1 t2] returns the pair [(c, p)], with [c] being the cost
  to change [t1] into [t2] and [p] the corresponding patch.
  @param fcost can be used to specify alternative cost functions.
  @param cut is called on XML tree tag nodes; returning [true] means
  that that subnodes won't be compared separately, but the whole node
  will be compared as is. This is useful in case of big XML trees, otherwise
  a lot of comparisons are done between the two trees. For example,
  one can cut on "p", "pre", "ul" and "code" tags in a regular web page.
  Default behaviour is to cut nothing.
*)
val diff :
  ?cut: (name -> string Nmap.t -> xmltree list -> bool) ->
  xmltree -> xmltree -> patch

(** {2 Utilities} *)

val xml_of_string : string -> xmltree
val xml_of_file : string -> xmltree

val atts_of_map : string Nmap.t -> (name * string) list

val string_of_xml : ?cut:bool -> xmltree -> string

val string_of_name : string * string -> string

val string_of_atts : string Nmap.t -> string

val string_of_path : patch_path -> string
val string_of_patch_operation : patch_path * patch_operation -> string
val string_of_patch : patch -> string
