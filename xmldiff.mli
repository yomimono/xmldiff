module Nmap :
  sig
    type key = Xtmpl.name
    type 'a t = 'a Xtmpl.Name_map.t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
type name = Xmlm.name
type 'a xmlt = E of name * string Nmap.t * 'a list | D of string
type xmltree = xmltree xmlt
type xmlnode = int option * xmlnode xmlt
type label = Node of string | Text of string
type node = {
  number : int;
  leftmost : int;
  keyroot : bool;
  child : int array;
  parent : int option;
  xml : xmltree;
  size : int;
  label : label;
}
val xmlnode_of_t : node array -> (int option * 'a xmlt as 'a)
type cost = int
type operation =
    Replace of node * int
  | InsertTree of node * int
  | DeleteTree of node
  | Edit of node * node
type cost_fun = operation -> cost
type actions = cost * operation list
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
val min_action : 'a * 'b -> 'a * 'b -> 'a * 'b
val add_action : int * 'a list -> ('a -> int) -> 'a -> int * 'a list
val add_actions : int * 'a list -> int * 'a list -> int * 'a list
val xml_of_source : string -> Xmlm.source -> ('a xmlt as 'a)
val xml_of_string : string -> ('a xmlt as 'a)
val xml_of_file : string -> ('a xmlt as 'a)
val atts_of_xml_atts : 'a Nmap.t -> (Nmap.key * 'a) list
val string_of_xml : ?cut:bool -> ('a xmlt as 'a) -> string
val string_of_name : string * string -> string
val string_of_atts : string Nmap.t -> string
val label_of_xml : 'a xmlt -> label
val short_label : 'a xmlt -> string
val dot_of_t : node array -> string
val t_of_xml : xmltree -> node array
val compute :
  (operation -> int) -> node array -> node array -> int * operation list
val string_of_action : operation -> string
type cur_path = N of Xmlm.name | CData
module Cur_path :
  sig
    type key = cur_path
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
val cur_path_get : Cur_path.key -> int Cur_path.t -> int
val cur_path_inc : Cur_path.key -> int Cur_path.t -> int Cur_path.t
val patch_path_of_cur_path_list : (cur_path * int) list -> patch_path
val path_of_id : (int option * 'a xmlt as 'a) -> int -> patch_path
val xmlnode_of_xmltree : ('a xmlt as 'a) -> ('c option * 'b xmlt as 'b)
val patch_xmlnode :
  ('b option * 'a xmlt as 'a) -> patch_path -> patch_operation -> 'a
val patch_of_action :
  (int option * 'a xmlt as 'a) * (patch_path * patch_operation) list ->
  operation -> 'a * (patch_path * patch_operation) list
val xmltree_of_xmlnode : ('b * 'a xmlt as 'a) -> ('c xmlt as 'c)
val mk_replace : operation list -> operation list
val file_of_string : file:string -> string -> unit
val patch_of_actions :
  node array ->
  node array -> operation list -> (patch_path * patch_operation) list
val string_of_path : patch_path -> string
val string_of_patch_operation : patch_path * patch_operation -> string
val string_of_patch : (patch_path * patch_operation) list -> string
val default_costs : operation -> int
val diff :
  ?fcost:(operation -> int) ->
  xmltree -> xmltree -> int * (patch_path * patch_operation) list
