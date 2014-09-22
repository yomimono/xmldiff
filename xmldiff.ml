(** *)

module Smap = Map.Make(String)

module Nmap =
 Map.Make (
   struct
     type t = string * string
     let compare (p1, s1) (p2, s2) =
       match String.compare p1 p2 with
         0 -> String.compare s1 s2
       | n -> n
   end)

type name = Xmlm.name
type 'a xmlt = [
    `E of name * string Nmap.t * 'a list
  | `D of string ]
type xmltree = xmltree xmlt
type xmlnode = int option * xmlnode xmlt

type label = Node of string | Text of string

type node = {
  number : int ;
  children : int array ;
  parent : int option ;
  xml : xmltree ;
  weight : float ;
  hash : string ;
  label : label ;
  can_update : bool ;
  mutable matched : int option ;
  }

type doc = {
  height: int ;
  w0 : float ;
  nodes : node array ;
 }

let string_of_name = function
  ("",s) -> s
| (ns,s) -> ns^":"^s
;;

let string_of_atts map =
  let l =
    Nmap.fold
      (fun name s acc ->
         (Printf.sprintf "%s=%S" (string_of_name name) s) :: acc)
      map []
  in
  String.concat " " l

let label_of_xml = function
| `D s -> Text s
| `E (tag, atts, _) ->
    Node
      (Printf.sprintf "<%s %s>"
       (string_of_name tag) (string_of_atts atts))

let hash xml =
  let s =
    match xml with
      `D s -> "!" ^ s
    | `E _ -> "<" ^ (Marshal.to_string xml [])
  in
  Digest.string s

let atts_of_map map =
  List.rev
    (Nmap.fold
     (fun name s acc -> (name, s) :: acc)
       map [])

let string_of_xml ?(cut=false) tree =
  let tree =
    if cut then
      match tree with
        `D _ -> tree
      | `E (name,atts,_) -> `E (name,atts,[])
    else
      tree
  in
  let b = Buffer.create 256 in
  let ns_prefix s = Some s in
  let output = Xmlm.make_output ~ns_prefix ~decl: false (`Buffer b) in
  let frag = function
  | `E (tag, atts, childs) ->
      let atts = atts_of_map atts in
      `El ((tag, atts), childs)
  | `D d -> `Data d
  in
  Xmlm.output_doc_tree frag output (None, tree);
  Buffer.contents b

let xmlnode_of_t t =
  let len = Array.length t in
  let rec build n =
    let xml = t.(n).xml in
    match xml with
      `D s -> (Some n, `D s)
    | `E (tag,atts,_) ->
        let children = List.map build (Array.to_list t.(n).children) in
        (Some n, `E (tag, atts, children))
  in
  build (len-1)
;;

let weight xml children =
  match xml with
    `D s -> 1. +. log (float (1 + String.length s))
  | `E _ -> List.fold_left (fun acc c -> c.weight +. acc) 1. children

let t_of_xml =
  let rec iter ?cut (n0, acc, h) xml =
    let (label, subs, can_update) =
      match xml with
      | `D _ -> (label_of_xml xml, [], true)
      | `E (tag, atts, l) ->
          match cut with
          | Some f when f tag atts l -> (Node (string_of_xml xml), [], false)
          | _ -> (label_of_xml xml, l, true)
    in
    let (n, children, h_children) = List.fold_left (iter ?cut) (n0, [], 0) subs in
    let children = List.rev_map (fun node -> { node with parent = Some n }) children in
    let hash = hash xml in
    let weight = weight xml children in
    let node =
      { number = n ;
        children = Array.of_list (List.map (fun node -> node.number) children) ;
        parent = None ;
        xml ; label ; hash ; weight ;
        can_update ;
        matched = None ;
      }
    in
    let acc = match children with [] -> acc | _ -> acc @ children in
    (n+1, node :: acc, max h (h_children + 1))
  in
  fun ?cut xml ->
    let (_, l, h) = iter ?cut (0, [], 0) xml in
    let t = Array.of_list l in
    Array.sort (fun n1 n2 -> n1.number - n2.number) t;
    Array.iteri (fun i node ->
       (*
       prerr_endline (Printf.sprintf "i=%d, keyroot=%b, node.number=%d, parent=%s, xml=%s" i node.keyroot node.number
         (match node.parent with None -> "" | Some n -> string_of_int n)
         (short_label node.xml)
       );*)
       assert (i = node.number)
    ) t;
    { height = h;
      nodes =  t;
      w0 = t.(Array.length t - 1).weight ;
    }
;;




type cost = int

type operation =
  | Replace of node * int
  | InsertBefore of node * int (* insert tree from t2 as a left sibling of [int] in t1 *)
  | InsertAfter of node * int (* insert tree from t2 as a right sibling of [int] in t1 *)
  | DeleteTree of node (* delete tree from t1 *)
  | Edit of node * node (* change label of node from t1 to label of node from t2 *)

type cost_funs = {
    cost_insert : int -> xmltree -> cost ;
    cost_delete : int -> xmltree -> cost ;
    cost_edit : (label * xmltree) -> (label * xmltree) -> cost ;
  }

type actions = cost * operation list

type patch_path =
  Path_cdata of int
| Path_node of Xmlm.name * int * patch_path option

type patch_operation =
  PInsertBefore of xmltree
| PInsertAfter of xmltree
| PDeleteTree
| PUpdateCData of string
| PUpdateNode of Xmlm.name * string Nmap.t
| PReplace of xmltree

type patch = (patch_path * patch_operation) list


let rec xml_of_source s_source source =
 try
    let ns s = Some s in
    let input = Xmlm.make_input ~ns ~enc: (Some `UTF_8) source in
    let el (tag, atts) childs =
      let atts = List.fold_left
        (fun map (name, v) -> Nmap.add name v map) Nmap.empty atts
      in
      `E (tag, atts, childs)
    in
    let data d = `D d in
    let (_, tree) = Xmlm.input_doc_tree ~el ~data input in
    tree
  with
    Xmlm.Error ((line, col), error) ->
      let msg = Printf.sprintf "%sLine %d, column %d: %s"
        s_source line col (Xmlm.error_message error)
      in
      failwith msg
  | Invalid_argument e ->
      let msg = Printf.sprintf "%sInvalid_argumen(%s)" s_source e in
      failwith msg

and xml_of_string s =
  xml_of_source s (`String (0, s))
;;

let xml_of_file file =
  let ic = open_in file in
  try
    let xml = xml_of_source
      (Printf.sprintf "File %S, " file) (`Channel ic)
    in
    close_in ic;
    xml
  with
    e ->
      close_in ic;
      raise e
;;


let short_label = function
  `E ((s1,s2), _, _) -> s1^":"^s2
| `D _ -> "<pcdata>"
;;

let dot_of_t t =
  let b = Buffer.create 256 in
  let p b = Printf.bprintf b in
  p b "digraph g {\nrankdir=TB;\nordering=out;\n";
  Array.iter
    (fun node ->
       p b "\"N%d\" [ label=\"%d: %s \", fontcolor=black ];\n"
         node.number node.number (short_label node.xml);
       Array.iter (fun i -> p b "\"N%d\" -> \"N%d\";\n" node.number i) node.children ;
    )
    t.nodes;
  p b "}\n";
  Buffer.contents b
;;



let string_of_action = function
| Replace (n2, i) -> Printf.sprintf "Replace (%d, %d): %s" n2.number i (string_of_xml ~cut:true n2.xml)
| InsertBefore (n2, i) -> Printf.sprintf "InsertBefore (%d, %d): %s" n2.number i (string_of_xml ~cut:true n2.xml)
| InsertAfter (n2, i) -> Printf.sprintf "InsertAfter (%d, %d): %s" n2.number i (string_of_xml ~cut:true n2.xml)
| DeleteTree n1 -> Printf.sprintf "DeleteTree(%d): %s" n1.number (string_of_xml ~cut: true n1.xml)
| Edit (n1, n2) -> Printf.sprintf "Edit(%d,%d): %s -> %s" n1.number n2.number
  (string_of_xml ~cut: true n1.xml) (string_of_xml ~cut: true n2.xml)
;;

let print pref i j (cost, actions) =
  Printf.printf "%s:%d,%d: cost=%d, actions=%s\n" pref i j cost
    (String.concat ", " (List.map string_of_action actions))

let build_hash_map =
  let add map node =
    let l =
      try Smap.find node.hash map
      with Not_found -> []
    in
    Smap.add node.hash (node.number :: l) map

  in
  fun t -> Array.fold_left add Smap.empty t.nodes
;;


let rec get_nth_parent t i level =
  match t.nodes.(i).parent with
    None -> None
  | Some p ->
      if level <= 1 then
        Some p
      else
        get_nth_parent t p (level-1)

let d_of_node t i =
  1. +. (float t.height) *. t.nodes.(i).weight /. t.w0

let match_nodes t1 t2 i j =
  t1.nodes.(i).matched <- Some j;
  t2.nodes.(j).matched <- Some i

let match_ancestors t1 t2 i j =
  let max_level = int_of_float (d_of_node t2 j) in
  let rec iter i j level =
    if level > max_level then
      ()
    else
      match t1.nodes.(i).parent, t2.nodes.(j).parent with
        Some p1, Some p2
          when t1.nodes.(p1).label = t2.nodes.(p2).label ->
          match_nodes t1 t2 p1 p2;
          iter p1 p2 (level + 1)
      | _ -> ()
  in
  iter i j 1

let rec best_candidate ?(level=1) t1 t2 j cands =
  let d = d_of_node t2 j in
  let parent_j = get_nth_parent t2 j level in
  match parent_j with
    None -> None
  | Some parent_j ->
      let test_parent i =
        match get_nth_parent t1 i level with
          Some p -> t1.nodes.(p).matched = Some parent_j
        | _ -> false
      in
      try Some (List.find test_parent cands)
      with Not_found ->
          if float level < d then
            best_candidate ~level: (level+1) t1 t2 j cands
          else
            None

let candidates hash_t1 t2 j =
  try Smap.find t2.nodes.(j).hash hash_t1 with Not_found -> []

let match_candidate hash_t1 t1 t2 j =
  match candidates hash_t1 t2 j with
    [] -> None
  | [i] -> Some i
  | l -> best_candidate t1 t2 j l

let compute fc t1 t2 =
  let weight_queue = Queue.create () in
  Queue.add 0 weight_queue ;
  let hash_t1 = build_hash_map t1 in
  while not (Queue.is_empty weight_queue) do
    let j = Queue.pop weight_queue in
    (* test whether j has already a match in t1 ? *)
    match match_candidate hash_t1 t1 t2 j with
      Some i ->
        match_nodes t1 t2 i j;
        match_ancestors t1 t2 i j
    | None ->
        let t = Array.map (Array.get t2.nodes) t2.nodes.(j).children in
        Array.sort (fun n1 n2 -> Pervasives.compare n2.weight n1.weight) t;
        Array.iteri (fun i _ -> Queue.add i weight_queue) t
  done;
  assert false

type cur_path = N of Xmlm.name | CData
module Cur_path = Map.Make (struct type t = cur_path let compare = Pervasives.compare end)
let cur_path_get cp map =
  try Cur_path.find cp map
  with Not_found -> 0
;;
let cur_path_inc cp map =
  let n = cur_path_get cp map in
  Cur_path.add cp (n+1) map
;;

let patch_path_of_cur_path_list =
  let rec iter (cp, n) acc =
    match acc, cp with
      (None, CData) -> Some (Path_cdata n)
    | (Some _, CData) -> assert false
    | (_, N name) -> Some (Path_node (name, n, acc))
  in
  fun l ->
    match List.fold_right iter l None with
      None -> assert false
    | Some p -> p
;;

let path_of_id =
  let cp_of_xml = function
    `D s -> CData
  | `E (name,_,_) -> N name
  in
  let rec iter i path cur_path = function
  | (Some j, xml) when i = j ->
      begin
        let cp = cp_of_xml xml in
        let path = (cp, cur_path_get cp cur_path) :: path in
        patch_path_of_cur_path_list (List.rev path)
      end
  | (Some j, _) when j < i -> raise Not_found
  | (_, `D _) -> raise Not_found
  | (_, `E (name, atts, subs)) ->
      (* None or Some j with j > i, let's go down after
         adding cur_path to path
         *)
      let cpt = cur_path_get (N name) cur_path in
      let path = (N name, cpt) :: path in
      iter_list i path Cur_path.empty subs

  and iter_list i path cur_path = function
    [] -> raise Not_found
  | h :: q ->
    try iter i path cur_path h
    with Not_found ->
      let cur_path =
        let cp = cp_of_xml (snd h) in
        cur_path_inc cp cur_path
      in
      iter_list i path cur_path q
  in
  fun xmlnode i ->
    try iter i [] Cur_path.empty xmlnode
    with Not_found ->
      let msg = "Id "^(string_of_int i)^" not found" in
      failwith msg
;;

let rec xmlnode_of_xmltree = function
  `D s -> (None, `D s)
| `E (name,atts,subs) ->
  (None, `E (name,atts, List.map xmlnode_of_xmltree subs))
;;

let patch_xmlnode t path op =
  let apply xml op =
    match xml, op with
    | _, PReplace tree -> [xmlnode_of_xmltree tree]
    | _, PInsertBefore tree -> [ xmlnode_of_xmltree tree ; xml ]
    | _, PInsertAfter tree -> xml :: [xmlnode_of_xmltree tree]
    | _, PDeleteTree -> []
    | (x, _), PUpdateCData s -> [(x, `D s)]
    | (x, `D _), PUpdateNode (name, atts) -> [x, `E (name,atts,[])]
    | (x, `E (_,_,subs)), PUpdateNode (name, atts) -> [x, `E (name,atts,subs)]
  in
  let rec iter xmls path =
    match xmls, path with
      ((x, `D _) as xml):: q, Path_cdata 0 -> (apply xml op) @ q
    | (x, `D s) :: q, Path_cdata n -> (x, `D s) :: iter q (Path_cdata (n-1))
    | ((x, `E (name,atts,subs) as xml) :: q, Path_node (name2, n, next)) when name = name2 ->
        if n = 0 then
          (match next with
             None -> (apply xml op) @ q
           | Some p ->  [x, `E (name, atts, iter subs p)] @ q
          )
        else
          xml :: iter q (Path_node (name2, n-1, next))
    | xml :: q, p ->
        xml :: iter q p
    | [], _ -> assert false
  in
  match iter [t] path with
    [t] -> t
  | _ -> assert false
;;


let patch_of_action (t1, patch) = function
| Replace (n2, i) ->
    let xmltree2 = n2.xml in
    let path = path_of_id t1 i in
    let op = PReplace xmltree2 in
    let t1 = patch_xmlnode t1 path op in
    (t1, (path, op) :: patch)
| InsertBefore (n2, i) ->
    let xmltree2 = n2.xml in
    let path = path_of_id t1 i in
    let op = PInsertBefore xmltree2 in
    let t1 = patch_xmlnode t1 path op in
    (t1, (path, op) :: patch)
| InsertAfter (n2, i) ->
    let xmltree2 = n2.xml in
    let path = path_of_id t1 i in
    let op = PInsertAfter xmltree2 in
    let t1 = patch_xmlnode t1 path op in
    (t1, (path, op) :: patch)
| DeleteTree i ->
    let path = path_of_id t1 i.number in
    let op = PDeleteTree in
    let t1 = patch_xmlnode t1 path op in
    (t1, (path, op) :: patch)
| Edit (n1, n2) ->
    let path = path_of_id t1 n1.number in
    let op =
      if n1.can_update then
        match n1.xml, n2.xml with
          _ , `D s2 -> PUpdateCData s2
        | `E (_,_,_), `E (name,atts,_) -> PUpdateNode (name, atts)
        | `D _, `E (name,atts,subs) -> PUpdateNode (name, atts)
      else
        PReplace n2.xml
    in
    let t1 = patch_xmlnode t1 path op in
    (t1, (path, op) :: patch)
;;

let rec xmltree_of_xmlnode = function
  (_, `D s) -> `D s
| (_, `E (tag,atts,subs)) -> `E (tag, atts, List.map xmltree_of_xmlnode subs)
;;

let mk_replace =
  let rec iter acc t1 = function
  | [] -> List.rev acc
  | InsertBefore(n2,0) :: q
  | InsertAfter(n2,0) :: q -> iter (Replace(n2,1)::acc) t1 q

  | (InsertBefore(n2,i) as h) :: DeleteTree j :: q
  | (InsertAfter(n2,i) as h) :: DeleteTree j :: q ->
      if j.number = i then
        begin
          if n2 = t1.(i) then (* no need to replace a tree by the same one *)
            iter acc t1 q
          else
            iter (Replace (n2, i) :: acc) t1 q
        end
      else
        iter (h :: acc) t1 (DeleteTree j :: q)
  | h :: q ->
      iter (h :: acc) t1 q
  in
  iter []
;;

(*c==v=[File.file_of_string]=1.1====*)
let file_of_string ~file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc
(*/c==v=[File.file_of_string]=1.1====*)

let patch_of_actions t1 t2 l =
  let actions = mk_replace t1 l in
  let t1 = xmlnode_of_t t1 in
  let (t1, l) = List.fold_left patch_of_action (t1, []) actions in

  let t1 = xmltree_of_xmlnode t1 in
  let t2 = xmltree_of_xmlnode (xmlnode_of_t t2) in
  file_of_string ~file: "/tmp/xml1.xml" (string_of_xml t1);
  file_of_string ~file: "/tmp/xml2.xml" (string_of_xml t2);

  List.rev l
;;

let rec string_of_path = function
  Path_cdata n -> "CData("^(string_of_int n)^")"
| Path_node (name, n, next) ->
    let s = (string_of_name name)^"("^(string_of_int n)^")" in
    match next with
      None -> s
    | Some p -> s^"/"^(string_of_path p)
;;

let string_of_patch_operation (path, op) =
  match op with
  | PReplace xmltree ->
      "REPLACE("^(string_of_path path)^", "^(string_of_xml xmltree)^")"
  | PInsertBefore xmltree ->
      "INSERT_BEFORE("^(string_of_path path)^", "^(string_of_xml xmltree)^")"
  | PInsertAfter xmltree ->
      "INSERT_AFTER("^(string_of_path path)^", "^(string_of_xml xmltree)^")"
  | PDeleteTree ->
      "DELETE_TREE("^(string_of_path path)^")"
  | PUpdateCData s ->
      Printf.sprintf "UPDATE_CDATA(%s, %S)" (string_of_path path) s
  | PUpdateNode (name, atts) ->
      Printf.sprintf "UPDATE_NODE(%s, %S, _)" (string_of_path path) (string_of_name name)
;;

let string_of_patch l =
  String.concat "\n" (List.map string_of_patch_operation l)
;;

let default_costs = {
    cost_insert = (fun size _ -> size) ;
    cost_delete = (fun size _ -> max (size / 4) 1) ;
    cost_edit = (fun (label1, _) (label2, _) ->
       if label1 = label2 then 0 else 1) ;
  }
;;

let diff ?(fcost=default_costs) ?cut xml1 xml2 =
  let t1 = t_of_xml ?cut xml1 in
  let t2 = t_of_xml ?cut xml2 in

  file_of_string ~file: "/tmp/t1.dot" (dot_of_t t1);
  file_of_string ~file: "/tmp/t2.dot" (dot_of_t t2);

  let cost, actions = compute fcost t1 t2 in
  prerr_endline ("actions=\n  "^(String.concat "\n  " (List.map string_of_action actions)));
  let patch = patch_of_actions t1 t2 actions in
  (cost, patch)
;;

