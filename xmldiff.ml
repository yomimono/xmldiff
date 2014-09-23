(** *)

module Smap = Map.Make(String)
module Intmap = Map.Make
  (struct type t = int let compare (x:int) y = Pervasives.compare x y end)

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
let compare_label l1 l2 =
  match l1, l2 with
    Node s1, Node s2 -> String.compare s1 s2
  | Text s1, Text s2 -> String.compare s1 s2
  | Node _, Text _  -> -1
  | Text _, Node _ -> 1

module Lmap = Map.Make(struct type t = label let compare = compare_label end)

type node = {
  number : int ;
  children : int array ;
  mutable parent : int option ;
  xml : xmltree ;
  weight : float ;
  hash : string ;
  label : label ;
  rank : int ;
  is_cut : bool ;
  mutable matched : int option ;
  }

type doc = {
  height: int ;
  w0 : float ;
  nodes : node array ;
 }

(*c==v=[File.file_of_string]=1.1====*)
let file_of_string ~file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc
(*/c==v=[File.file_of_string]=1.1====*)

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
| `E (tag, _, _) -> Node (string_of_name tag)

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

let short_label = function
  `E (("",s2), _, _) -> "<"^s2^">"
| `E ((s1,s2), _, _) -> "<"^s1^":"^s2^">"
| `D s ->
    let len = String.length s in
    let s = Printf.sprintf "%S" (String.sub s 0 (min 10 len)) in
    let len = String.length s in
    String.sub s 1 (len - 2)

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
  let rec iter ?cut (n0, acc, acc_children, h) xml =
    let (label, subs, is_cut) =
      match xml with
      | `D _ -> (label_of_xml xml, [], false)
      | `E (tag, atts, l) ->
          match cut with
          | Some f when f tag atts l -> (Node (string_of_xml xml), [], true)
          | _ -> (label_of_xml xml, l, false)
    in
    let (n, acc, children, h_children) = List.fold_left (iter ?cut) (n0, acc, [], 0) subs in
    let children = List.rev children in
    List.iter (fun node -> node.parent <- Some n) children ;
    let hash = hash xml in
    let weight = weight xml children in
    let node =
      { number = n ;
        children = Array.of_list (List.map (fun node -> node.number) children) ;
        parent = None ;
        xml ; label ; hash ; weight ;
        rank = List.length acc_children ;
        is_cut ;
        matched = None ;
      }
    in
    (n+1, node :: acc, node :: acc_children, max h (h_children + 1))
  in
  fun ?cut xml ->
    let (_, l, _, h) = iter ?cut (0, [], [], 0) xml in
    let t = Array.of_list l in
    Array.sort (fun n1 n2 -> n1.number - n2.number) t;
    Array.iteri (fun i node ->
       (*
       prerr_endline (Printf.sprintf "i=%d, node.number=%d, parent=%s, xml=%s"
        i node.number
          (match node.parent with None -> "" | Some n -> string_of_int n)
          (short_label node.xml)
       );
       *)
       assert (i = node.number)
    ) t;
    { height = h;
      nodes =  t;
      w0 = t.(Array.length t - 1).weight ;
    }
;;


type operation =
  | Replace of node * int
  | Move of int * node * int * int
  | MoveRank of int * int
  | Insert of node * int * int (* Insert(node,i,rank) insert tree from t2 as nth child of i *)
  | Delete of node (* delete tree from t1 *)
  | Edit of node * node (* change label of node from t1 to label of node from t2 *)

type actions = operation list

type patch_path =
  Path_cdata of int
| Path_node of Xmlm.name * int * patch_path option

type position = [ `FirstChild | `After]
type patch_operation =
| PInsert of xmltree * position
| PDelete
| PUpdateCData of string
| PUpdateNode of Xmlm.name * string Nmap.t
| PReplace of xmltree
| PMove of patch_path * position

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

let dot_of_t t =
  let b = Buffer.create 256 in
  let p b = Printf.bprintf b in
  p b "digraph g {\nrankdir=TB;\nordering=out;\n";
  Array.iter
    (fun node ->
       p b "\"N%d\" [ label=\"%d: %s[%d]\", fontcolor=black ];\n"
         node.number node.number (short_label node.xml) node.rank;
       Array.iter (fun i -> p b "\"N%d\" -> \"N%d\";\n" node.number i) node.children ;
    )
    t.nodes;
  p b "}\n";
  Buffer.contents b

let dot_of_xmlnode t =
  let b = Buffer.create 256 in
  let p b = Printf.bprintf b in
  p b "digraph g {\nrankdir=TB;\nordering=out;\n";
  let string_of_id = function None -> "_" | Some n -> string_of_int n in
  let cpt = ref 0 in
  let rec iter parent rank (id, xml) =
    incr cpt;
    let n = !cpt in
    p b "\"N%d\" [ label=\"%s: %s[%d]\", fontcolor=black ];\n"
      n  (string_of_id id) (short_label xml) rank;
    match xml with
      `D _ -> ()
    | `E (_,_,subs) ->
        List.iteri (fun i child ->
           iter (Some n) i child ;
        )
          subs;
        match parent with
          None -> ()
        | Some parent ->  p b "\"N%d\" -> \"N%d\";\n" parent n
  in
  iter None 0 t ;
  p b "}\n";
  Buffer.contents b

let dot_of_matches t1 t2 =
  let b = Buffer.create 256 in
  let p b = Printf.bprintf b in
  p b "digraph g {\nrankdir=TB;\nordering=out;\n";
  p b "subgraph cluster_2 {\n";
  Array.iter
    (fun node ->
       p b "\"T%d\" [ label=\"%d: %s[%d]\", fontcolor=black ];\n"
         node.number node.number (short_label node.xml) node.rank;
       Array.iter (fun i -> p b "\"T%d\" -> \"T%d\";\n" node.number i) node.children ;
    )
    t2.nodes;
  p b "}\n";
  p b "subgraph cluster_1 {\n";
  Array.iter
    (fun node ->
       p b "\"S%d\" [ label=\"%d: %s[%d]\", fontcolor=black ];\n"
         node.number node.number (short_label node.xml) node.rank;
       Array.iter (fun i -> p b "\"S%d\" -> \"S%d\";\n" node.number i) node.children ;
       match node.matched with
         None -> ()
       | Some j -> p b "S%d -> T%d [style=\"dashed\"];\n" node.number j
    )
    t1.nodes;
  p b "}\n";
  p b "}\n";
  Buffer.contents b

let string_of_action = function
| Replace (n2, i) -> Printf.sprintf "Replace (%d, %d): %s" n2.number i (string_of_xml ~cut:true n2.xml)
| Move (i, n2, new_parent, rank) -> Printf.sprintf "Move(%d,%d,%d)" i new_parent rank
| MoveRank (i, rank) -> Printf.sprintf "MoveRank(%d,%d)" i rank
| Insert (n2, i, rank) -> Printf.sprintf "Insert (%d, %d, %d): %s" n2.number i rank (string_of_xml ~cut:true n2.xml)
| Delete n1 -> Printf.sprintf "Delete(%d): %s" n1.number (string_of_xml ~cut: true n1.xml)
| Edit (n1, n2) -> Printf.sprintf "Edit(%d,%d): %s -> %s" n1.number n2.number
  (string_of_xml ~cut: true n1.xml) (string_of_xml ~cut: true n2.xml)
;;

let have_matching_parents nodes1 n1 n2 =
  match n1.parent, n2.parent with
  | None, None -> true
  | None, _
  | _, None -> false
  | Some p1, Some p2 -> nodes1.(p1).matched = Some p2

let matching_parent nodes n =
  match n.parent with
    None -> None
  | Some p -> nodes.(p).matched

let add_edit_action acc n1 n2 =
  match n1.xml, n2.xml with
    `E _, `D _
  | `D _, `E _ -> assert false
  | `D s1, `D s2 -> Edit (n1, n2) :: acc
  | `E (tag1, atts1, _), `E (tag2, atts2, _) ->
      match n1.is_cut, n2.is_cut with
        true, true -> Replace (n2, n1.number) :: acc
      | false, false ->
          begin
            if tag1 = tag2 && Nmap.equal (=) atts1 atts2 then
              acc
            else
              Edit (n1, n2) :: acc
          end
      | _ -> assert false

let make_actions t1 t2 =
  let nodes1 = t1.nodes in
  let nodes2 = t2.nodes in
  let rec f (acc, rank) i =
    let n1 = nodes1.(i) in
    match n1.matched with
      None -> ((Delete n1) :: acc, rank + 1)
    | Some j ->
        let n2 = nodes2.(j) in
        let matching_parents = have_matching_parents nodes1 n1 n2 in
        let acc =
          if matching_parents then
            if n1.rank = n2.rank then
              acc
            else
              MoveRank(n1.number, n2.rank) :: acc
          else
            (
             let new_parent =
               match matching_parent nodes2 n2 with
                 None ->
                   prerr_endline (Printf.sprintf "make_actions, i = %d" i);
                   assert false
               | Some i -> i
             in
             (Move(n1.number, n2, new_parent, rank)) :: acc
            )
        in
        let acc =
          if n1.hash = n2.hash then
            acc
          else
            (
             let acc = add_edit_action acc n1 n2 in
             let (acc, _) = Array.fold_left f (acc, 0) n1.children in
             acc
            )
        in
        (acc, rank + 1)
  in
  let (actions, _) = f ([], 0) (Array.length nodes1 - 1) in
  (* note: we should not have a node not matched in t2 with matched descendants *)
  let rec g (acc, rank) j =
    let n2 = nodes2.(j) in
    match n2.matched with
      None ->
        let new_parent =
          match matching_parent nodes2 n2 with
            None ->
              prerr_endline (Printf.sprintf "no matching parent for t2.(%d)" n2.number);
              assert false
          | Some i -> i
        in
        ((Insert (n2, new_parent, rank)) :: acc, rank + 1)
    | Some _ ->
        let (acc, _) = Array.fold_left g (acc, 0) n2.children in
        (acc, rank + 1)
  in
  let (actions, _) = g (actions, 0) (Array.length nodes2 - 1) in
  actions
;;

let sort_actions =
  let pred a1 a2 =
    match a1, a2 with
    | Delete _, Delete _ -> 0
    | Delete _, _ -> -1
    | _, Delete _ -> 1
    | Edit _, Edit _ -> 0
    | Edit _, _ -> 1
    | _, Edit _ -> -1
    | Replace _, Replace _ -> 0
    | Replace _, _ -> 1
    | _, Replace _ -> -1
    | MoveRank(_,rank1), MoveRank(_,rank2)
    | MoveRank(_,rank1), Move(_,_,_,rank2)
    | Move(_,_,_,rank1), MoveRank(_,rank2)
    | MoveRank(_,rank1), Insert(_,_,rank2)
    | Insert(_,_,rank1), MoveRank(_,rank2)
    | Move (_,_,_,rank1), Move (_,_,_,rank2)
    | Move (_,_,_,rank1), Insert (_,_,rank2)
    | Insert (_,_,rank1), Move (_, _,_, rank2)
    | Insert (_,_,rank1), Insert (_,_,rank2) -> rank1 - rank2
  in
  List.sort pred

let build_hash_map =
  let add node map =
    let l =
      try Smap.find node.hash map
      with Not_found -> []
    in
    Smap.add node.hash (node.number :: l) map

  in
  fun t -> Array.fold_right add t.nodes Smap.empty
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

let rec match_nodes ?(with_subs=false) t1 t2 i j =
  (*prerr_endline (Printf.sprintf "matching %d -> %d" i j);*)
  t1.nodes.(i).matched <- Some j;
  t2.nodes.(j).matched <- Some i;
  if with_subs then
    begin
      let ch_i = t1.nodes.(i).children in
      let ch_j = t2.nodes.(j).children in
      for x = 0 to Array.length ch_i - 1 do
        match_nodes ~with_subs: true t1 t2 ch_i.(x) ch_j.(x)
      done
    end

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

let rec min_list p v l =
  let rec iter acc = function
    [] -> acc
  | h :: q -> if p h < acc then iter h q else iter acc q
  in
  iter v l

let rec best_candidate ?(level=1) t1 t2 j cands =
  prerr_endline ("best_candidates "^(String.concat ", " (List.map string_of_int cands)));
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
  let candidates = candidates hash_t1 t2 j in
  let pred i = t1.nodes.(i).matched = None in
  match List.filter pred candidates with
    [] -> None
  | [i] -> Some i
  | l -> best_candidate t1 t2 j l

let (+=) map (k, v) =
  let x = try Intmap.find k map with Not_found -> 0. in
  Intmap.add k (v +. x) map

let match_uniquely_labeled =
  let map_of_t nodes t f map =
    Array.fold_left
      (fun map i ->
         let node = nodes.(i) in
         match node.matched with
           Some _ -> map
         | None ->
             let label = node.label in
             let x = try Lmap.find label map with Not_found -> ([], []) in
             let x = f i x in
             Lmap.add label x map
      )
      map t
  in
  fun t1 t2 li lj ->
    let map = map_of_t t1.nodes li (fun i (l1,l2) -> (i :: l1, l2)) Lmap.empty in
    let map = map_of_t t2.nodes lj (fun j (l1,l2) -> (l1, j :: l2)) map in
    Lmap.iter
      (fun _ -> function
         | [ i ], [ j ] -> match_nodes t1 t2 i j
         | _ -> ())
      map

let match_uniquely_labeled_children =
  let do_match t1 t2 j =
    let nj = t2.nodes.(j) in
    match nj.matched with
      None -> ()
    | Some i ->
        let children_i = t1.nodes.(i).children in
        let children_j = nj.children in
        match_uniquely_labeled t1 t2 children_i children_j
  in
  fun t1 t2 ->
    for j = Array.length t2.nodes -1 downto 0 do
      do_match t1 t2 j
    done

let run_phase4 t1 t2 =
  let f j node =
    match node.matched with
      Some _ -> ()
    | None ->
        let parents = Array.fold_left
          (fun acc jc ->
             match t2.nodes.(jc).matched with
               None -> acc
             | Some i ->
                 prerr_endline (Printf.sprintf "%d has a child %d matched to %d" j jc i);
                 match t1.nodes.(i).parent with
                 | Some p when t1.nodes.(p).matched = None ->
                     prerr_endline (Printf.sprintf "%d has a non-matched parent %d" i p);
                     acc += (p, t1.nodes.(i).weight)
                 | Some p ->
                     prerr_endline (Printf.sprintf "%d has a parent %d already matched" i p);
                     acc
                 | None -> acc
          )
            Intmap.empty node.children
        in
        let (parent, _) = Intmap.fold
          (fun p w ((acc_parent, acc_w) as acc) ->
             if w > acc_w then (p, w) else acc)
            parents (-1, -1.0)
        in
        if parent >= 0 then match_nodes t1 t2 j parent
  in
  Array.iteri f t2.nodes;
  match_uniquely_labeled_children t1 t2

let order_by_weight n1 n2 =
  match Pervasives.compare n2.weight n1.weight with
    0 -> Pervasives.compare n1.rank n2.rank
  | n -> n

let compute t1 t2 =
  let weight_queue = Queue.create () in
  let root2 = Array.length t2.nodes - 1 in
  (* make roots match orelse we'll have problems *)
  match_nodes t1 t2 (Array.length t1.nodes - 1) root2 ;
  let queue_nodes children =
    let t = Array.map (Array.get t2.nodes) children in
    Array.sort order_by_weight t;
    Array.iteri (fun _ n ->
       (*prerr_endline (Printf.sprintf "queuing %d" n.number);*)
       Queue.add n.number weight_queue)
      t
  in
  queue_nodes t2.nodes.(root2).children ;
  let hash_t1 = build_hash_map t1 in
  while not (Queue.is_empty weight_queue) do
    let j = Queue.pop weight_queue in
    (*prerr_endline (Printf.sprintf "trying to match %d" j);*)
    match t2.nodes.(j).matched with
       Some _ -> ()
    | None ->
        match match_candidate hash_t1 t1 t2 j with
          Some i ->
            match_nodes ~with_subs: true t1 t2 i j;
            match_ancestors t1 t2 i j
        | None ->
            queue_nodes t2.nodes.(j).children
  done;

  run_phase4 t1 t2 ;
  file_of_string ~file:"/tmp/matches.dot" (dot_of_matches t1 t2);
  sort_actions (make_actions t1 t2)

type cur_path = N of Xmlm.name | CData
module Cur_path = Map.Make (struct type t = cur_path let compare = Pervasives.compare end)
let cur_path_get cp map =
  try Cur_path.find cp map
  with Not_found -> 0

let cur_path_inc cp map =
  let n = cur_path_get cp map in
  Cur_path.add cp (n+1) map

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

let path_of_id =
  let cp_of_xml = function
    `D s -> CData
  | `E (name,_,_) -> N name
  in
  let rec forward path cur_path n = function
  | [] -> failwith (Printf.sprintf "Invalid rank: %d element missing" n)
  | (_, xml) :: _ when n = 0 ->
      let cp = cp_of_xml xml in
      (cp, cur_path_get cp cur_path) :: path
  | (_, xml) :: q ->
      let cur_path =
        let cp = cp_of_xml xml in
        cur_path_inc cp cur_path
      in
      forward path cur_path (n-1) q
  in
  let rec iter ~rank i path cur_path = function
  | (Some j, xml) when i = j ->
      begin
        let cp = cp_of_xml xml in
        let path = (cp, cur_path_get cp cur_path) :: path in
        let (path, pos) =
          match rank with
            None -> (path, `After)
          | Some 0 -> (path, `FirstChild)
          | Some n ->
              match xml with
                `D _ -> assert false
              | `E (_,_,subs) -> (forward path Cur_path.empty (n-1) subs, `After)
        in
        (patch_path_of_cur_path_list (List.rev path), pos)
      end
  | (Some j, _) when j < i -> raise Not_found
  | (_, `D _) -> raise Not_found
  | (_, `E (name, atts, subs)) ->
      (* None or Some j with j > i, let's go down after
         adding cur_path to path
         *)
      let cpt = cur_path_get (N name) cur_path in
      let path = (N name, cpt) :: path in
      iter_list ~rank i path Cur_path.empty subs

  and iter_list ~rank i path cur_path = function
    [] -> raise Not_found
  | h :: q ->
    try iter ~rank i path cur_path h
    with Not_found ->
      let cur_path =
        let cp = cp_of_xml (snd h) in
        cur_path_inc cp cur_path
      in
      iter_list ~rank i path cur_path q
  in
  fun xmlnode ?rank i ->
    try iter ~rank i [] Cur_path.empty xmlnode
    with Not_found ->
      let msg = "Id "^(string_of_int i)^" not found" in
      failwith msg
;;

let rec xmlnode_of_xmltree = function
  `D s -> (None, `D s)
| `E (name,atts,subs) ->
    (None, `E (name,atts, List.map xmlnode_of_xmltree subs))
;;


let remove_xmlnode t path =
  let rec iter xmls path =
    match xmls, path with
      ((x, `D _) as xml):: q, Path_cdata 0 -> (xml, q)
    | (x, `D s) :: q, Path_cdata n ->
       let (removed, xmls) = iter q (Path_cdata (n-1)) in
       (removed, (x, `D s) :: xmls)
    | ((x, `E (name,atts,subs) as xml) :: q, Path_node (name2, n, next)) when name = name2 ->
        if n = 0 then
          (match next with
             None -> (xml, q)
           | Some p ->
               let (removed, xmls) = iter subs p in
               (removed, [x, `E (name, atts, xmls)] @ q)
          )
        else
          (
           let (removed, xmls) = iter q (Path_node (name2, n-1, next)) in
           (removed, xml :: xmls)
          )
    | xml :: q, p ->
       let (removed, xmls) = iter q p in
       (removed, xml :: xmls)
    | [], _ -> assert false
  in
  match iter [t] path with
    removed, [t] -> (removed, t)
  | _ -> assert false

let insert_xmlnode t node path pos =
  let rec iter xmls path =
    match xmls, path with
      ((x, `D _) as xml):: q, Path_cdata 0 ->
        begin
          match pos with
            `FirstChild -> assert false
          | `After -> xml :: node :: q
        end
    | (x, `D s) :: q, Path_cdata n ->
        (x, `D s) :: iter q (Path_cdata (n-1))
    | ((x, `E (name,atts,subs) as xml) :: q, Path_node (name2, n, next)) when name = name2 ->
        if n = 0 then
          (match next with
             None ->
               begin
                 match pos with
                   `FirstChild -> (x, `E(name,atts,node::subs)) :: q
                 | `After -> xml :: node :: q
               end
           | Some p -> [x, `E (name, atts, iter subs p)] @ q
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

let rec patch_xmlnode t path op =
  match op with
    PMove (newpath, pos) ->
     let removed, t = remove_xmlnode t path in
     insert_xmlnode t removed newpath pos
  | _ ->
      let apply xml op =
        match xml, op with
        | _, PReplace tree -> [xmlnode_of_xmltree tree]
        | _, PInsert (tree, `After) -> [ xml ; xmlnode_of_xmltree tree ]
        | (_, `D _), PInsert (_, `FirstChild) -> assert false
        | (x, `E (tag,atts,subs)), PInsert (tree, `FirstChild) ->
            [ (x, `E (tag, atts, xmlnode_of_xmltree tree :: subs)) ]
        | _, PDelete -> []
        | (x, _), PUpdateCData s -> [(x, `D s)]
        | (x, `D _), PUpdateNode (name, atts) -> [x, `E (name,atts,[])]
        | (x, `E (_,_,subs)), PUpdateNode (name, atts) -> [x, `E (name,atts,subs)]
        | (_,_), PMove (_, _) -> assert false
      in
      let rec iter xmls path =
        match xmls, path with
          ((x, `D _) as xml):: q, Path_cdata 0 -> (apply xml op) @ q
        | (x, `D s) :: q, Path_cdata n ->
            (x, `D s) :: iter q (Path_cdata (n-1))
        | ((x, `E (name,atts,subs) as xml) :: q, Path_node (name2, n, next)) when name = name2 ->
            if n = 0 then
              (match next with
                 None -> (apply xml op) @ q
               | Some p -> [x, `E (name, atts, iter subs p)] @ q
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

let cpt = ref 0;;
let patch_of_action (t1, patch) = function
| Replace (n2, i) ->
    let xmltree2 = n2.xml in
    let (path, _) = path_of_id t1 i in
    let op = PReplace xmltree2 in
    let t1 = patch_xmlnode t1 path op in
    (t1, (path, op) :: patch)
| Move (i, _, new_parent, rank) ->
    let (path, _) = path_of_id t1 i in
    let (new_path, pos) = path_of_id t1 ~rank new_parent in
    let op = PMove (new_path, pos) in
    let t1 = patch_xmlnode t1 path op in
    (t1, (path, op) :: patch)
| MoveRank (i, rank) ->
    let (path, _) = path_of_id t1 i in
    let (new_path, pos) = path_of_id t1 ~rank i in
    let op = PMove (new_path, pos) in
    let t1 = patch_xmlnode t1 path op in
    (t1, (path, op) :: patch)
| Insert (n2, i, rank) ->
    let xmltree2 = n2.xml in
    let (path, pos) = path_of_id t1 ~rank i in
    let op = PInsert (xmltree2, pos) in
    let t1 = patch_xmlnode t1 path op in
    incr cpt;
    file_of_string ~file: (Printf.sprintf "/tmp/insert%02d.dot" !cpt) (dot_of_xmlnode t1);
    (t1, (path, op) :: patch)
| Delete i ->
    let (path,_) = path_of_id t1 i.number in
    let op = PDelete in
    let t1 = patch_xmlnode t1 path op in
    (t1, (path, op) :: patch)
| Edit (n1, n2) ->
    let (path,_) = path_of_id t1 n1.number in
    let op =
      match n1.xml, n2.xml with
        _ , `D s2 -> PUpdateCData s2
      | `E (_,_,_), `E (name,atts,_) -> PUpdateNode (name, atts)
      | `D _, `E (name,atts,subs) -> PUpdateNode (name, atts)
    in
    let t1 = patch_xmlnode t1 path op in
    (t1, (path, op) :: patch)
;;

let rec xmltree_of_xmlnode = function
  (_, `D s) -> `D s
| (_, `E (tag,atts,subs)) -> `E (tag, atts, List.map xmltree_of_xmlnode subs)
;;


let patch_of_actions t1 t2 actions =
  let nodes1 = xmlnode_of_t t1.nodes in
  file_of_string ~file: "/tmp/before_patch.dot" (dot_of_xmlnode nodes1);
  let (nodes1, l) = List.fold_left patch_of_action (nodes1, []) actions in
  file_of_string ~file: "/tmp/result.dot" (dot_of_xmlnode nodes1);

  let t1 = xmltree_of_xmlnode nodes1 in
  let t2 = xmltree_of_xmlnode (xmlnode_of_t t2.nodes) in
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
let string_of_position = function
  `FirstChild -> "FirstChild"
| `After -> "After"

let string_of_patch_operation (path, op) =
  match op with
  | PReplace xmltree ->
      "REPLACE("^(string_of_path path)^", "^(string_of_xml xmltree)^")"
  | PInsert (xmltree, pos) ->
      "INSERT("^(string_of_path path)^", "^(string_of_xml xmltree)^", "^(string_of_position pos)^")"
  | PDelete ->
      "DELETE("^(string_of_path path)^")"
  | PUpdateCData s ->
      Printf.sprintf "UPDATE_CDATA(%s, %S)" (string_of_path path) s
  | PUpdateNode (name, atts) ->
      Printf.sprintf "UPDATE_NODE(%s, %S, _)" (string_of_path path) (string_of_name name)
  | PMove (newpath, pos) ->
      Printf.sprintf "MOVE(%s, %s, %s)" (string_of_path path) (string_of_path newpath) (string_of_position pos)
;;

let string_of_patch l =
  String.concat "\n" (List.map string_of_patch_operation l)
;;


let diff ?cut xml1 xml2 =
  let t1 = t_of_xml ?cut xml1 in
  let t2 = t_of_xml ?cut xml2 in

  file_of_string ~file: "/tmp/t1.dot" (dot_of_t t1);
  file_of_string ~file: "/tmp/t2.dot" (dot_of_t t2);

  let actions = compute t1 t2 in
  prerr_endline ("actions=\n  "^(String.concat "\n  " (List.map string_of_action actions)));
  patch_of_actions t1 t2 actions
;;

