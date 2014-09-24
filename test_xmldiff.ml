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

let cut_tags = ref []

let diff file1 file2 =
  let xml1 = Xmldiff.xml_of_file file1 in
  let xml2 = Xmldiff.xml_of_file file2 in
  let cut (_,tag) _ _ = List.mem (String.lowercase tag) !cut_tags in
  let (patch, xml3) = Xmldiff.diff_with_final_tree ~cut xml1 xml2 in
  (patch, xml3 = xml2)

let options = [
  "-c", Arg.String (fun s -> cut_tags := s :: !cut_tags),
  "<tag> cut nodes with tag *:<tag>" ;
  ]

let usage = "Usage: "^Sys.argv.(0)^" [options] file1 file2";;

let main () =
  let files = ref [] in
  Arg.parse options (fun f -> files := f :: !files) usage;
  match List.rev !files with
  | [ file1 ; file2 ] ->
      let (patch, ok) = diff file1 file2 in
      print_endline (Xmldiff.string_of_patch patch);
      if not ok then
        failwith "Target tree and patched tree differ"
  | _ -> failwith usage
;;

let () = try main () with Failure msg -> prerr_endline msg ; exit 1;;