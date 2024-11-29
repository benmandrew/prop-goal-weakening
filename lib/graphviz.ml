open Lattice

let id = Format.sprintf "f%d"

let style ~f_greenred ~f_interp_chain fs f =
  if f_interp_chain f then "color=blueviolet"
  else if f_greenred f then
    let fs' = List.map (fun (_, x) -> x) fs in
    let has_pre_covers =
      List.exists (fun (_, x) -> f_greenred x && Func.covers fs' x f) fs
    in
    if has_pre_covers then "color=forestgreen" else "color=red"
  else "color=indianred3"

let nodes ~f_greenred ~f_interp_chain fs =
  ignore f_interp_chain;
  let fs = List.mapi (fun i x -> (i, x)) fs in
  let get_line (i, f) =
    let s = Func.to_string f in
    Format.sprintf "    %s [label=\"%s\", %s];\n" (id i) s
      (style ~f_greenred ~f_interp_chain fs f)
  in
  String.concat "" @@ List.map get_line fs

let edges fs =
  let fs' = List.mapi (fun i x -> (i, x)) fs in
  let get_edges_f i f =
    let covers =
      List.filter_map
        (fun (i, x) -> if Func.covers fs f x then Some (i, x) else None)
        fs'
    in
    List.map
      (fun (j, _) -> Format.sprintf "    %s -> %s;\n" (id i) (id j))
      covers
  in
  List.mapi get_edges_f fs |> List.concat |> String.concat ""

let to_string ~f_greenred ~f_interp_chain fs =
  Format.sprintf
    {|digraph B16 {
    rankdir=TB;
    node [shape=circle, style=filled, color=lightgray, fontname="Courier New"];
    
%s
%s}
|}
    (nodes ~f_greenred ~f_interp_chain fs)
    (edges fs)
