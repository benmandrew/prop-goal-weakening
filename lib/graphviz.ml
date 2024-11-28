open Lattice

let id = Format.sprintf "f%d"

let nodes fs =
  let get_line i f =
    let s = Func.to_string f in
    Format.sprintf "    %s [label=\"%s\"];\n" (id i) s
  in
  String.concat "" @@ List.mapi get_line fs

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

let to_string fs =
  Format.sprintf
    {|digraph B16 {
    fontname="Courier New"
    rankdir=TB;
    node [shape=circle, style=filled, color=lightgray];
    
%s
%s}
|}
    (nodes fs) (edges fs)
