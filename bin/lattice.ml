open Why3
open Craigar
open Lattice

let () =
  (* Format.printf "n = %d\n" (List.length @@ Func.all); *)
  let f =
    Func.generate @@ Term.t_and_l
    @@ List.map (Problem.make_atom vs) [ "P1"; "P2"; "P3" ]
  in
  let f' =
    Func.generate @@ Term.t_or_l
    @@ List.map (Problem.make_atom vs) [ "P1"; "P2"; "P3" ]
  in
  let filter x = Func.implies f x && Func.implies x f' in
  let all = List.filter filter Func.all in

  (* Format.printf "%s" @@ Graphviz.to_string all; *)
  let ch = open_out "out/lattice.gv" in
  output_string ch @@ Graphviz.to_string all;
  close_out ch

(* Format.printf "Covers of\n%s\n\n" @@ Func.to_string f;
   let covers = List.filter (Func.covers all f) all in
   List.iter (fun a -> Format.printf "%s\n\n" @@ Func.to_string a) covers *)
(* Format.printf "n = %d\n"
   (List.length @@ List.filter (Func.covers Func.all f) Func.all) *)
