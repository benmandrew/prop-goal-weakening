open Why3
open Weaken
open Lattice

let a = Problem.make_atom vs
let desirable = Term.t_and_l @@ List.map a [ "P1"; "P2"; "P3" ]
let critical = Term.t_or_l @@ List.map a [ "P1"; "P2"; "P3" ]
let assumption = Term.t_and_l @@ List.map a [ "P1"; "P2" ]

let () =
  let p1 = a "P1" in
  let p2 = a "P2" in
  let p3 = a "P3" in
  let open Term in
  let desirable = Func.generate desirable in
  let critical = Func.generate critical in
  let c = Func.generate assumption in

  let interpolant_chain =
    let counterexamples =
      List.map Func.generate
        [
          t_and_l [ t_not p1; p2 ];
          t_and_l [ p1; t_not p2 ];
          t_and_l [ p1; p2; t_not p3 ];
          (* t_and_l [ t_not p1; p2 ];
             t_and_l [ p1; t_not p2 ]; *)
          (* t_and_l [ p1; t_not p2 ];
             t_and_l [ p1; p2; t_not p3 ]; *)
          (* t_and_l [ p1; t_not p2 ]; *)
        ]
    in
    List.fold_left
      (fun acc c ->
        match acc with
        | [] -> failwith "Bruh"
        | hd :: _ -> Func.union c hd :: acc)
      [ desirable ] counterexamples
  in
  List.iter
    (fun f -> Format.printf "%s\n\n" @@ Func.to_string f)
    interpolant_chain;

  let f_interp_chain x = List.exists (Func.equal x) interpolant_chain in
  let f_greenred x = Func.implies c x in

  (* let filter x = Func.implies f x && Func.implies x f' in
     let all = List.filter filter Func.all in *)

  (* let all = Func.all in *)
  let all = Func.interval desirable critical in

  (* Format.printf "%s" @@ Graphviz.to_string all; *)
  let ch = open_out "out/lattice.gv" in
  output_string ch @@ Graphviz.to_string ~f_greenred ~f_interp_chain all;
  close_out ch

(* Format.printf "Covers of\n%s\n\n" @@ Func.to_string f;
   let covers = List.filter (Func.covers all f) all in
   List.iter (fun a -> Format.printf "%s\n\n" @@ Func.to_string a) covers *)
(* Format.printf "n = %d\n"
   (List.length @@ List.filter (Func.covers Func.all f) Func.all) *)
