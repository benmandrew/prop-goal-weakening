open Why3
open Formula
module SMap = Map.Make (String)

module TSet = Set.Make (struct
  type t = Term.term

  let compare = Term.t_compare
end)

type problem = {
  assumption : Term.term;
  desirable : Term.term;
  critical : Term.term;
  interpolant : TSet.t;
}

let print p =
  Format.printf "@[Assumption:@.  %a@.@]" Pretty.print_term p.assumption;
  Format.printf "@[Desirable:@.  %a@.@]" Pretty.print_term p.desirable;
  Format.printf "@[Critical:@.  %a@.@]" Pretty.print_term p.critical;
  Format.printf "@[Interpolant:@.";
  TSet.iter (Format.printf "  %a@." Pretty.print_term) p.interpolant;
  Format.printf "@]@."

let make_vmap vs = List.map (fun n -> (n, Var.make n)) vs |> SMap.of_list
let make_atom vs s = Var.atom (SMap.find s vs)

(* let critical vs =
   Term.t_or_l
   @@ List.map (make_atom vs) [ "P1"; "P2"; "P3"; "P4" ] *)

(* let critical vs =
   let atoms = List.map (make_atom vs) [ "P1"; "P2"; "P3"; "P4" ] in
   let rec f l =
     match l with
     | [] -> []
     | hd :: tl -> List.map (fun x -> Term.t_and hd x) tl @ f tl
   in
   Term.t_or_l @@ f atoms *)

let critical vs = Term.t_or_l @@ List.map (make_atom vs) [ "P1"; "P2"; "P3" ]

(* let desirable vs =
   Term.t_and_l
   @@ List.map (make_atom vs) [ "P1"; "P2"; "P3"; "P4" ] *)

let desirable vs = Term.t_and_l @@ List.map (make_atom vs) [ "P1"; "P2"; "P3" ]

(* let assumption vs = Term.t_and_l @@ List.map (make_atom vs) [ "P1"; "P2"; "P3"; "P4"; "P5"; "P6" ] *)

let assumption vs =
  let p1 = make_atom vs "P1" in
  let p2 = make_atom vs "P2" in
  let p3 = make_atom vs "P3" in
  Term.(t_or (t_and_l [ p1; p2; p3 ]) (t_and p1 (t_not p2)))
(* Term.(t_and (t_or p1 p2) (t_or (t_not p1) (t_not p2))) *)

(* Term.t_or_l @@ List.map (make_atom vs) [ "P1"; "P2" ] *)

let init vs =
  let desirable = desirable vs in
  let assumption = assumption vs in
  {
    assumption;
    desirable;
    critical = critical vs;
    interpolant = TSet.singleton desirable;
  }

let get_fmla vs ~problem =
  let interpolant = Term.t_or_l @@ TSet.to_list problem.interpolant in
  let vars = List.map (fun p -> Var.name (snd p)) @@ SMap.to_list vs in
  let a_to_i = Term.t_implies problem.assumption interpolant in
  let d_to_i = Term.t_implies problem.desirable interpolant in
  let i_to_c = Term.t_implies interpolant problem.critical in
  make_fmla vars @@ Term.t_and_l [ a_to_i; d_to_i; i_to_c ]

(* let make_full vs ~critical ~desirable ~assumption ~interpolant =
   let interpolant = Term.t_or_l interpolant in
   let vars = List.map (fun p -> Var.name (snd p)) @@ SMap.to_list vs in
   let a_to_i = Term.t_implies assumption interpolant in
   let d_to_i = Term.(t_not @@ t_implies interpolant desirable) in
   let i_to_c = Term.(t_not @@ t_implies critical interpolant) in
   make_fmla vars @@ Term.t_and_l [ a_to_i; d_to_i; i_to_c ] *)

(* let make_full vs ~critical ~desirable ~assumption ~interpolant =
   let vars = List.map (fun p -> Var.name (snd p)) @@ SMap.to_list vs in
   let interpolant = Term.t_or_simp_l interpolant in
   let n_a_to_i = Term.(t_not_simp @@ t_implies_simp interpolant assumption) in
   let d_to_i = Term.t_implies_simp desirable interpolant in
   let i_to_c = Term.t_implies_simp interpolant critical in
   (* let d_to_i = Term.(t_not @@ t_implies interpolant desirable) in
   let i_to_c = Term.(t_not @@ t_implies critical interpolant) in *)
   ignore (d_to_i, i_to_c);
   (* make_fmla vars @@ Term.t_and_l [ n_a_to_i; d_to_i; i_to_c ] *)
   make_fmla vars @@ Term.t_and_l [ n_a_to_i ] *)

let add_cex problem cex =
  if TSet.mem cex problem.interpolant then
    failwith "Duplicate counterexample; UNSAT?";
  { problem with interpolant = TSet.add cex problem.interpolant }
