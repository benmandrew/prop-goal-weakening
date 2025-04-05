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
  let printf = Format.printf in
  printf "@[Problem statement:@.@]";
  printf "@[  Assumption (system+environment):@.    %a@.@]" Pretty.print_term
    p.assumption;
  printf "@[  Desirable property:@.    %a@.@]" Pretty.print_term p.desirable;
  printf "@[  Critical property:@.    %a@.@]" Pretty.print_term p.critical;
  printf "@[  Initial interpolant property:@.";
  TSet.iter (printf "    %a@." Pretty.print_term) p.interpolant;
  printf "@]@."

let make_vmap vs = List.map (fun n -> (n, Var.make n)) vs |> SMap.of_list
let make_atom vs s = Var.atom (SMap.find s vs)

let critical vs =
  ignore vs;
  Term.t_true

let desirable vs =
  let high_wind = make_atom vs "HighWind" in
  let low_wind = make_atom vs "LowWind" in
  let can_land = make_atom vs "CanLand" in
  Term.(t_implies (t_or high_wind low_wind) can_land)

let assumption vs =
  let four_rotors = make_atom vs "FourRotors" in
  let three_rotors = make_atom vs "ThreeRotors" in
  let high_wind = make_atom vs "HighWind" in
  let low_wind = make_atom vs "LowWind" in
  let can_land = make_atom vs "CanLand" in
  let ideal =
    Term.(t_implies (t_and four_rotors (t_or high_wind low_wind)) can_land)
  in
  let degraded = Term.(t_implies (t_and three_rotors low_wind) can_land) in
  Term.t_and_l [ three_rotors; ideal; degraded ]

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

let add_cex problem cex =
  if TSet.mem cex problem.interpolant then
    failwith "Duplicate counterexample; UNSAT?";
  { problem with interpolant = TSet.add cex problem.interpolant }
