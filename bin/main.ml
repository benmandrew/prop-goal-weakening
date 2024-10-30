open Why3
open Craigar

let make_prop_var s = Term.create_psymbol (Ident.id_fresh s) []

let () =
  let v_a = make_prop_var "A" in
  let v_b = make_prop_var "B" in
  let atom_a = Term.ps_app v_a [] in
  let atom_b = Term.ps_app v_b [] in
  let fmla = Term.t_implies (Term.t_and atom_a atom_b) atom_a in

  let goal_id = Decl.create_prsymbol (Ident.id_fresh "goal") in
  let task = Task.add_param_decl None v_a in
  let task = Task.add_param_decl task v_b in
  let task = Task.add_prop_decl task Decl.Pgoal goal_id fmla in
  Format.printf "@[%a@]@." Pretty.print_task task;

  Format.printf "@[%a@]@." (Call_provers.print_prover_result ?json:None)
  @@ Solver.call task
