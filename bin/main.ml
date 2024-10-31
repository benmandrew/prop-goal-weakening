open Why3
open Craigar

let make_attribute name = Ident.create_attribute ("model_trace:" ^ name)

let make_prop_var s =
  let loc = Loc.user_position "" 0 0 0 0 in
  let attrs = Ident.Sattr.singleton (make_attribute s) in
  Term.create_psymbol (Ident.id_fresh ~loc ~attrs s) []

type var = { v : Term.lsymbol; a : Term.term }

let make_var s =
  let v = make_prop_var s in
  { v; a = Term.ps_app v [] }

type fmla = { vars : Term.lsymbol list; f : Term.term }

let fmla_to_task task_name fmla =
  let goal_id = Decl.create_prsymbol (Ident.id_fresh task_name) in
  let task = Task.add_meta None Driver.meta_get_counterexmp [] in
  let task =
    List.fold_left (fun task v -> Task.add_param_decl task v) task fmla.vars
  in
  Task.add_prop_decl task Decl.Pgoal goal_id fmla.f

let () =
  let a = make_var "A" in
  let b = make_var "B" in
  (* let c = make_var "C" in *)
  let f = Term.t_implies b.a (Term.t_and a.a b.a) in
  let fmla =
    let loc = Loc.user_position "" 0 0 0 0 in
    let attrs = Ident.Sattr.singleton Ity.annot_attr in
    { vars = [ a.v; b.v ]; f = Term.t_attr_set ~loc attrs f }
  in

  let task = fmla_to_task "g" fmla in
  Format.printf "@[%a@]@." Pretty.print_task task;

  match Solver.get_model task with
  | Some m ->
      Format.printf "%t@." (fun fmt ->
          Json_base.print_json fmt (Model_parser.json_model m))
  | None -> Format.printf "Model unavailable@."
