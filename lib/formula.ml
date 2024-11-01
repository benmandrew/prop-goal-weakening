open Why3

let make_attribute name = Ident.create_attribute ("model_trace:" ^ name)

let make_prop_var s =
  let loc = Loc.user_position "" 0 0 0 0 in
  let attrs = Ident.Sattr.singleton (make_attribute s) in
  Term.create_psymbol (Ident.id_fresh ~loc ~attrs s) []

module Var = struct
  type t = { n : Term.lsymbol; a : Term.term }

  let make s =
    let n = make_prop_var s in
    { n; a = Term.ps_app n [] }

  let atom t = t.a
  let name t = t.n
end

type fmla = { vars : Term.lsymbol list; f : Term.term }

let fmla_to_task task_name fmla =
  let goal_id = Decl.create_prsymbol (Ident.id_fresh task_name) in
  let task = Task.add_meta None Driver.meta_get_counterexmp [] in
  let task =
    List.fold_left (fun task v -> Task.add_param_decl task v) task fmla.vars
  in
  Task.add_prop_decl task Decl.Pgoal goal_id fmla.f

let make_fmla vars term =
  let loc = Loc.user_position "" 0 0 0 0 in
  let attrs = Ident.Sattr.singleton Ity.annot_attr in
  { vars; f = Term.t_attr_set ~loc attrs term }
