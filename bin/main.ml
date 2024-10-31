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

module SMap = Map.Make (String)

let make_vmap vs =
  List.fold_left (fun m v -> SMap.add v (make_var v) m) SMap.empty vs

type fmla = { vars : Term.lsymbol list; f : Term.term }

let fmla_to_task task_name fmla =
  let goal_id = Decl.create_prsymbol (Ident.id_fresh task_name) in
  let task = Task.add_meta None Driver.meta_get_counterexmp [] in
  let task =
    List.fold_left (fun task v -> Task.add_param_decl task v) task fmla.vars
  in
  Task.add_prop_decl task Decl.Pgoal goal_id fmla.f

let extract_var json =
  let open Json_base in
  let name = get_field json "lsymbol" |> fun j -> get_string_field j "name" in
  let value =
    get_field json "value" |> fun j ->
    get_field j "value_concrete_term" |> fun j -> get_bool_field j "val"
  in
  (name, value)

let extract_cex json =
  let open Json_base in
  get_list json
  |> List.map (fun j ->
         get_field j "model" |> get_list
         |> List.map (fun j ->
                get_field j "model_elements" |> get_list |> List.map extract_var))
  |> List.flatten |> List.flatten

let make_fmla vars term =
  let loc = Loc.user_position "" 0 0 0 0 in
  let attrs = Ident.Sattr.singleton Ity.annot_attr in
  { vars; f = Term.t_attr_set ~loc attrs term }

let critical vs =
  Term.t_or_l
  @@ List.map (fun s -> (SMap.find s vs).a) [ "P1"; "P2"; "P3"; "P4" ]

let desirable vs =
  Term.t_and_l
  @@ List.map (fun s -> (SMap.find s vs).a) [ "P1"; "P2"; "P3"; "P4" ]

let assumption vs =
  Term.t_and_l @@ List.map (fun s -> (SMap.find s vs).a) [ "P1"; "P2"; "P5" ]

let make_full vs ~critical ~desirable ~assumption ~interpolant =
  let vars = List.map (fun p -> (snd p).v) @@ SMap.to_list vs in
  let a_to_i = Term.t_implies assumption interpolant in
  let d_to_i = Term.t_implies desirable interpolant in
  let i_to_c = Term.t_implies interpolant critical in
  make_fmla vars @@ Term.t_and_l [ a_to_i; d_to_i; i_to_c ]

let term_from_cex vs model =
  Term.t_and_l
  @@ List.map
       (fun (n, v) ->
         let vt = SMap.find n vs in
         if v then vt.a else Term.t_not vt.a)
       model

let solve vs fmla =
  let task = fmla_to_task "g" fmla in
  (* Format.printf "@[%a@]@." Pretty.print_task task; *)
  let res = Solver.call task in
  match Solver.get_model res with
  | Some m ->
      Format.printf "Counterexample:@.";
      let cex = extract_cex @@ Model_parser.json_model m in
      List.iter (fun (s, v) -> Format.printf "%s = %b@." s v) cex;
      Some (term_from_cex vs cex)
  | None ->
      Format.printf "Model unavailable@.";
      None

let rec cegar_loop vs make_full ~interpolant =
  let fmla = make_full ~interpolant in
  match solve vs fmla with
  | None -> ()
  | Some cex ->
      let interpolant = Term.t_or interpolant cex in
      Format.printf "@[Interpolant:@.%a@]@." Pretty.print_term interpolant;
      cegar_loop vs make_full ~interpolant

let () =
  let vs = make_vmap [ "P1"; "P2"; "P3"; "P4"; "P5" ] in
  let critical = critical vs in
  let desirable = desirable vs in
  let assumption = assumption vs in
  let make_full = make_full vs ~critical ~desirable ~assumption in
  cegar_loop vs make_full ~interpolant:desirable
