open Why3
open Craigar
open Formula

let term_from_cex vs model =
  Term.t_and_l
  @@ List.map
       (fun (n, v) ->
         let vt = Var.atom (Problem.SMap.find n vs) in
         if v then vt else Term.t_not vt)
       model

let match_result i vs res =
  match (res.Call_provers.pr_answer, Solver.get_model res) with
  | Call_provers.Unknown _, Some m ->
      let cex = Model.extract_cex m in
      Format.printf "Counterexample %d:@.  " i;
      List.iter (fun (n, v) -> Format.printf "%s=%d " n (Bool.to_int v)) cex;
      Format.printf "@.@.";
      Some (term_from_cex vs cex)
  | Unknown _, None ->
      Format.printf "Model unavailable; invalid?@.";
      None
  | Valid, _ ->
      Format.printf "Valid@.";
      None
  | _, _ -> None

let solve i vs fmla =
  let task = fmla_to_task "g" fmla in
  let res = Solver.call task in
  match_result i vs res

let rec cegar_loop i vs ~problem =
  Problem.print problem;
  let fmla = Problem.get_fmla vs ~problem in
  match solve i vs fmla with
  | None ->
      Format.printf "@[Interpolant:@.  %a@]@.@." Pretty.print_term
        (Term.t_or_l @@ Problem.TSet.to_list problem.Problem.interpolant)
  | Some cex ->
      let problem = Problem.add_cex problem cex in
      cegar_loop (i + 1) vs ~problem

let () =
  let vs =
    Problem.make_vmap @@ List.init 10 (fun i -> "P" ^ string_of_int (i + 1))
  in
  let problem = Problem.init vs in
  cegar_loop 0 vs ~problem
