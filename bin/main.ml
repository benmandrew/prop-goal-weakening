open Why3
open Craigar
open Formula
module SMap = Map.Make (String)

let make_vmap vs = List.map (fun n -> (n, Var.make n)) vs |> SMap.of_list

let critical vs =
  Term.t_or_l
  @@ List.map (fun s -> Var.atom (SMap.find s vs)) [ "P1"; "P2"; "P3"; "P4" ]

let desirable vs =
  Term.t_and_l
  @@ List.map (fun s -> Var.atom (SMap.find s vs)) [ "P1"; "P2"; "P3"; "P4" ]

let assumption vs =
  Term.t_and_l
  @@ List.map (fun s -> Var.atom (SMap.find s vs)) [ "P1"; "P2"; "P5" ]

let make_full vs ~critical ~desirable ~assumption ~interpolant =
  let vars = List.map (fun p -> Var.name (snd p)) @@ SMap.to_list vs in
  let a_to_i = Term.t_implies assumption interpolant in
  let d_to_i = Term.t_implies desirable interpolant in
  let i_to_c = Term.t_implies interpolant critical in
  make_fmla vars @@ Term.t_and_l [ a_to_i; d_to_i; i_to_c ]

let term_from_cex vs model =
  Term.t_and_l
  @@ List.map
       (fun (n, v) ->
         let vt = Var.atom (SMap.find n vs) in
         if v then vt else Term.t_not vt)
       model

let solve i vs fmla =
  let task = fmla_to_task "g" fmla in
  (* Format.printf "@[%a@]@." Pretty.print_task task; *)
  let res = Solver.call task in
  match Solver.get_model res with
  | Some m ->
      let cex = Model.extract_cex m in
      Format.printf "Loop %d - CE:@." i;
      List.iter (fun (n, v) -> Format.printf "%s=%d@." n (Bool.to_int v)) cex;
      Format.printf "@.";
      Some (term_from_cex vs cex)
  | None ->
      Format.printf "Model unavailable@.";
      None

let rec cegar_loop i vs make_full ~interpolant =
  let fmla = make_full ~interpolant in
  match solve i vs fmla with
  | None -> ()
  | Some cex ->
      let interpolant = Term.t_or interpolant cex in
      (* Format.printf "@[Interpolant:@.%a@]@." Pretty.print_term interpolant; *)
      cegar_loop (i + 1) vs make_full ~interpolant

let () =
  let vs =
    List.init 7 Fun.id
    |> List.map (fun i -> "P" ^ string_of_int (i + 1))
    |> make_vmap
  in
  let critical = critical vs in
  let desirable = desirable vs in
  let assumption = assumption vs in
  let make_full = make_full vs ~critical ~desirable ~assumption in
  cegar_loop 0 vs make_full ~interpolant:desirable
