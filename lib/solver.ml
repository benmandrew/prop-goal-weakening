open Why3
open Whyconf

let config = init_config None
let main = get_main config
let env = Env.create_env (Whyconf.loadpath main)

module Prover = struct
  type t = { cfg : config_prover; driver : Driver.driver }

  let cfg name =
    let open Whyconf in
    let fp = parse_filter_prover name in
    let provers = filter_provers config fp in
    if Mprover.is_empty provers then (
      Format.eprintf "Prover %s not installed or not configured@." name;
      exit 1)
    else (
      Format.printf "Versions of %s found:" name;
      let provers =
        Mprover.filter
          (fun k _ -> String.equal k.prover_altern "counterexamples")
          provers
      in
      Mprover.iter
        (fun k _ -> Format.printf " %s (%s)," k.prover_version k.prover_altern)
        provers;
      Format.printf "@.";
      snd (Mprover.max_binding provers))

  let driver name env cfg =
    try Driver.load_driver_for_prover main env cfg
    with e ->
      Format.eprintf "Failed to load driver for %s: %a@." name
        Exn_printer.exn_printer e;
      exit 1

  let make name =
    let cfg = cfg name in
    { cfg; driver = driver name env cfg }

  let call t task =
    Call_provers.wait_on_call
      (Driver.prove_task ~limit:Call_provers.empty_limit ~config:main
         ~command:t.cfg.command t.driver task)

  let get_model res =
    Check_ce.select_model_last_non_empty res.Call_provers.pr_models
end

(* let prover = Prover.make "Alt-Ergo" *)
let prover = Prover.make "Z3"

(* let prover = Prover.make "CVC5" *)
let call = Prover.call prover
let get_model = Prover.get_model
