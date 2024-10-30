open Why3

let config = Whyconf.init_config None
let main = Whyconf.get_main config
(* let provers = Whyconf.get_provers config *)

let z3 =
  let fp = Whyconf.parse_filter_prover "Z3" in
  let provers = Whyconf.filter_provers config fp in
  if Whyconf.Mprover.is_empty provers then (
    Format.eprintf "Prover Z3 not installed or not configured@.";
    exit 1)
  else (
    Format.printf "Versions of Z3 found:";
    Whyconf.(
      Mprover.iter (fun k _ -> Format.printf " %s" k.prover_version) provers);
    Format.printf "@.";
    snd (Whyconf.Mprover.max_binding provers))

let env = Env.create_env (Whyconf.loadpath main)

let z3_driver =
  try Driver.load_driver_for_prover main env z3
  with e ->
    Format.eprintf "Failed to load driver for alt-ergo: %a@."
      Exn_printer.exn_printer e;
    exit 1

let call task =
  Call_provers.wait_on_call
    (Driver.prove_task ~limit:Call_provers.empty_limit ~config:main
       ~command:z3.Whyconf.command z3_driver task)
