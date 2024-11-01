open Why3

val call : Task.task -> Call_provers.prover_result
val get_model : Call_provers.prover_result -> Model_parser.model option
val get_models : Call_provers.prover_result -> Model_parser.model list
