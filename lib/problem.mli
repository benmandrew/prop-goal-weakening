module SMap : Map.S with type key = string
module TSet : Set.S with type elt = Why3.Term.term

type problem = {
  assumption : TSet.elt;
  desirable : TSet.elt;
  critical : TSet.elt;
  interpolant : TSet.t;
}

val print : problem -> unit
val make_vmap : SMap.key list -> Formula.Var.t SMap.t
val make_atom : Formula.Var.t SMap.t -> SMap.key -> Why3.Term.term
val critical : 'a -> Why3.Term.term
val desirable : Formula.Var.t SMap.t -> Why3.Term.term
val assumption : Formula.Var.t SMap.t -> Why3.Term.term
val init : Formula.Var.t SMap.t -> problem
val get_fmla : Formula.Var.t SMap.t -> problem:problem -> Formula.fmla
val add_cex : problem -> TSet.elt -> problem
