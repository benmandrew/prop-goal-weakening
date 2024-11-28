open Why3
(* open Formula *)

module A = Map.Make (struct
  type t = Term.lsymbol

  let compare = Term.ls_compare
end)

(* let eval a { vars; f } = *)
let rec eval a f =
  let open Term in
  match f.t_node with
  | Tbinop (Tand, p, q) -> eval a p && eval a q
  | Tbinop (Tor, p, q) -> eval a p || eval a q
  | Tbinop (Timplies, p, q) -> (not @@ eval a p) || eval a q
  | Tbinop (Tiff, p, q) -> eval a p == eval a q
  | Tnot p -> not @@ eval a p
  | Ttrue -> true
  | Tfalse -> false
  | Tapp (p, []) -> A.find p a
  | _ ->
      Format.printf "'%a' not supported\n" Pretty.print_term f;
      raise Not_found
