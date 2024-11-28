open Why3
open Craigar

let vs = Problem.make_vmap @@ List.init 2 (fun i -> "P" ^ string_of_int (i + 1))

(** Truth assignments to propositional literals;
    if the literal exists in the set it's true, otherwise false *)
module A = struct
  include Set.Make (struct
    type t = Term.lsymbol

    let compare = Term.ls_compare
  end)

  let valid =
    Problem.SMap.bindings vs
    |> List.map (fun x -> (snd x).Formula.Var.n)
    |> of_list

  let to_string t =
    let vars =
      List.map (fun l -> if mem l t then l.Term.ls_name.id_string else "  ")
      @@ to_list valid
    in
    String.concat " " vars
end

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
  | Tapp (p, []) -> A.mem p a
  | _ ->
      Format.printf "'%a' not supported\n" Pretty.print_term f;
      raise Not_found

(** Boolean function represented as a truth table;
    if a set of truth assignments exists then it's true, otherwise false *)
module Func = struct
  include Set.Make (A)

  (** Generate function that's always true, i.e. valid *)
  let valid =
    A.fold
      (fun x ps -> fold (fun ss -> add (A.add x ss)) ps ps)
      A.valid (singleton A.empty)

  (** Generate all possible functions *)
  let all =
    fold
      (fun x ps -> List.fold_left (fun acc ss -> add x ss :: acc) ps ps)
      valid [ empty ]

  let generate t = filter (fun a -> eval a t) valid
  let implies f f' = for_all (fun a -> mem a f') f

  (** Covering relation for drawing Hasse diagrams *)
  let covers all f f' =
    (not (equal f f'))
    && implies f f'
    && List.for_all
         (fun g ->
           (not ((not (equal f g)) && not (equal g f')))
           || not (implies f g && implies g f'))
         all

  let to_string t =
    let a = List.sort A.compare @@ to_list t in
    String.concat "\n"
    @@ List.map (fun x -> Format.sprintf "+ %s" @@ A.to_string x) a
end

let () =
  Format.printf "n = %d\n" (List.length @@ Func.all);
  let f =
    Func.generate @@ Term.t_and_l
    @@ List.map (Problem.make_atom vs) [ "P1"; "P2" ]
  in
  (* let f' =
       Func.generate @@ Term.t_or_l
       @@ List.map (Problem.make_atom vs) [ "P1"; "P2" ]
     in *)
  (* let filter x = Func.implies f x && Func.implies x f' in *)
  (* let all = List.filter filter Func.all in *)
  Format.printf "Covers of\n%s\n\n" @@ Func.to_string f;
  let covers = List.filter (Func.covers Func.all f) Func.all in
  List.iter (fun a -> Format.printf "%s\n\n" @@ Func.to_string a) covers
(* Format.printf "n = %d\n"
   (List.length @@ List.filter (Func.covers Func.all f) Func.all) *)
