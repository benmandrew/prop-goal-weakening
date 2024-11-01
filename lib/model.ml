open Why3

let extract_var json =
  let open Json_base in
  let name = get_field json "lsymbol" |> fun j -> get_string_field j "name" in
  let value =
    get_field json "value" |> fun j ->
    get_field j "value_concrete_term" |> fun j -> get_bool_field j "val"
  in
  (name, value)

let extract_cex model =
  let open Json_base in
  let res =
    Model_parser.json_model model
    |> get_list
    |> List.map (fun j ->
           get_field j "model" |> get_list
           |> List.map (fun j ->
                  get_field j "model_elements"
                  |> get_list |> List.map extract_var))
    |> List.flatten
  in
  (* If assert triggers there may be more than one model returned *)
  assert (List.length res == 1);
  List.flatten res
