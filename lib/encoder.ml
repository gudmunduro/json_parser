open Core
open Json_types

let rec encode_json json = 
  let add_quotation string = ("\"" ^ string ^ "\"") in

  let json_array_to_string array = (
    let items_str = 
      List.map array ~f: encode_json |>
      String.concat ~sep: "," in
    "[" ^ items_str ^ "]"
  ) in

  let json_object_to_string obj = (
    let properties_str = 
      Map.to_alist obj |>
      List.map ~f: (fun (key, value) -> (add_quotation key) ^ ":" ^ (encode_json value)) |>
      String.concat ~sep: "," in
    "{" ^ properties_str ^ "}"
  ) in

  let bool_to_string bool = (
    match bool with
    | true -> "true"
    | false -> "false"
  ) in

  match json with
  | String(value) -> value |> add_quotation
  | Number(value) -> Float.to_string value
  | Null -> "null"
  | Array(array) -> json_array_to_string array
  | Object(obj) -> json_object_to_string obj
  | Bool(value) -> bool_to_string value
