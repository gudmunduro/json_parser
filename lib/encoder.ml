open Core
open Json_types

let encode_json ?formatted: (formatted=false) json =
  (* Shared *)
  let bool_to_string bool = (
    match bool with
    | true -> "true"
    | false -> "false"
  ) in

  let add_quotation string = ("\"" ^ string ^ "\"") in

  (* Minified *)
  let rec encode_value json = (
    let json_array_to_string array = (
      let items_str = 
        List.map array ~f: encode_value |>
        String.concat ~sep: "," in
      "[" ^ items_str ^ "]"
    ) in

    let json_object_to_string obj = (
      let properties_str = 
        Map.to_alist obj |>
        List.map ~f: (fun (key, value) -> (add_quotation key) ^ ":" ^ (encode_value value)) |>
        String.concat ~sep: "," in
      "{" ^ properties_str ^ "}"
    ) in

    match json with
    | String(value) -> value |> add_quotation
    | Number(value) -> Float.to_string value
    | Null -> "null"
    | Array(array) -> json_array_to_string array
    | Object(obj) -> json_object_to_string obj
    | Bool(value) -> bool_to_string value
  ) in
    
    (* Formatted *)
    let rec encode_formatted_value json ~level = (
      let indent = String.make (level * 4) ' ' in
      let inner_indent = String.make ((level + 1) * 4) ' ' in

      let json_array_to_string array = (
        let items_str = 
          List.map array ~f: (fun element -> (encode_formatted_value element ~level: (level + 1)) ) |>
          String.concat ~sep: (",\n" ^ inner_indent)  in
          "[" ^ "\n" ^ inner_indent ^ items_str ^ "\n" ^ indent ^ "]"
      ) in
    
      let json_object_to_string obj = (
        let properties_str = 
          Map.to_alist obj |>
          List.map ~f: (fun (key, value) -> (add_quotation key) ^ ": " ^ (encode_formatted_value value ~level: (level + 1))) |>
          String.concat ~sep: (",\n" ^ inner_indent) in
          "{" ^ "\n" ^ inner_indent ^ properties_str ^ "\n" ^ indent ^ "}"
      ) in
    
      match json with
      | String(value) -> value |> add_quotation
      | Number(value) -> Float.to_string value
      | Null -> "null"
      | Array(array) -> json_array_to_string array
      | Object(obj) -> json_object_to_string obj
      | Bool(value) -> bool_to_string value
    ) in

    if formatted then encode_formatted_value json ~level: 0
    else encode_value json