open Core
open Json_types

exception PraseException of string

(* Types *)

type object_parser_state =
 | WaitingForKey
 | WaitingForSeperator
 | WaitingForKeyValueSeperator of string
 | WaitingForValue of string

type array_parser_state =
 | WaitingForValue
 | WaitingForSeperator

(* Parser *)

let rec parse_json_value text =
  (* Parse string *)
  let rec parse_string_helper text = (
    match Str_utils.get_first_char text with
    | "\"" -> ("", String.drop_prefix text 1)
    | "\\" -> 
      let (value, remaining) = parse_string_helper (String.drop_prefix text 2) in
      ((String.prefix text 2) ^ value, remaining)
    | _ -> 
      let (value, remaining) = parse_string_helper (String.drop_prefix text 1) in
      ((String.prefix text 1) ^ value, remaining)
  ) in

  let parse_string text = (parse_string_helper (String.drop_prefix text 1)) in


  (* Parse number *)
  let rec parse_number_helper text = (
    match Str_utils.get_first_char text with
    | x when Str_utils.is_float_number_char x && String.length text = 1 -> (x, "")
    | x when Str_utils.is_float_number_char x ->
      let (number_str, remaining) = parse_number_helper (String.drop_prefix text 1) in
      (x ^ number_str, remaining)
    | (" " | "," | "}" | "]" | "\n" | "\r") -> ("", text)
    | x -> raise (PraseException ("Invalid character in number (" ^ x ^ ")"))
  ) in

  let parse_number text = (
    let (number_str, remaining) = parse_number_helper text in
    (Float.of_string number_str, remaining)
  ) in

  (* Parse null and boolean *)
  let rec parse_any_value text = (
    match Str_utils.get_first_char text with
    | (" " | "," | "}" | "]" | "\n" | "\r") -> ("", text)
    | x when String.length text = 1 -> (x, "")
    | x ->
      let (value, remaining) = parse_any_value (String.drop_prefix text 1) in
      (x ^ value, remaining)
  ) in

  let parse_constant_value text = (
    let (value, remaining) = parse_any_value text in
    match value with
    | "null" -> (Null, remaining)
    | "true" -> (Bool(true), remaining)
    | "false" -> (Bool(false), remaining)
    | _ -> raise (PraseException ("Invalid value (" ^ value ^ ")"))
  ) in

  (* Parse object *)
  let rec parse_object_helper text current_object state = (
    let curr_char = Str_utils.get_first_char text in
    match (state, curr_char) with
    | ((WaitingForKey | WaitingForSeperator | WaitingForKeyValueSeperator(_) | WaitingForValue(_)), (" " | "\n" | "\r")) ->
       parse_object_helper (String.drop_prefix text 1) current_object state
    | (WaitingForSeperator, (",")) ->
       parse_object_helper (String.drop_prefix text 1) current_object WaitingForKey
    | ((WaitingForKey | WaitingForSeperator), "}") -> (current_object, String.drop_prefix text 1)
    | (WaitingForKey, "\"") ->
      let (key, remaining) = parse_string text in
      parse_object_helper remaining current_object (WaitingForKeyValueSeperator(key))
    | (WaitingForKeyValueSeperator(key), ":") ->
      parse_object_helper (String.drop_prefix text 1) current_object (WaitingForValue(key))
    | (WaitingForValue(key), x) when not (String.equal x " ") ->
      let (value, remaining) = parse_json_value text in
      let new_object = Map.set current_object ~key:key ~data:value in
      
      parse_object_helper remaining new_object WaitingForSeperator
    | (_, x) -> raise (PraseException("Failed to parse character (" ^ x ^ ")"))
  ) in

  let parse_object text = (parse_object_helper (String.drop_prefix text 1) (Map.empty (module String)) WaitingForKey) in


  (* Parse array *)
  let rec parse_array_helper text current_array state = (
    match (state, Str_utils.get_first_char text) with
    | ((WaitingForValue | WaitingForSeperator), (" " | "\n" | "\r")) ->
      parse_array_helper (String.drop_prefix text 1) current_array state
    | ((WaitingForValue | WaitingForSeperator), "]") -> (current_array, String.drop_prefix text 1)
    | (WaitingForSeperator, ",") ->
      parse_array_helper (String.drop_prefix text 1) current_array WaitingForValue
    | (WaitingForValue, x) when not (String.equal x " ") ->
      let (value, remaining) = parse_json_value text in
      parse_array_helper remaining (current_array @ [ value ]) WaitingForSeperator
    | (_, x) -> raise (PraseException ("Invalid character (" ^ x ^ ") in array"))
  ) in

  let parse_array text = (parse_array_helper (String.drop_prefix text 1) [] WaitingForValue) in

  (* Main parser *)
  match Str_utils.get_first_char text with
  | "\"" ->
    let (value, remaining) = parse_string text in
    (String(value), remaining)
  | ("n" | "t" | "f") -> parse_constant_value text
  | x when Str_utils.is_float_number_char x ->
    let (value, remaining) = parse_number text in
    (Number(value), remaining)
  | "{" ->
    let (obj_res, remaining) = parse_object text in
    (Object(obj_res), remaining)
  | "[" ->
    let (array, remaining) = parse_array text in
    (Array(array), remaining)
  | (" " | "\n" | "\r") -> parse_json_value (String.drop_prefix text 1)
  | x -> raise (PraseException ("Invalid fist character in json value (" ^ x ^ ")"))

let decode_json text = 
  let (json_value, _) = parse_json_value text in
  json_value