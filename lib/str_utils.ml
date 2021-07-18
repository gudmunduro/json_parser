open Core

let char_at_index string index =
  if Int.is_negative index then
    String.sub string ~pos: ((String.length string) - index) ~len: 1
  else
    String.sub string ~pos: index ~len: 1

let cut_char_at string index =
  String.sub string ~pos: 0 ~len: index ^
  String.sub string ~pos: (index + 1) ~len: ((String.length string) - (index + 1))

let matches string regex = Str.string_match (Str.regexp regex) string 0

let get_first_char string = String.prefix string 1

let get_last_char string = String.suffix string 1

let is_float_number_char string = matches string "[0-9.]"