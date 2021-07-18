open Core

let matches string regex = Str.string_match (Str.regexp regex) string 0

let get_first_char string = String.prefix string 1

let get_last_char string = String.suffix string 1

let is_float_number_char string = matches string "[0-9.-]"