open Core
open Json_parser_lib.Encoder
open Json_parser_lib.Decoder

let () =
  let text = In_channel.read_all "json_test_file.json" in
  let parsed = decode_json text in
  let encoded = encode_json parsed ~formatted: true in
  let () = print_endline "Parsed value:" in
  print_endline encoded