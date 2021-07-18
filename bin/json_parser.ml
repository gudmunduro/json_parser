open Core
open Json_parser_lib.Parser
open Json_parser_lib.Encoder

let () =
  let text = In_channel.read_all "json_test_file.json" in
  let parsed = parse_json text in
  let encoded = encode_json parsed in
  print_endline encoded