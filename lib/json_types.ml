open Core

type json_value =
 | Object of (string,json_value,String.comparator_witness) Map.t
 | Array of json_value list
 | Number of float
 | String of string
 | Bool of bool
 | Null