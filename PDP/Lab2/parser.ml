open Angstrom

let ws = skip_while (function ' ' | '\t' -> true | _ -> false)
let eol = end_of_line <* ws
let num = take_while1 (function '0' .. '9' -> true | _ -> false) <* ws >>| int_of_string

let vector = many1 num <* eol
let vectors = both vector vector

let parse s =
  match parse_string ~consume:All vectors s with
  | Error msg -> failwith msg
  | Ok v -> v

let read_file path =
  let ch = open_in path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let read_vectors path =
  let a, b = parse (read_file path) in
  List.combine a b
