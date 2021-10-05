open Angstrom

type var =
  | Primary of string * int
  | Secondary of string * string list

let ws = skip_while (function ' ' | '\t' | '\n' -> true | _ -> false)
let plus = char '+' <* ws
let eq = char '=' <* ws

let num = take_while1 (function '0' .. '9' -> true | _ -> false) <* ws >>| int_of_string

let name = take_while1 (function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false) <* ws

let primary = (fun n v -> Primary (n, v)) <$> (name <* eq) <*> num
let secondary = (fun n vs -> Secondary (n, vs)) <$> (name <* eq) <*> sep_by1 plus name

let var = primary <|> secondary

let graph = many1 var <* end_of_input

let parse s =
  match parse_string ~consume:All graph s with
  | Error msg -> failwith msg
  | Ok v -> v

let read_file path =
  let ch = open_in path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let split =
  let rec go p s = function
    | Primary (n, v) :: ns -> go ((n, v) :: p) s ns
    | Secondary (n, vs) :: ns -> go p ((n, vs) :: s) ns
    | [] -> (List.rev p, List.rev s)
  in
  go [] []

let read_graph path =
  let input = read_file path in
  let graph = parse input in
  split graph
