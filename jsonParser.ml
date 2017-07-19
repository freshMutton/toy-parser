open Combinators

module Obj = Map.Make(String)

type json =
  | String of string
  | Number of float
  | Boolean of bool
  | Null
  | Array of json list
  | Object of json Obj.t

let escParsers = choice [ char '\"' >> return '\"'
                        ; char '\\' >> return '\\'
                        ; char '/' >> return '/'
                        ; char 'b' >> return '\b'
                        ; char 'f' >> return '\012'
                        ; char 'n' >> return '\n'
                        ; char 'r' >> return '\r'
                        ; char 't' >> return '\t'
                        ]

let escapedChar = char '\\' >> escParsers

let unescapedChar = satisfy (fun c -> c != '\\' || c != '\"')

let quoted_string =
  let quote = char '"' in
  let quoted = between quote quote in

  quoted (many (unescapedChar <|> escapedChar)) |>> fun cs ->
  implode cs

let negative_sign = option (char '-') |>> string_of_option_char

let zero_int = char '0' >> return "0"

let non_zero_int =
  satisfy (fun c -> c >= '1' && c <= '9') >>= fun c  ->
  many digit                              >>= fun cs ->
  return (implode (c::cs))

let exponent =
  (char 'e' <|> char 'E') >> return "e" >>= fun e ->
  option (char '+' <|> char '-')        |>>
  string_of_option_char                 >>= fun s ->
  many digit                            >>= fun ds ->
  return (implode ds)                   >>= fun d ->
  return (e ^ s ^ d)

let optional_exponent =
  option exponent |>> function
    | None   -> ""
    | Some x -> x

let json_null = word "null" >> return Null

let json_bool =
  let parse_true = word "true" >> return (Boolean true) in
  let parse_false = word "false" >> return (Boolean false) in

  parse_true <|> parse_false

let json_string = quoted_string |>> fun x -> String x

let json_number =
  negative_sign                                                    >>= fun s ->
  zero_int <|> non_zero_int                                        >>= fun i ->
  char '.' |>> String.make 1                                       >>= fun d ->
  many digit |>> implode                                           >>= fun f ->
  optional_exponent                                                |>> fun e ->
  Number (float_of_string (s ^ i ^ d ^ f ^ e))

(* TODO: recursive let bindings don't play nicely with function application,
 * need to work this out to get arrays and objects parsing *)

let json = choice [ json_null
                  ; json_bool
                  ; json_string
                  ; json_number
                  ]

let json_array =
  let l = char '[' in
  let r = char ']' in
  let sep = char ',' in
  let separated_json = separated json sep in

  between l r separated_json |>> fun x ->
  Array x

let json_object =
  let l = char '{' in
  let r = char '}' in
  let colon = char ':' in
  let sep = char ',' in
  let obj_of_list =
    List.fold_left (fun acc (k,v) -> Obj.add k v acc) Obj.empty
  in
  let pair =
    quoted_string              >>= fun k ->
    colon                      >>
    json                       >>= fun v ->
    return (k, v)
  in
  let separated_pair = separated pair sep in

  between l r separated_pair |>> fun x ->
  Object (obj_of_list x)
