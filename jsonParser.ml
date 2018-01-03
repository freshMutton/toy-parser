open Combinators

type json =
  | String  of string
  | Number  of float
  | Boolean of bool
  | Null
  | Array   of json list
  | Object  of (json * json) list

let escapedChar =
  char '\\' >>
  choice [ char '\"' >> return '\"'
         ; char '\\' >> return '\\'
         ; char '/'  >> return '/'
         ; char 'b'  >> return '\b'
         ; char 'f'  >> return '\012'
         ; char 'n'  >> return '\n'
         ; char 'r'  >> return '\r'
         ; char 't'  >> return '\t'
         ]

let unescapedChar = satisfy (fun c -> c != '\\' && c != '\"')

let quoted_string =
  let quote  = char '"' in
  between quote quote ((many unescapedChar) <|> (many escapedChar)) |>> fun cs ->
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

let fraction =
  char '.'   |>> String.make 1 >>= fun d ->
  many digit |>> implode       >>= fun f ->
  return (d ^ f)

let optional_fraction =
  option fraction |>> function
    | None   -> ""
    | Some x -> x

let json_null = word "null" >> return Null

let json_bool =
  let parse_true  = word "true"  >> return (Boolean true) in
  let parse_false = word "false" >> return (Boolean false) in
  parse_true <|> parse_false

let json_string = quoted_string |>> fun x -> String x

let json_number =
  negative_sign                >>= fun s ->
  zero_int <|> non_zero_int    >>= fun i ->
  optional_fraction            >>= fun f ->
  optional_exponent            |>> fun e ->
  Number (float_of_string (s ^ i ^ f ^ e))

let rec json_value =
  Parser (fun input ->
    let json_parser = choice [ json_null
                             ; json_bool
                             ; json_string
                             ; json_number
                             ; json_array
                             ; json_object
                             ]
    in
    run json_parser input)
and json_array =
  Parser (fun input ->
    let l              = trim (char '[') in
    let r              = trim (char ']') in
    let sep            = trim (char ',') in
    let separated_json = separated json_value sep in
    let array_parser   =
      between l r separated_json |>> fun x ->
      Array x
    in
    run array_parser input)
and json_object =
  Parser (fun input ->
    let l              = trim (char '{') in
    let r              = trim (char '}') in
    let colon          = trim (char ':') in
    let sep            = trim (char ',') in
    let pair           =
      json_string >>= fun k ->
      colon       >>
      json_value  >>= fun v ->
      return (k, v)
    in
    let separated_pair = separated pair sep in
    let object_parser  =
      between l r separated_pair |>> fun x ->
      Object x
    in
    run object_parser input)
