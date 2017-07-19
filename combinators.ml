type 'a result = Success of 'a | Failure of string
type 'a parser = Parser of (string -> 'a result)

let run parser input =
  let (Parser parse) = parser in
  parse input

let zero = Parser (fun input -> Failure "")

let return x = Parser (fun input -> Success (x, input))

let (<|>) p q =
  Parser (fun input ->
    let result = run p input in
    match result with
    | Failure err      -> run q input
    | Success (hd, tl) -> Success (hd, tl)
  )

let (>>=) p f =
  Parser (fun input ->
    let result = run p input in
    match result with
    | Failure err      -> Failure err
    | Success (hd, tl) ->
        run (f hd) tl
  )

let (|>>) p f =
  Parser (fun input ->
    let result = run p input in
    match result with
    | Failure err      -> Failure err
    | Success (hd, tl) -> Success (f hd, tl)
  )

let (>>) p1 p2 = p1 >>= (fun _ -> p2)

let choice ps = List.fold_right (<|>) ps zero

let option p = (p |>> fun x -> Some x) <|> return None

let string_of_option_char = function
  | None   -> ""
  | Some x -> String.make 1 x

let rec many p = (
  p
  >>= fun r  -> many p
  >>= fun rs -> return (r::rs)
) <|> (return [])

let between op cl p = op >> p >>= (fun x -> cl >> return x)

let separated p sep =
  p               >>= fun x  ->
  many (sep >> p) >>= fun xs ->
  return (x::xs)

let any =
  Parser (fun input ->
    let len = String.length input in
    match len with
    | 0 -> Failure "EOL"
    | _ -> Success (input.[0], (String.sub input 1 (len - 1)))
  )

let satisfy pred =
  any
  >>= fun x ->
    if (pred x) then
      return x
    else
      zero

let char c = satisfy (fun x -> x = c)

let lower        = satisfy (fun c -> c >= 'a' && c <= 'z')

let upper        = satisfy (fun c -> c >= 'A' && c <= 'Z')

let digit        = satisfy (fun c -> c >= '0' && c <= '9')

let alpha        = lower <|> upper

let alphaNumeric = alpha <|> digit

let implode cs = String.concat "" (List.map (String.make 1) cs)

let word w =
  alpha
  >>= fun c -> many alpha
  >>= fun cs -> return (implode (c::cs))
  >>= fun x ->
    if x = w then
      return x
    else
      zero
