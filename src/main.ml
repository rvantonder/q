open Angstrom

module Parse_tree = struct
  type value = string
  [@@deriving yojson]

  type parameter =
    { keyword : string
    ; value : value
    }
  [@@deriving yojson]

  type parameters = parameter list
  [@@deriving yojson]

  type t = parameters
  [@@deriving yojson]

  let empty : t = []

  (* Preserves the quotes and escape character *)
  let single_quoted =
    let escapable = char '\\' *> any_char >>| fun c -> [ '\\'; c ] in
    let unescaped = (not_char '"' >>| fun c -> [ c ]) in
    let either = many1 @@ choice [ escapable; unescaped ] >>| List.concat in
    lift3 (fun _ value _ -> ['"'] @ value @ ['"']) (char '"') either (char '"')

  (* Preserves the quotes and escape character *)
  let double_quoted =
    let escapable = char '\\' *> any_char >>| fun c -> [ '\\'; c ] in
    let unescaped = (not_char '\'' >>| fun c -> [ c ]) in
    let either = many1 @@ choice [ escapable; unescaped ] >>| List.concat in
    lift3 (fun _ value _ -> ['\''] @ value @ ['\'']) (char '\'') either (char '\'')

  (* Preserves escape character *)
  module Parameter = struct
    let literal_char =
      let escapable = char '\\' *> satisfy (function ' ' | ':' | '-' | '\\' | '"' | '\'' | '(' | ')' -> true | _ -> false ) >>| fun c -> [ '\\'; c ] in
      (* It's possible to lift the ':' restriction and apply it only to
         search_pattern, which is where it is truly needed. Realistically,
         though, it's dumb to allow foo:blah:x *)
      let unescaped = satisfy (function ':' | ' ' | '\n' | '\r' | '\t' | '"' | '\'' | '(' | ')' -> false | _ -> true) >>| fun c -> [ c ] in
      many1 @@ choice [ escapable; unescaped ] >>| List.concat

    let string_of_char_list chars =
      let buf = Buffer.create 16 in
      List.iter (Buffer.add_char buf) chars;
      Buffer.contents buf

    let keyword = many1 @@ satisfy (function 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true | _ -> false) >>| string_of_char_list

    let value = choice [ single_quoted; double_quoted; literal_char ] >>| string_of_char_list
  end

  let whitespace = many1 @@ satisfy (function ' ' | '\n' | '\r' | '\t' -> true | _ -> false)
  let parameter =
    lift4 (fun negated keyword _ value -> { keyword = negated ^ keyword; value })
      (option "" (char '-' >>| fun _ -> "-")) Parameter.keyword (char ':') Parameter.value
  let search_pattern = Parameter.value

  let parameter =
    choice
      [ parameter
      ; search_pattern >>= fun value -> return { keyword = "content"; value }
      ]

  let parameters = sep_by1 whitespace parameter

  let parser = parameters <* end_of_input

  let parse s =
    parse_string parser s

  let of_string s =
    match parse_string parser s with
    | Error e -> failwith (Format.sprintf "Invalid: %s@." e)
    | Ok expression ->
      Format.printf "%s@." @@ Yojson.Safe.pretty_to_string @@ to_yojson expression;
      expression
end

open Parse_tree

module Expression_parse_tree = struct

  type op =
    | And
    | Or
    | Not
  [@@deriving yojson]

  type expr =
    | Expr of op * expr list
    | Parameter of parameter

  let expr_of_yojson _ = failwith "dont care"

  let rec expr_to_yojson (expr : expr) : Yojson.Safe.t =
    match expr with
    | Expr (And, xs) -> `Assoc [("AND", `List (List.map expr_to_yojson xs))]
    | Expr (Or, xs) -> `Assoc [("OR", `List (List.map expr_to_yojson xs))]
    | Expr (Not, xs) -> `Assoc [("NOT", `List (List.map expr_to_yojson xs))]
    | Parameter parameter -> `Assoc [("Parameter", parameter_to_yojson parameter)]

  type t = expr list
  [@@deriving yojson]

  let make_parameters ps =
    List.map (fun p -> Parameter p) ps

  let chainl1 e op =
    let rec go acc =
      (lift2 (fun f x -> f acc x) op e >>= go) <|> return acc in
    e >>= fun init -> go init

  let parens p = char '(' *> (p <|> return []) <* char ')'
  (* Doesn't use parameters above, because we don't want that parser to continue
     parsing many parameters separated by whitespace. *)
  let parameters = parameter >>= fun ps -> return (make_parameters [ps])
  let not_ term = string "NOT" *> whitespace *> term >>= fun term -> return [Expr (Not, term)]
  let and_ = whitespace *> string "AND" *> whitespace *>  return (fun left right -> [Expr (And, left@right)])
  let or_ = whitespace *> string "OR" *> whitespace *> return (fun left right -> [Expr (Or, left@right)])

  let optional_whitespace = many @@ satisfy (function ' ' | '\n' | '\r' | '\t' -> true | _ -> false)

  let expr : expr list Angstrom.t =
    fix (fun expr ->
        let term = fix (fun term -> parens expr <|> not_ term <|> parameters) in
        let and_term  = chainl1 term and_ in
        let repeat_term = chainl1 and_term or_ in
        many (optional_whitespace *> repeat_term <* optional_whitespace) >>| List.concat)

  let parser = expr <* end_of_input

  let parse s =
    parse_string parser s

  let of_string s =
    match parse_string parser s with
    | Error e -> Format.printf "No parse %s@." e; []
    | Ok expression ->
      Format.printf "%s@." @@ Yojson.Safe.pretty_to_string @@ to_yojson expression;
      expression
end

open Js_of_ocaml

let js_parse : Js.js_string Js.t -> Js.js_string Js.t =
  fun s ->
  let expression = Expression_parse_tree.parse (Js.to_string s) in
  let string_result =
    match expression with
    | Ok expression -> Yojson.Safe.pretty_to_string @@ Expression_parse_tree.to_yojson expression
    | Error _ -> "Parse error."
  in
  Js.string string_result

let () =
  print_endline "exporting 'parse'";
  Js.export "parse" js_parse
