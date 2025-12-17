module T = Types
open Result

let number_re = Str.regexp {|-?[0-9]+|}

let token_re =
  Str.regexp
    "~@\\|[][{}()'`~^@]\\|\"\\(\\\\.\\|[^\"]\\)*\"?\\|;.*\\|[^][  \n\
     {}('\"`,;)]*"

let tokenize str =
  str
  |> Str.full_split token_re
  |> List.filter_map (function
    | Str.Delim s when String.(s <> "") -> Some s
    | Str.Delim _ -> None
    | Str.Text _ -> None)

let is_comment s = Char.(s.[0] = ';')
let is_int_literal s = Str.string_match number_re s 0
let is_string_literal s = Char.(s.[0] = '"')
let is_keyword_literal s = Char.(s.[0] = ':')

let unexpected_eof expected =
  T.errstr (Printf.sprintf "Expected '%s', got EOF" expected)

let unescaped s =
  try Ok (Scanf.sscanf s "%S%!" Fun.id) with _ -> unexpected_eof "\""

let rec read_form ?(expected = "") = function
  | [] -> (
      match expected with
      | "" -> Error T.Nil (* this is used for empty line *)
      | x -> unexpected_eof x)
  | x :: tokens when is_comment x -> read_form tokens
  | "(" :: tokens -> read_list tokens
  | "[" :: tokens -> read_vector tokens
  | "{" :: tokens -> read_map tokens
  | "@" :: tokens -> read_quote "deref" tokens
  | "'" :: tokens -> read_quote "quote" tokens
  | "`" :: tokens -> read_quote "quasiquote" tokens
  | "~" :: tokens -> read_quote "unquote" tokens
  | "~@" :: tokens -> read_quote "splice-unquote" tokens
  | "^" :: tokens -> read_with_meta tokens
  | x :: tokens -> read_atom x >|= fun x -> (x, tokens)

and read_collection closing =
  let rec aux acc = function
    | [] -> unexpected_eof closing
    | x :: tokens when String.(x = closing) -> Ok (List.rev acc, tokens)
    | tokens ->
        let* form, tokens = read_form tokens ~expected:closing in
        aux (form :: acc) tokens
  in
  aux []

and read_list tokens = read_collection ")" tokens >|= Pair.map_fst T.list
and read_vector tokens = read_collection "]" tokens >|= Pair.map_fst T.vector

and read_map tokens =
  let* list, tokens = read_collection "}" tokens in
  let+ map = T.map_of_list T.MalMap.empty list in
  (map, tokens)

and read_quote symbol tokens =
  let+ form, tokens = read_form tokens ~expected:"expr" in
  (T.list [ T.symbol symbol; form ], tokens)

and read_with_meta tokens =
  let* meta, tokens = read_form tokens ~expected:"meta" in
  let+ value, tokens = read_form tokens ~expected:"expr" in
  (T.list [ T.symbol "with-meta"; value; meta ], tokens)

and read_atom = function
  | "nil" -> T.nil'
  | "true" -> T.maltrue'
  | "false" -> T.malfalse'
  | x when is_int_literal x -> (
      try T.int' (int_of_string x) with _ -> T.errstr "Number too big")
  | x when is_string_literal x -> unescaped x >|= T.string
  | x when is_keyword_literal x ->
      T.keyword' (String.sub x 1 (String.length x - 1))
  | x -> T.symbol' x

(* TODO: handle multi-line expressions *)
let read_str str =
  let rec aux tokens () =
    match read_form tokens with
    | Error T.Nil -> Seq.Nil
    | Ok (form, tokens) -> Seq.Cons (Ok form, aux tokens)
    | Error _ as err -> Seq.Cons (err, aux [])
  in
  aux (tokenize str)
