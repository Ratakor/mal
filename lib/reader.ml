module T = Types.Types

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

let unescaped s =
  try Ok (Scanf.sscanf s "%S%!" Fun.id)
  with e -> Error (Some (Printexc.to_string e))

let unexpected_eof expected =
  Error (Some (Printf.sprintf "Expected '%s', got EOF" expected))

let rec read_form = function
  | [] -> Error None
  | x :: tokens when is_comment x -> read_form tokens
  | "(" :: tokens -> read_list tokens
  | "[" :: tokens -> read_vector tokens
  | "{" :: tokens -> read_map tokens
  | "@" :: tokens -> read_quote "deref" tokens
  | x :: tokens -> Result.(read_atom x >|= fun x -> (x, tokens))

and read_collection closing =
  let rec aux acc = function
    | [] -> unexpected_eof closing
    | x :: tokens when String.(x = closing) -> Ok (List.rev acc, tokens)
    | tokens -> (
        match read_form tokens with
        | Error None -> unexpected_eof closing
        | Error _ as err -> err
        | Ok (form, tokens) -> aux (form :: acc) tokens)
  in
  aux []

and read_list tokens =
  Result.(read_collection ")" tokens >|= Pair.map_fst Types.list)

and read_vector tokens =
  Result.(read_collection "]" tokens >|= Pair.map_fst Types.vector)

and read_map tokens =
  let open Result in
  let* list, tokens = read_collection "}" tokens in
  Types.map_of_list list |> Result.map2 (fun m -> (m, tokens)) (fun e -> Some e)

and read_quote symbol tokens =
  match read_form tokens with
  | Error None -> unexpected_eof "expr"
  | Error _ as err -> err
  | Ok (form, tokens) -> Ok (T.List [ T.Symbol symbol; form ], tokens)

and read_atom = function
  | "nil" -> Ok T.Nil
  | "true" -> Ok (T.Bool true)
  | "false" -> Ok (T.Bool false)
  | x when is_int_literal x -> Ok (T.Int (int_of_string x))
  | x when is_string_literal x -> Result.(unescaped x >|= Types.string)
  | x when is_keyword_literal x ->
      Ok (T.Keyword (String.sub x 1 (String.length x - 1)))
  | x -> Ok (T.Symbol x)

let read_str str =
  let open Result in
  str
  |> tokenize
  |> read_form
  >>= fun (form, tokens) ->
  match tokens with
  | [] -> return form
  | x :: _ when is_comment x -> return form
  | _ -> fail (Some ("Remaining tokens: " ^ List.to_string Fun.id tokens))
