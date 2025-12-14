module T = Types

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

let is_int_literal s = Str.string_match number_re s 0
let is_string_literal s = Char.(s.[0] = '"')
let is_keyword_literal s = Char.(s.[0] = ':')
let unescaped s = Scanf.sscanf s "%S%!" (fun x -> x)

let rec read_form = function
  | [] -> Error None
  | "(" :: tokens -> read_list tokens
  | x :: tokens -> Result.(read_atom x >|= fun x -> (x, tokens))

and read_collection closing forms = function
  | [] -> Error (Some ("Unmatched " ^ closing))
  | x :: tokens when String.(x = closing) -> Ok (forms, tokens)
  | tokens ->
      Result.(
        read_form tokens
        >>= fun (form, tokens) ->
        read_collection closing (forms @ [ form ]) tokens)

and read_list tokens =
  Result.(read_collection ")" [] tokens >|= Pair.map_fst T.list)

and read_atom = function
  | "nil" -> Ok T.Nil
  | "true" -> Ok (T.Bool true)
  | "false" -> Ok (T.Bool false)
  | x when is_int_literal x -> Ok (T.Int (int_of_string x))
  | x when is_string_literal x ->
      let len = String.length x in
      if len = 1 || Char.(x.[len - 1] <> '"') then Error (Some "Unmatched \"")
      else Ok (T.String (unescaped x))
  | x when is_keyword_literal x ->
      Ok (T.Keyword (String.sub x 1 (String.length x - 1)))
  | x -> Ok (T.Symbol x)

let read_str str =
  Result.(
    str
    |> tokenize
    |> read_form
    >>= fun (form, tokens) ->
    match tokens with
    | [] -> Ok form
    | _ ->
        Error (Some ("Remaining tokens: " ^ List.to_string (fun x -> x) tokens)))
