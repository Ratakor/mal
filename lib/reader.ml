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

let rec read_form = function
  | [] -> None
  | "(" :: tokens -> read_list tokens
  | x :: tokens -> Option.(read_atom x >|= fun x -> (x, tokens))

and read_collection closing forms = function
  | [] -> None
  | x :: tokens when String.(x = closing) -> Some (forms, tokens)
  | tokens ->
      Option.(
        read_form tokens
        >>= fun (form, tokens) ->
        read_collection closing (forms @ [ form ]) tokens)

and read_list tokens =
  Option.(read_collection ")" [] tokens >|= Pair.map_fst T.list)

and read_atom = function
  | "nil" -> Some T.Nil
  | "true" -> Some (T.Bool true)
  | "false" -> Some (T.Bool false)
  | x when is_int_literal x -> Some (T.Int (int_of_string x))
  (* | x when is_string_literal x -> Some *)
  | symbol -> Some (T.Symbol symbol)

let read_str str = Option.(str |> tokenize |> read_form >|= fst)
