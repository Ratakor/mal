module T = Types

let rec pr_str readably = function
  | T.Nil -> "nil"
  | T.Bool x -> string_of_bool x
  | T.Int x -> string_of_int x
  | T.String x when readably -> "\"" ^ String.escaped x ^ "\""
  | T.String x | T.Symbol x -> x
  | T.Keyword x -> ":" ^ x
  | T.List x -> List.to_string ~start:"(" ~stop:")" ~sep:" " (pr_str readably) x
