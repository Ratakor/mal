module T = Types

let rec pr_str readably = function
  | T.Nil -> "nil"
  | T.Bool x -> string_of_bool x
  | T.Int x -> string_of_int x
  | T.String s when readably -> "\"" ^ String.escaped s ^ "\""
  | T.String s -> s
  | T.Symbol s -> s
  | T.Keyword s -> s
  | T.List x -> List.to_string ~start:"(" ~stop:")" ~sep:" " (pr_str readably) x
