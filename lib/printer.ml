module T = Types

let pr_str readably =
  let rec aux = function
    | T.Nil -> "nil"
    | T.Bool x -> string_of_bool x
    | T.Int x -> string_of_int x
    | T.String x when readably -> "\"" ^ String.escaped x ^ "\""
    | T.String x | T.Symbol x -> x
    | T.Keyword x -> ":" ^ x
    | T.List x -> List.to_string ~start:"(" ~stop:")" ~sep:" " aux x
    | T.Vector x -> List.to_string ~start:"[" ~stop:"]" ~sep:" " aux x
    | T.Map x ->
        T.MalMap.to_list x
        (* |> List.rev *)
        |> List.to_string ~start:"{" ~stop:"}" ~sep:" " (fun (k, v) ->
            Printf.sprintf "%s %s" (aux k) (aux v))
    | T.Fn _ -> "#<fn>"
  in
  aux
