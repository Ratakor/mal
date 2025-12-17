open Printf
module T = Types

let read str = Reader.read_str str

let rec eval env ast =
  let open Result in
  (match Env.get "DEBUG-EVAL" env with
  | None | Some T.Nil | Some (T.Bool false) -> ()
  | _ -> printf "EVAL: %s\n%!" (T.to_string true ast));

  match ast with
  | T.Symbol x -> (
      match Env.get x env with
      | Some v -> Ok v
      | None -> T.errstr (sprintf "'%s' not found" x))
  | T.List (x :: xs, _) -> (
      match eval env x with
      | Ok (T.Fn (f, _)) -> T.LT.map_m (eval env) xs >>= f
      | Ok _ -> T.errstr (sprintf "'%s' is not callable" (T.to_string true x))
      | Error e -> Error e)
  | T.Vector (xs, _) -> T.LT.map_m (eval env) xs >|= Types.vector
  | T.Map (xs, _) ->
      Types.MalMap.fold
        (fun k v acc ->
          let* acc' = acc in
          let* k' = eval env k in
          let* v' = eval env v in
          return (Types.MalMap.add k' v' acc'))
        xs (Ok Types.MalMap.empty)
      >|= Types.map
  | x -> Ok x

let print exp = T.to_string true exp

let repl_env =
  let int_fn f =
    Types.fn (function
      | [ T.Int a; T.Int b ] -> Ok (T.Int (f a b))
      | _ -> T.errstr "Invalid argument")
  in
  let env = Env.make None in
  Env.set "+" (int_fn ( + )) env;
  Env.set "-" (int_fn ( - )) env;
  Env.set "*" (int_fn ( * )) env;
  Env.set "/" (int_fn ( / )) env;
  Env.set "DEBUG-EVAL" (T.Bool false) env;
  env

let rep str =
  read str
  |> Seq.map (function
    | Ok ast -> Result.(eval repl_env ast >|= print)
    | Error _ as err -> err)

let () =
  try
    while true do
      printf "user> %!";
      read_line ()
      |> rep
      |> Seq.iter (function
        | Ok x -> printf "%s\n%!" x
        | Error x -> printf "Error: %s\n%!" (T.to_string false x))
    done
  with End_of_file -> print_newline ()
