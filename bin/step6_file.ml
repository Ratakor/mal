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
  | T.List ([ T.Symbol "def!"; T.Symbol key; expr ], _) ->
      let* value = eval env expr in
      Env.set key value env;
      Ok value
  | T.List ([ T.Symbol "let*"; T.List (binds, _); body ], _)
  | T.List ([ T.Symbol "let*"; T.Vector (binds, _); body ], _) ->
      let sub_env = Env.make (Some env) in
      let rec bind_pairs = function
        | T.Symbol key :: expr :: tail ->
            let* value = eval sub_env expr in
            Env.set key value sub_env;
            bind_pairs tail
        | _ :: _ :: _ -> T.errstr "'let*' keys must be symbols"
        | _ :: [] ->
            T.errstr "'let*' bindings must be an even number of elements"
        | [] -> Ok ()
      in
      let* () = bind_pairs binds in
      eval sub_env body
  | T.List (T.Symbol "do" :: body, _) ->
      T.LT.fold_m (fun _acc x -> eval env x) T.Nil body
  | T.List ([ T.Symbol "if"; cond; then_expr; else_expr ], _) -> (
      eval env cond
      >>= function
      | T.Nil | T.Bool false -> eval env else_expr
      | _ -> eval env then_expr)
  | T.List ([ T.Symbol "if"; cond; then_expr ], _) -> (
      eval env cond
      >>= function
      | T.Nil | T.Bool false -> Ok T.Nil
      | _ -> eval env then_expr)
  | T.List ([ T.Symbol "fn*"; T.List (binds, _); body ], _)
  | T.List ([ T.Symbol "fn*"; T.Vector (binds, _); body ], _) ->
      (fun exprs ->
        let sub_env = Env.make (Some env) in
        let rec bind_args = function
          | [ T.Symbol "&"; T.Symbol name ], args ->
              Env.set name (Types.list args) sub_env;
              Ok ()
          | T.Symbol name :: names, arg :: args ->
              Env.set name arg sub_env;
              bind_args (names, args)
          | [], [] -> Ok ()
          | _ ->
              T.errstr
                (sprintf "Expected %d args, got %d" (List.length binds)
                   (List.length exprs))
        in
        let* () = bind_args (binds, exprs) in
        eval sub_env body)
      |> Types.fn
      |> return
  | T.List (x :: xs, _) -> (
      eval env x
      >>= function
      | T.Fn (f, _) -> T.LT.map_m (eval env) xs >>= f
      | _ -> T.errstr (sprintf "'%s' is not callable" (T.to_string true x)))
  | T.Vector (xs, _) -> T.LT.map_m (eval env) xs >|= Types.vector
  | T.Map (xs, _) ->
      Types.MalMap.fold
        (fun k v acc ->
          let* acc = acc in
          (* let* k = eval env k in *)
          let* v = eval env v in
          return (Types.MalMap.add k v acc))
        xs (Ok Types.MalMap.empty)
      >|= Types.map
  | x -> Ok x

let print exp = T.to_string true exp
let repl_env = Core.ns

let rep str =
  read str
  |> Seq.map (function
    | Ok ast -> Result.(eval repl_env ast >|= print)
    | Error _ as err -> err)

let re str =
  read str
  |> T.ST.map_m (function
    | Ok ast -> eval repl_env ast
    | Error _ as err -> err)
  |> function
  | Ok _ -> ()
  | Error e ->
      printf "Error: %s\n%!" (T.to_string false e);
      exit 1

let () =
  Env.set "*ARGV*"
    (T.list
       (if Array.length Sys.argv > 1 then
          Sys.argv |> Array.to_list |> List.drop 2 |> List.map Types.string
        else []))
    repl_env;

  Env.set "eval"
    (Types.fn (function
      | [ ast ] -> eval repl_env ast
      | _ -> T.errstr "Invalid argument"))
    repl_env;

  re {|(def! not (fn* (a) (if a false true)))|};

  re
    {|(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))|};

  if Array.length Sys.argv > 1 then
    re (sprintf {|(load-file "%s")|} Sys.argv.(1)) |> ignore
  else
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
