open Printf
module T = Types.Types

let read str = Reader.read_str str

let rec eval env ast =
  let open Result in
  (match Env.get "DEBUG-EVAL" env with
  | None | Some T.Nil | Some (T.Bool false) -> ()
  | _ -> printf "EVAL: %s\n%!" (Printer.pr_str true ast));

  match ast with
  | T.Symbol x -> (
      match Env.get x env with
      | Some v -> Ok v
      | None -> Error (sprintf "'%s' not found" x))
  | T.List [ T.Symbol "def!"; T.Symbol key; expr ] ->
      let* value = eval env expr in
      Env.set key value env;
      Ok value
  | T.List [ T.Symbol "let*"; T.List binds; body ]
  | T.List [ T.Symbol "let*"; T.Vector binds; body ] ->
      let sub_env = Env.make (Some env) in
      let rec bind_pairs = function
        | T.Symbol key :: expr :: tail ->
            let* value = eval sub_env expr in
            Env.set key value sub_env;
            bind_pairs tail
        | _ :: _ :: _ -> Error "'let*' keys must be symbols"
        | _ :: [] -> Error "'let*' bindings must be an even number of elements"
        | [] -> Ok ()
      in
      let* () = bind_pairs binds in
      eval sub_env body
  | T.List (T.Symbol "do" :: body) ->
      Utils.ListTraverse.fold_m (fun _acc x -> eval env x) T.Nil body
  | T.List [ T.Symbol "if"; cond; then_expr; else_expr ] -> (
      eval env cond
      >>= function
      | T.Nil | T.Bool false -> eval env else_expr
      | _ -> eval env then_expr)
  | T.List [ T.Symbol "if"; cond; then_expr ] -> (
      eval env cond
      >>= function
      | T.Nil | T.Bool false -> Ok T.Nil
      | _ -> eval env then_expr)
  | T.List [ T.Symbol "fn*"; T.List binds; body ]
  | T.List [ T.Symbol "fn*"; T.Vector binds; body ] ->
      (fun exprs ->
        let sub_env = Env.make (Some env) in
        let rec bind_args = function
          | [ T.Symbol "&"; T.Symbol name ], args ->
              Env.set name (Types.List args) sub_env;
              Ok ()
          | T.Symbol name :: names, arg :: args ->
              Env.set name arg sub_env;
              bind_args (names, args)
          | [], [] -> Ok ()
          | _ ->
              Error
                (sprintf "Expected %d args, got %d" (List.length binds)
                   (List.length exprs))
        in
        let* () = bind_args (binds, exprs) in
        eval sub_env body)
      |> Types.fn
      |> return
  | T.List (x :: xs) -> (
      eval env x
      >>= function
      | T.Fn { value = f; _ } -> Utils.ListTraverse.map_m (eval env) xs >>= f
      | _ -> Error (sprintf "'%s' is not callable" (Printer.pr_str true x)))
  | T.Vector xs -> Utils.ListTraverse.map_m (eval env) xs >|= Types.vector
  | T.Map xs ->
      Types.MalMap.fold
        (fun k v acc ->
          let* acc = acc in
          (* let* k = eval env k in *)
          let* v = eval env v in
          return (Types.MalMap.add k v acc))
        xs (Ok Types.MalMap.empty)
      >|= Types.map
  | x -> Ok x

let print exp = Printer.pr_str true exp
let re str = Result.(str |> read >|= eval Core.ns >>= map_err Option.some)
let rep str = Result.(str |> re >|= print)

let () =
  Core.init Core.ns;

  Env.set "*ARGV*"
    (T.List
       (if Array.length Sys.argv > 1 then
          Sys.argv |> Array.to_list |> List.drop 2 |> List.map Types.string
        else []))
    Core.ns;

  Env.set "eval"
    (Types.fn (function
      | [ ast ] -> eval Core.ns ast
      | _ -> Error "Invalid argument"))
    Core.ns;

  re {|(def! not (fn* (a) (if a false true)))|} |> ignore;

  re
    {|(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))|}
  |> ignore;

  if Array.length Sys.argv > 1 then
    re (sprintf {|(load-file "%s")|} Sys.argv.(1)) |> ignore
  else
    try
      while true do
        printf "user> %!";
        let line = read_line () in
        match rep line with
        | Ok x -> printf "%s\n%!" x
        | Error None -> ()
        | Error (Some x) -> printf "Error: %s\n%!" x
      done
    with End_of_file -> print_newline ()
