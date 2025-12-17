open Printf
module T = Types.Types

let rec quasiquote = function
  | T.List [ T.Symbol "unquote"; x ] -> x
  | T.List xs -> qq_list xs
  | T.Vector xs -> T.List [ T.Symbol "vec"; qq_list xs ]
  | (T.Map _ | T.Symbol _) as ast -> T.List [ T.Symbol "quote"; ast ]
  | ast -> ast

and qq_list xs =
  List.fold_left
    (fun acc elt ->
      match elt with
      | T.List [ T.Symbol "splice-unquote"; x ] ->
          T.List [ T.Symbol "concat"; x; acc ]
      | _ -> T.List [ T.Symbol "cons"; quasiquote elt; acc ])
    (T.List []) (List.rev xs)

let rec eval env ast =
  let open Result in
  (match Env.get "DEBUG-EVAL" env with
  | None | Some T.Nil | Some (T.Bool false) -> ()
  | _ -> printf "EVAL: %s\n%!" (Types.to_string true ast));

  match ast with
  | T.Symbol x -> (
      match Env.get x env with
      | Some v -> Ok v
      | None -> Types.errstr (sprintf "'%s' not found" x))
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
        | _ :: _ :: _ -> Types.errstr "'let*' keys must be symbols"
        | _ :: [] ->
            Types.errstr "'let*' bindings must be an even number of elements"
        | [] -> Ok ()
      in
      let* () = bind_pairs binds in
      eval sub_env body
  | T.List (T.Symbol "do" :: body) ->
      Types.Traverse.fold_m (fun _acc x -> eval env x) T.Nil body
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
              Types.errstr
                (sprintf "Expected %d args, got %d" (List.length binds)
                   (List.length exprs))
        in
        let* () = bind_args (binds, exprs) in
        eval sub_env body)
      |> Types.fn
      |> return
  | T.List [ T.Symbol "quote"; ast ] -> Ok ast
  | T.List [ T.Symbol "quasiquote"; ast ] -> eval env (quasiquote ast)
  | T.List [ T.Symbol "defmacro!"; T.Symbol key; expr ] -> (
      eval env expr
      >>= function
      | T.Fn fn ->
          let fn = T.Fn { fn with is_macro = true } in
          Env.set key fn env;
          Ok fn
      | _ -> Types.errstr "'defmacro!' value must be a function")
  | T.List [ T.Symbol "try*"; expr ] -> eval env expr
  | T.List
      [
        T.Symbol "try*";
        try_expr;
        T.List [ T.Symbol "catch*"; T.Symbol bind; catch_expr ];
      ] -> (
      match eval env try_expr with
      | Ok _ as ok -> ok
      | Error err ->
          let sub_env = Env.make (Some env) in
          Env.set bind err sub_env;
          eval sub_env catch_expr)
  | T.List (x :: xs) -> (
      eval env x
      >>= function
      | T.Fn { value = f; is_macro = true } -> f xs >>= eval env
      | T.Fn { value = f; _ } -> Types.Traverse.map_m (eval env) xs >>= f
      | _ ->
          Types.errstr (sprintf "'%s' is not callable" (Types.to_string true x))
      )
  | T.Vector xs -> Types.Traverse.map_m (eval env) xs >|= Types.vector
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

let read str = Reader.read_str str
let print exp = Types.to_string true exp
let re str = Result.(str |> read >>= eval Core.ns)
let rep str = Result.(str |> re >|= print)

let () =
  Core.init Core.ns;

  Env.set "*host-language*" (T.String "OCaml") Core.ns;

  Env.set "*ARGV*"
    (T.List
       (if Array.length Sys.argv > 1 then
          Sys.argv |> Array.to_list |> List.drop 2 |> List.map Types.string
        else []))
    Core.ns;

  Env.set "eval"
    (Types.fn (function
      | [ ast ] -> eval Core.ns ast
      | xs -> Core.invalid_num_args "eval" xs))
    Core.ns;

  re "(def! not (fn* (a) (if a false true)))" |> ignore;

  re
    "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\n\
     nil)\")))))"
  |> ignore;

  re
    "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if \
     (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) \
     (cons 'cond (rest (rest xs)))))))"
  |> ignore;

  if Array.length Sys.argv > 1 then
    match re (sprintf {|(load-file "%s")|} Sys.argv.(1)) with
    | Ok _ | Error T.Nil -> ()
    | Error x -> printf "Error: %s\n%!" (Types.to_string false x)
  else
    try
      re "(println (str \"Mal [\" *host-language* \"]\" ))" |> ignore;
      while true do
        printf "user> %!";
        let line = read_line () in
        match rep line with
        | Ok x -> printf "%s\n%!" x
        | Error T.Nil -> ()
        | Error x -> printf "Error: %s\n%!" (Types.to_string false x)
      done
    with End_of_file -> print_newline ()
