open Printf
module T = Types

let rec quasiquote = function
  | T.List ([ T.Symbol "unquote"; x ], _) -> x
  | T.List (xs, _) -> qq_list xs
  | T.Vector (xs, _) -> T.list [ T.symbol "vec"; qq_list xs ]
  | (T.Map _ | T.Symbol _) as ast -> T.list [ T.symbol "quote"; ast ]
  | ast -> ast

and qq_list xs =
  List.fold_left
    (fun acc elt ->
      match elt with
      | T.List ([ T.Symbol "splice-unquote"; x ], _) ->
          T.list [ T.symbol "concat"; x; acc ]
      | _ -> T.list [ T.symbol "cons"; quasiquote elt; acc ])
    (T.list []) (List.rev xs)

let rec eval env ast =
  let open Result in
  (match Env.get "DEBUG-EVAL" env with
  | None | Some T.Nil | Some (T.Bool false) -> ()
  | _ -> printf "EVAL: %s\n%!" (T.to_string true ast));

  match ast with
  | T.Symbol x -> (
      match Env.get x env with
      | Some v -> Ok v
      | None -> T.errstr ("Unable to resolve symbol: " ^ x))
  | T.Vector (xs, _) -> T.LT.map_m (eval env) xs >|= T.vector
  | T.Map (xs, _) ->
      T.MalMap.fold
        (fun k v acc ->
          let* acc = acc in
          (* let* k = eval env k in *)
          let* v = eval env v in
          Ok (T.MalMap.add k v acc))
        xs (Ok T.MalMap.empty)
      >|= T.map
  | T.List ([ T.Symbol "def!"; T.Symbol key; expr ], _) ->
      let* value = eval env expr in
      Env.set key value env;
      Ok value
  | T.List ([ T.Symbol "let*"; T.List (binds, _); expr ], _)
  | T.List ([ T.Symbol "let*"; T.Vector (binds, _); expr ], _) ->
      let sub_env = Env.make (Some env) in
      let rec bind_pairs = function
        | T.Symbol key :: value :: xs ->
            let* value = eval sub_env value in
            Env.set key value sub_env;
            bind_pairs xs
        | _ :: _ :: _ -> T.errstr "let* keys must be symbols"
        | _ :: [] -> T.errstr "let* bindings must be an even number of elements"
        | [] -> Ok ()
      in
      let* () = bind_pairs binds in
      eval sub_env expr
  | T.List (T.Symbol "do" :: xs, _) ->
      T.LT.fold_m (fun _acc x -> eval env x) T.nil xs
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
  | T.List ([ T.Symbol "fn*"; T.List (binds, _); expr ], _)
  | T.List ([ T.Symbol "fn*"; T.Vector (binds, _); expr ], _) ->
      T.fn' (fun args ->
          let sub_env = Env.make (Some env) in
          let rec bind_args = function
            | [ T.Symbol "&"; T.Symbol name ], args ->
                Env.set name (T.list args) sub_env;
                Ok ()
            | T.Symbol name :: names, arg :: args ->
                Env.set name arg sub_env;
                bind_args (names, args)
            | [], [] -> Ok ()
            | _ ->
                T.errstr
                  (sprintf "Expected %d args, got %d" (List.length binds)
                     (List.length args))
          in
          let* () = bind_args (binds, args) in
          eval sub_env expr)
  | T.List ([ T.Symbol "quote"; ast ], _) -> Ok ast
  | T.List ([ T.Symbol "quasiquote"; ast ], _) -> eval env (quasiquote ast)
  | T.List ([ T.Symbol "defmacro!"; T.Symbol key; expr ], _) -> (
      eval env expr
      >>= function
      | T.Fn fn ->
          let* fn = T.macro fn in
          Env.set key fn env;
          Ok fn
      | _ -> T.errstr "'defmacro!' value must be a function")
  | T.List ([ T.Symbol "try*"; expr ], _) -> eval env expr
  | T.List
      ( [
          T.Symbol "try*";
          try_expr;
          T.List ([ T.Symbol "catch*"; T.Symbol key; catch_expr ], _);
        ],
        _ ) -> (
      match eval env try_expr with
      | Ok _ as ok -> ok
      | Error err ->
          let sub_env = Env.make (Some env) in
          Env.set key err sub_env;
          eval sub_env catch_expr)
  | T.List (x :: xs, _) -> (
      eval env x
      >>= function
      | T.Fn ((f, _) as fn) when T.is_macro fn -> f xs >>= eval env
      | T.Fn (f, _) -> T.LT.map_m (eval env) xs >>= f
      | _ -> T.errstr (sprintf "'%s' is not callable" (T.to_string true x)))
  | x -> Ok x

let read str = Reader.read_str str
let print exp = T.to_string true exp
let repl_env = Core.ns

(* eval all forms (lazy) *)
let rep str =
  read str
  |> Seq.map (function
    | Ok ast -> Result.(eval repl_env ast >|= print)
    | Error _ as err -> err)

(* exit on first error (eager) *)
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
  Env.set "eval"
    (T.fn (function
      | [ ast ] -> eval repl_env ast
      | xs -> Core.invalid_num_args "eval" xs))
    repl_env;
  Env.set "*ARGV*"
    (if Array.length Sys.argv > 1 then
       Sys.argv |> Array.to_list |> List.drop 2 |> List.map T.string |> T.list
     else T.list [])
    repl_env;

  re {|(def! *host-language* "OCaml")|};
  re "(def! not (fn* (a) (if a false true)))";
  re
    "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\n\
     nil)\")))))";
  re
    "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if \
     (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) \
     (cons 'cond (rest (rest xs)))))))";

  if Array.length Sys.argv > 1 then
    re (sprintf {|(load-file "%s")|} Sys.argv.(1))
  else
    try
      re {|(println (str "Mal [" *host-language* "]" ))|};
      while true do
        printf "user> %!";
        read_line ()
        |> rep
        |> Seq.iter (function
          | Ok x -> printf "%s\n%!" x
          | Error x -> printf "Error: %s\n%!" (T.to_string false x))
      done
    with End_of_file -> print_newline ()
