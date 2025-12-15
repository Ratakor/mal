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
  (* By matching on 'def!' and 'let*' directly we allow to "override" their value *)
  | T.List [ T.Symbol "def!"; T.Symbol key; x ] ->
      let* value = eval env x in
      Env.set key value env;
      Ok value
  | T.List [ T.Symbol "let*"; T.List bindings; body ]
  | T.List [ T.Symbol "let*"; T.Vector bindings; body ] ->
      let sub_env = Env.make (Some env) in
      let rec bind_pairs = function
        | T.Symbol key :: x :: tail ->
            let* value = eval sub_env x in
            Env.set key value sub_env;
            bind_pairs tail
        | _ :: _ :: _ -> Error "'let*' keys must be symbols"
        | _ :: [] -> Error "'let*' bindings must be an even number of elements"
        | [] -> Ok ()
      in
      let* () = bind_pairs bindings in
      eval sub_env body
  | T.List (x :: xs) -> (
      match eval env x with
      | Ok (T.Fn f) -> Utils.ListTraverse.map_m (eval env) xs >>= f.value
      | Ok _ -> Error (sprintf "'%s' is not callable" (Printer.pr_str true x))
      | Error e -> Error e)
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

let repl_env =
  let int_fn f =
    Types.fn (function
      | [ T.Int a; T.Int b ] -> Ok (T.Int (f a b))
      | _ -> Error "Invalid argument")
  in
  let env = Env.make None in
  Env.set "+" (int_fn ( + )) env;
  Env.set "-" (int_fn ( - )) env;
  Env.set "*" (int_fn ( * )) env;
  Env.set "/" (int_fn ( / )) env;
  Env.set "DEBUG-EVAL" (T.Bool false) env;
  env

let rep str =
  Result.(str |> read >|= eval repl_env >>= map_err Option.some >|= print)

let () =
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
