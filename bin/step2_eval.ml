open Printf
module T = Types

module ResultMonad = struct
  type 'a t = ('a, string) result

  let return = Result.return
  let ( >>= ) = Result.( >>= )
end

module TraverseM = List.Traverse (ResultMonad)

let repl_env =
  let int_fn f =
    T.Fn
      (function
      | [ T.Int a; T.Int b ] -> Ok (T.Int (f a b))
      | _ -> Error "Invalid argument")
  in
  let env = Env.make None in
  Env.set "+" (int_fn ( + )) env;
  Env.set "-" (int_fn ( - )) env;
  Env.set "*" (int_fn ( * )) env;
  Env.set "/" (int_fn ( / )) env;
  env

let read str = Reader.read_str str

let rec eval env ast =
  match ast with
  | T.Symbol x -> (
      match Env.get x env with
      | Some v -> Ok v
      | None -> Error ("Symbol not found: " ^ x))
  | T.List (x :: xs) -> (
      match eval env x with
      | Ok (T.Fn f) -> Result.(TraverseM.map_m (eval env) xs >>= f)
      | Ok _ -> Error "Invalid argument"
      | Error e -> Error e)
  | _ -> Ok ast

let print exp = Printer.pr_str true exp

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
