module T = Types.Types

let ns = Env.make None
let invalid_arg fn_name = Types.errstr (fn_name ^ ": Invalid argument")

let equal = function
  | [ a; b ] -> Ok (T.Bool (Types.equal a b))
  | _ -> invalid_arg __FUNCTION__

let int_arith_fold f = function
  | T.Int x :: xs ->
      let open Result in
      Types.Traverse.fold_m
        (fun acc -> function
          | T.Int a -> Ok (f acc a)
          | _ -> invalid_arg __FUNCTION__)
        x xs
      >|= Types.int
  | _ -> invalid_arg __FUNCTION__

let int_cmp_binary f = function
  | [ T.Int a; T.Int b ] -> Ok (T.Bool (f a b))
  | _ -> invalid_arg __FUNCTION__

let div = function
  | T.Int x :: xs ->
      let open Result in
      Types.Traverse.fold_m
        (fun acc -> function
          | T.Int a -> (
              try Ok (acc / a) with _ -> Types.errstr "Division by zero")
          | _ -> invalid_arg __FUNCTION__)
        x xs
      >|= Types.int
  | _ -> invalid_arg __FUNCTION__

let atom = function
  | [ x ] -> Ok (Types.atom x)
  | _ -> invalid_arg __FUNCTION__

let vec = function
  | [ T.List xs ] | [ T.Vector xs ] -> Ok (T.Vector xs)
  | _ -> invalid_arg __FUNCTION__

let is_list = function
  | [ T.List _ ] -> Ok (T.Bool true)
  | _ -> Ok (T.Bool false)

let is_atom = function
  | [ T.Atom _ ] -> Ok (T.Bool true)
  | _ -> Ok (T.Bool false)

let is_empty = function
  | [ T.List [] ] | [ T.Vector [] ] -> Ok (T.Bool true)
  | [ T.List _ ] | [ T.Vector _ ] -> Ok (T.Bool false)
  | _ -> invalid_arg __FUNCTION__

let count = function
  | [ T.List xs ] | [ T.Vector xs ] -> Ok (T.Int (List.length xs))
  | [ T.Nil ] -> Ok (T.Int 0)
  | _ -> invalid_arg __FUNCTION__

let cons = function
  | [ x; T.List xs ] | [ x; T.Vector xs ] -> Ok (T.List (x :: xs))
  | _ -> invalid_arg __FUNCTION__

let concat arg =
  let open Result in
  let rec aux = function
    | [] -> Ok []
    | T.List x :: xs | T.Vector x :: xs ->
        let* xs = aux xs in
        Ok (x @ xs)
    | _ -> invalid_arg __FUNCTION__
  in
  aux arg >|= Types.list

let nth = function
  | [ T.List xs; T.Int i ] | [ T.Vector xs; T.Int i ] -> (
      try Ok (List.nth xs i) with _ -> Types.errstr "nth: index out of range")
  | _ -> invalid_arg __FUNCTION__

let first = function
  | [ T.List xs ] | [ T.Vector xs ] -> (
      match xs with
      | [] -> Ok T.Nil
      | x :: _ -> Ok x)
  | [ T.Nil ] -> Ok T.Nil
  | _ -> invalid_arg __FUNCTION__

let rest = function
  | [ T.List xs ] | [ T.Vector xs ] -> (
      match xs with
      | [] -> Ok (T.List [])
      | _ :: xs -> Ok (T.List xs))
  | [ T.Nil ] -> Ok (T.List [])
  | _ -> invalid_arg __FUNCTION__

let deref = function
  | [ T.Atom x ] -> Ok !x
  | _ -> invalid_arg __FUNCTION__

let reset = function
  | [ T.Atom x; v ] ->
      x := v;
      Ok v
  | _ -> invalid_arg __FUNCTION__

let swap = function
  | T.Atom x :: T.Fn { value = f; _ } :: args ->
      let open Result in
      let* v = f (!x :: args) in
      x := v;
      Ok v
  | _ -> invalid_arg __FUNCTION__

let throw = function
  | [ x ] -> Error x
  | _ -> invalid_arg __FUNCTION__

let pr_str_list sep readably xs =
  String.concat sep (List.map (Printer.pr_str readably) xs)

let pr_str xs = Ok (T.String (pr_str_list " " true xs))
let str xs = Ok (T.String (pr_str_list "" false xs))

let prn xs =
  print_endline (pr_str_list " " true xs);
  Ok T.Nil

let println xs =
  print_endline (pr_str_list " " false xs);
  Ok T.Nil

let read_string = function
  | [ T.String s ] -> (
      Reader.read_str s
      |> function
      | Ok _ as ok -> ok
      | Error T.Nil -> Ok T.Nil
      | Error x -> Types.errstr (Printer.pr_str false x))
  | _ -> invalid_arg __FUNCTION__

let slurp = function
  | [ T.String filename ] -> (
      try Ok (T.String IO.(with_in filename read_all))
      with e -> Types.errstr (Printexc.to_string e))
  | _ -> invalid_arg __FUNCTION__

let init env =
  let set s f = Env.set s (Types.fn f) env in

  set "=" equal;

  set "<" (int_cmp_binary Int.( < ));
  set "<=" (int_cmp_binary Int.( <= ));
  set ">" (int_cmp_binary Int.( > ));
  set ">=" (int_cmp_binary Int.( >= ));

  set "+" (int_arith_fold Int.( + ));
  set "-" (int_arith_fold Int.( - ));
  set "*" (int_arith_fold Int.( * ));
  set "/" div;

  set "list" Fun.(Types.list %> Result.return);
  set "atom" atom;
  set "vec" vec;

  set "list?" is_list;
  set "atom?" is_atom;

  set "empty?" is_empty;
  set "count" count;
  set "cons" cons;
  set "concat" concat;
  set "nth" nth;
  set "first" first;
  set "rest" rest;

  set "deref" deref;
  set "reset!" reset;
  set "swap!" swap;

  set "throw" throw;

  set "pr-str" pr_str;
  set "str" str;
  set "prn" prn;
  set "println" println;

  set "read-string" read_string;
  set "slurp" slurp
