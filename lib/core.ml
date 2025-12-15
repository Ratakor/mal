module T = Types.Types

let ns = Env.make None
let invalid_arg fn_name = Error ("Invalid argument given to " ^ fn_name)

let equal = function
  | [ a; b ] -> Ok (T.Bool (Types.equal a b))
  | _ -> invalid_arg __FUNCTION__

let arith_binary f = function
  | [ T.Int a; T.Int b ] -> Ok (T.Int (f a b))
  | _ -> invalid_arg __FUNCTION__

let int_cmp_binary f = function
  | [ T.Int a; T.Int b ] -> Ok (T.Bool (f a b))
  | _ -> invalid_arg __FUNCTION__

let is_list = function
  | [ T.List _ ] -> Ok (T.Bool true)
  | _ -> Ok (T.Bool false)

let is_empty = function
  | [ T.List [] ] | [ T.Vector [] ] -> Ok (T.Bool true)
  | [ T.List _ ] | [ T.Vector _ ] -> Ok (T.Bool false)
  | _ -> invalid_arg __FUNCTION__

let count = function
  | [ T.List xs ] | [ T.Vector xs ] -> Ok (T.Int (List.length xs))
  (* | [ T.Nil ] -> Ok (T.Int 0) *)
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
      | Error None -> Ok T.Nil
      | Error (Some x) -> Error x)
  | _ -> invalid_arg __FUNCTION__

let slurp = function
  | [ T.String filename ] ->
      let open Result in
      (try Ok IO.(with_in filename read_all) with e -> of_exn e)
      >>= Fun.(Types.string %> return)
  | _ -> invalid_arg __FUNCTION__

let init env =
  let set s f = Env.set s (T.Fn f) env in

  set "=" equal;

  set "<" (int_cmp_binary Int.( < ));
  set "<=" (int_cmp_binary Int.( <= ));
  set ">" (int_cmp_binary Int.( > ));
  set ">=" (int_cmp_binary Int.( >= ));

  set "+" (arith_binary Int.( + ));
  set "-" (arith_binary Int.( - ));
  set "*" (arith_binary Int.( * ));
  set "/" (arith_binary Int.( / ));

  set "list" Fun.(Types.list %> Result.return);

  set "list?" is_list;

  set "empty?" is_empty;
  set "count" count;

  set "pr-str" pr_str;
  set "str" str;
  set "prn" prn;
  set "println" println;

  set "read-string" read_string;
  set "slurp" slurp
