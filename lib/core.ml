module T = Types.Types

let ns = Env.make None

let invalid_num_args fn args =
  Printf.sprintf "Wrong number of args (%d) passed to: %s" (List.length args) fn
  |> Types.errstr

let invalid_arg fn = Types.errstr ("Invalid argument passed to: " ^ fn)

let equal self = function
  | [ a; b ] -> Ok (T.Bool (Types.equal a b))
  | xs -> invalid_num_args self xs

let int_cmp_binary f self = function
  | [ T.Int a; T.Int b ] -> Ok (T.Bool (f a b))
  | [ _; _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let int_arith_fold f self = function
  | T.Int x :: xs ->
      let open Result in
      Types.Traverse.fold_m
        (fun acc -> function
          | T.Int a -> Ok (f acc a)
          | _ -> invalid_arg self)
        x xs
      >|= Types.int
  | [] -> invalid_num_args self []
  | _ -> invalid_arg self

let div self = function
  | T.Int x :: xs ->
      let open Result in
      Types.Traverse.fold_m
        (fun acc -> function
          | T.Int a -> (
              try Ok Int.(acc / a) with _ -> Types.errstr "Division by zero")
          | _ -> invalid_arg self)
        x xs
      >|= Types.int
  | [] -> invalid_num_args self []
  | _ -> invalid_arg self

let list _ = Fun.(Types.list %> Result.return)
let vector _ = Fun.(Types.vector %> Result.return)

let atom self = function
  | [ x ] -> Ok (Types.atom x)
  | xs -> invalid_num_args self xs

let vec self = function
  | [ T.List xs ] | [ T.Vector xs ] -> Ok (T.Vector xs)
  | [ _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let symbol self = function
  | [ T.String x ] -> Ok (T.Symbol x)
  | [ _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let keyword self = function
  | [ T.String x ] | [ T.Keyword x ] -> Ok (T.Keyword x)
  | [ _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let hash_map _ = Types.map_of_list Types.MalMap.empty

let is_list self = function
  | [ T.List _ ] -> Ok (T.Bool true)
  | [ _ ] -> Ok (T.Bool false)
  | xs -> invalid_num_args self xs

let is_atom self = function
  | [ T.Atom _ ] -> Ok (T.Bool true)
  | [ _ ] -> Ok (T.Bool false)
  | xs -> invalid_num_args self xs

let is_nil self = function
  | [ T.Nil ] -> Ok (T.Bool true)
  | [ _ ] -> Ok (T.Bool false)
  | xs -> invalid_num_args self xs

let is_true self = function
  | [ T.Bool true ] -> Ok (T.Bool true)
  | [ _ ] -> Ok (T.Bool false)
  | xs -> invalid_num_args self xs

let is_false self = function
  | [ T.Bool false ] -> Ok (T.Bool true)
  | [ _ ] -> Ok (T.Bool false)
  | xs -> invalid_num_args self xs

let is_symbol self = function
  | [ T.Symbol _ ] -> Ok (T.Bool true)
  | [ _ ] -> Ok (T.Bool false)
  | xs -> invalid_num_args self xs

let is_keyword self = function
  | [ T.Keyword _ ] -> Ok (T.Bool true)
  | [ _ ] -> Ok (T.Bool false)
  | xs -> invalid_num_args self xs

let is_vector self = function
  | [ T.Vector _ ] -> Ok (T.Bool true)
  | [ _ ] -> Ok (T.Bool false)
  | xs -> invalid_num_args self xs

let is_map self = function
  | [ T.Map _ ] -> Ok (T.Bool true)
  | [ _ ] -> Ok (T.Bool false)
  | xs -> invalid_num_args self xs

let is_sequential self = function
  | [ T.List _ ] | [ T.Vector _ ] -> Ok (T.Bool true)
  | [ _ ] -> Ok (T.Bool false)
  | xs -> invalid_num_args self xs

let is_empty self = function
  | [ T.List [] ] | [ T.Vector [] ] -> Ok (T.Bool true)
  | [ T.List _ ] | [ T.Vector _ ] -> Ok (T.Bool false)
  | [ _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let count self = function
  | [ T.List xs ] | [ T.Vector xs ] -> Ok (T.Int (List.length xs))
  | [ T.Nil ] -> Ok (T.Int 0)
  | [ _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let cons self = function
  | [ x; T.List xs ] | [ x; T.Vector xs ] -> Ok (T.List (x :: xs))
  | [ _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let concat self arg =
  let open Result in
  Types.Traverse.fold_m
    (fun acc -> function
      | T.List x | T.Vector x -> Ok (acc @ x)
      | _ -> invalid_arg self)
    [] arg
  >|= Types.list

let nth self = function
  | [ T.List xs; T.Int i ] | [ T.Vector xs; T.Int i ] -> (
      try Ok (List.nth xs i)
      with _ -> Types.errstr (self ^ ": index out of range"))
  | [ _; _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let first self = function
  | [ T.List xs ] | [ T.Vector xs ] -> (
      match xs with
      | [] -> Ok T.Nil
      | x :: _ -> Ok x)
  | [ T.Nil ] -> Ok T.Nil
  | [ _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let rest self = function
  | [ T.List xs ] | [ T.Vector xs ] -> (
      match xs with
      | [] -> Ok (T.List [])
      | _ :: xs -> Ok (T.List xs))
  | [ T.Nil ] -> Ok (T.List [])
  | [ _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let apply self = function
  | ([] | T.Fn _ :: []) as xs -> invalid_num_args self xs
  | T.Fn { value = f; _ } :: xs ->
      let[@tail_mod_cons] rec aux = function
        | [] -> []
        | [ T.List x ] | [ T.Vector x ] -> x
        | x :: xs -> x :: aux xs
      in
      f (aux xs)
  | _ -> invalid_arg self

let map self = function
  | [ T.Fn { value = f; _ }; T.List xs ]
  | [ T.Fn { value = f; _ }; T.Vector xs ] ->
      Result.(Types.Traverse.map_m Fun.(List.pure %> f) xs >|= Types.list)
  | [ _; _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let assoc self = function
  | T.Map m :: xs -> Types.map_of_list m xs
  | _ :: _ -> invalid_arg self
  | [] -> invalid_num_args self []

let dissoc self = function
  | T.Map m :: xs ->
      Ok (T.Map (List.fold_left (Fun.flip Types.MalMap.remove) m xs))
  | _ :: _ -> invalid_arg self
  | [] -> invalid_num_args self []

let get self = function
  | [ T.Map m; k ] -> Ok (Types.MalMap.get_or k m ~default:Nil)
  | [ _; _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let contains self = function
  | [ T.Map m; k ] ->
      Ok (Types.MalMap.find_opt k m |> Option.is_some |> Types.bool)
  | [ _; _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let keys self = function
  | [ T.Map m ] ->
      Ok (T.List (Types.MalMap.fold (fun k _ acc -> k :: acc) m []))
  | [ _ ] -> invalid_arg self
  | _ -> invalid_num_args self []

let vals self = function
  | [ T.Map m ] ->
      Ok (T.List (Types.MalMap.fold (fun _ v acc -> v :: acc) m []))
  | [ _ ] -> invalid_arg self
  | _ -> invalid_num_args self []

let deref self = function
  | [ T.Atom x ] -> Ok !x
  | [ _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let reset self = function
  | [ T.Atom x; v ] ->
      x := v;
      Ok v
  | [ _; _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let swap self = function
  | T.Atom x :: T.Fn { value = f; _ } :: args ->
      let open Result in
      let* v = f (!x :: args) in
      x := v;
      Ok v
  | ([] | [ _ ]) as xs -> invalid_num_args self xs
  | _ -> invalid_arg self

let throw self = function
  | [ x ] -> Error x
  | xs -> invalid_num_args self xs

let pr_str_list sep readably xs =
  String.concat sep (List.map (Printer.pr_str readably) xs)

let pr_str _ xs = Ok (T.String (pr_str_list " " true xs))
let str _ xs = Ok (T.String (pr_str_list "" false xs))

let prn _ xs =
  print_endline (pr_str_list " " true xs);
  Ok T.Nil

let println _ xs =
  print_endline (pr_str_list " " false xs);
  Ok T.Nil

let read_string self = function
  | [ T.String s ] -> (
      Reader.read_str s
      |> function
      | Ok _ as ok -> ok
      | Error T.Nil -> Ok T.Nil
      | Error x -> Types.errstr (self ^ ": " ^ Printer.pr_str false x))
  | [ _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let slurp self = function
  | [ T.String filename ] -> (
      try Ok (T.String IO.(with_in filename read_all))
      with e -> Types.errstr (self ^ ": " ^ Printexc.to_string e))
  | [ _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let init env =
  let set s f = Env.set s (Types.fn (f ("Core." ^ s))) env in

  set "=" equal;

  set "<" (int_cmp_binary Int.( < ));
  set "<=" (int_cmp_binary Int.( <= ));
  set ">" (int_cmp_binary Int.( > ));
  set ">=" (int_cmp_binary Int.( >= ));

  set "+" (int_arith_fold Int.( + ));
  set "-" (int_arith_fold Int.( - ));
  set "*" (int_arith_fold Int.( * ));
  set "/" div;

  set "list" list;
  set "vector" vector;
  set "atom" atom;
  set "vec" vec;
  set "symbol" symbol;
  set "keyword" keyword;
  set "hash-map" hash_map;

  set "list?" is_list;
  set "atom?" is_atom;
  set "nil?" is_nil;
  set "true?" is_true;
  set "false?" is_false;
  set "symbol?" is_symbol;
  set "keyword?" is_keyword;
  set "vector?" is_vector;
  set "map?" is_map;
  set "sequential?" is_sequential;

  set "empty?" is_empty;
  set "count" count;
  set "cons" cons;
  set "concat" concat;
  set "nth" nth;
  set "first" first;
  set "rest" rest;
  set "apply" apply;
  set "map" map;

  set "assoc" assoc;
  set "dissoc" dissoc;
  set "get" get;
  set "contains?" contains;
  set "keys" keys;
  set "vals" vals;

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
