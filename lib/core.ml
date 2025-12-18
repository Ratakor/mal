module T = Types

let invalid_num_args fn args =
  Printf.sprintf "Wrong number of args (%d) passed to: %s" (List.length args) fn
  |> T.errstr

let invalid_arg fn = T.errstr ("Invalid argument passed to: " ^ fn)

let equal self = function
  | [ a; b ] -> T.bool' (T.equal a b)
  | xs -> invalid_num_args self xs

let int_cmp_binary f self = function
  | [ T.Int a; T.Int b ] -> T.bool' (f a b)
  | [ _; _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let int_arith_fold f self = function
  | T.Int x :: xs ->
      let open Result in
      T.LT.fold_m
        (fun acc -> function
          | T.Int a -> Ok (f acc a)
          | _ -> invalid_arg self)
        x xs
      >|= T.int
  | [] -> invalid_num_args self []
  | _ -> invalid_arg self

let div self = function
  | T.Int x :: xs ->
      let open Result in
      T.LT.fold_m
        (fun acc -> function
          | T.Int a -> (
              try Ok Int.(acc / a) with _ -> T.errstr "Division by zero")
          | _ -> invalid_arg self)
        x xs
      >|= T.int
  | [] -> invalid_num_args self []
  | _ -> invalid_arg self

let list _ arg = T.list' arg
let vector _ arg = T.vector' arg

let atom self = function
  | [ x ] -> T.atom' x
  | xs -> invalid_num_args self xs

let vec self = function
  | [ T.List (xs, _) ] | [ T.Vector (xs, _) ] -> T.vector' xs
  | [ _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let symbol self = function
  | [ T.String x ] -> T.symbol' x
  | [ _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let keyword self = function
  | [ T.String x ] | [ T.Keyword x ] -> T.keyword' x
  | [ _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let hash_map _ = T.map_of_list T.MalMap.empty

let is_list self = function
  | [ T.List _ ] -> T.maltrue'
  | [ _ ] -> T.malfalse'
  | xs -> invalid_num_args self xs

let is_atom self = function
  | [ T.Atom _ ] -> T.maltrue'
  | [ _ ] -> T.malfalse'
  | xs -> invalid_num_args self xs

let is_nil self = function
  | [ T.Nil ] -> T.maltrue'
  | [ _ ] -> T.malfalse'
  | xs -> invalid_num_args self xs

let is_true self = function
  | [ T.Bool true ] -> T.maltrue'
  | [ _ ] -> T.malfalse'
  | xs -> invalid_num_args self xs

let is_false self = function
  | [ T.Bool false ] -> T.maltrue'
  | [ _ ] -> T.malfalse'
  | xs -> invalid_num_args self xs

let is_symbol self = function
  | [ T.Symbol _ ] -> T.maltrue'
  | [ _ ] -> T.malfalse'
  | xs -> invalid_num_args self xs

let is_keyword self = function
  | [ T.Keyword _ ] -> T.maltrue'
  | [ _ ] -> T.malfalse'
  | xs -> invalid_num_args self xs

let is_vector self = function
  | [ T.Vector _ ] -> T.maltrue'
  | [ _ ] -> T.malfalse'
  | xs -> invalid_num_args self xs

let is_map self = function
  | [ T.Map _ ] -> T.maltrue'
  | [ _ ] -> T.malfalse'
  | xs -> invalid_num_args self xs

let is_sequential self = function
  | [ T.List _ ] | [ T.Vector _ ] -> T.maltrue'
  | [ _ ] -> T.malfalse'
  | xs -> invalid_num_args self xs

let is_string self = function
  | [ T.String _ ] -> T.maltrue'
  | [ _ ] -> T.malfalse'
  | xs -> invalid_num_args self xs

let is_number self = function
  | [ T.Int _ ] | [ T.Float _ ] -> T.maltrue'
  | [ _ ] -> T.malfalse'
  | xs -> invalid_num_args self xs

let is_char self = function
  | [ T.Char _ ] -> T.maltrue'
  | [ _ ] -> T.malfalse'
  | xs -> invalid_num_args self xs

let is_int self = function
  | [ T.Int _ ] -> T.maltrue'
  | [ _ ] -> T.malfalse'
  | xs -> invalid_num_args self xs

let is_float self = function
  | [ T.Float _ ] -> T.maltrue'
  | [ _ ] -> T.malfalse'
  | xs -> invalid_num_args self xs

let is_seq self = function
  | [ T.Seq _ ] -> T.maltrue'
  | [ _ ] -> T.malfalse'
  | xs -> invalid_num_args self xs

let is_fn self = function
  | [ T.Fn fn ] when not (T.is_macro fn) -> T.maltrue'
  | [ _ ] -> T.malfalse'
  | xs -> invalid_num_args self xs

let is_macro self = function
  | [ T.Fn fn ] when T.is_macro fn -> T.maltrue'
  | [ _ ] -> T.malfalse'
  | xs -> invalid_num_args self xs

let is_empty self = function
  | [ T.List ([], _) ] | [ T.Vector ([], _) ] -> T.maltrue'
  | [ T.List _ ] | [ T.Vector _ ] -> T.malfalse'
  | [ _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let count self = function
  | [ T.List (xs, _) ] | [ T.Vector (xs, _) ] -> T.int' (List.length xs)
  | [ T.Nil ] -> T.int' 0
  | [ _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let cons self = function
  | [ x; T.List (xs, _) ] | [ x; T.Vector (xs, _) ] -> T.list' (x :: xs)
  | [ _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let concat self arg =
  let open Result in
  T.LT.fold_m
    (fun acc -> function
      | T.List (x, _) | T.Vector (x, _) -> Ok (acc @ x)
      | _ -> invalid_arg self)
    [] arg
  >|= T.list

let nth self = function
  | [ T.List (xs, _); T.Int i ] | [ T.Vector (xs, _); T.Int i ] -> (
      try Ok (List.nth xs i) with _ -> T.errstr (self ^ ": index out of range"))
  | [ _; _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let first self = function
  | [ T.List (xs, _) ] | [ T.Vector (xs, _) ] -> (
      match xs with
      | [] -> T.nil'
      | x :: _ -> Ok x)
  | [ T.Nil ] -> T.nil'
  | [ _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let rest self = function
  | [ T.List (xs, _) ] | [ T.Vector (xs, _) ] -> (
      match xs with
      | [] -> T.list' []
      | _ :: xs -> T.list' xs)
  | [ T.Nil ] -> T.list' []
  | [ _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let apply self = function
  | ([] | T.Fn _ :: []) as xs -> invalid_num_args self xs
  | T.Fn (f, _) :: xs ->
      let[@tail_mod_cons] rec aux = function
        | [] -> []
        | [ T.List (x, _) ] | [ T.Vector (x, _) ] -> x
        | x :: xs -> x :: aux xs
      in
      f (aux xs)
  | _ -> invalid_arg self

let map self = function
  | [ T.Fn (f, _); T.List (xs, _) ] | [ T.Fn (f, _); T.Vector (xs, _) ] ->
      Result.(T.LT.map_m Fun.(List.pure %> f) xs >|= T.list)
  | [ _; _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let conj self = function
  | ([] | T.List _ :: [] | T.Vector _ :: []) as xs -> invalid_num_args self xs
  | T.List (x, meta) :: xs -> T.list' (List.fold_left List.cons' x xs) ~meta
  | T.Vector (x, meta) :: xs -> T.vector' (x @ xs) ~meta
  | _ :: _ -> invalid_arg self

let seq self = function
  | [ T.List ([], _) ] | [ T.Vector ([], _) ] | [ T.String "" ] | [ T.Nil ] ->
      T.nil'
  | [ T.List (x, _) ] | [ T.Vector (x, _) ] -> T.seq' (List.to_seq x)
  | [ T.String x ] ->
      let rec aux s i len () =
        if len = 0 then Seq.Nil
        else Seq.Cons (T.char s.[i], aux s (i + 1) (len - 1))
      in
      T.seq' (aux x 0 (String.length x))
  | [ _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let assoc self = function
  | T.Map (m, meta) :: xs -> T.map_of_list m xs ~meta
  | _ :: _ -> invalid_arg self
  | [] -> invalid_num_args self []

let dissoc self = function
  | T.Map (m, meta) :: xs ->
      T.map' (List.fold_left (Fun.flip T.MalMap.remove) m xs) ~meta
  | _ :: _ -> invalid_arg self
  | [] -> invalid_num_args self []

let get self = function
  | [ T.Map (m, _); k ] -> Ok (T.MalMap.get_or k m ~default:T.nil)
  | [ T.Nil; _ ] -> T.nil'
  | [ _; _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let contains self = function
  | [ T.Map (m, _); k ] -> T.MalMap.get k m |> Option.is_some |> T.bool'
  | [ _; _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let keys self = function
  | [ T.Map (m, _) ] -> T.MalMap.fold (fun k _ acc -> k :: acc) m [] |> T.list'
  | [ _ ] -> invalid_arg self
  | _ -> invalid_num_args self []

let vals self = function
  | [ T.Map (m, _) ] -> T.MalMap.fold (fun _ v acc -> v :: acc) m [] |> T.list'
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
  | T.Atom x :: T.Fn (f, _) :: args ->
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
  String.concat sep (List.map (T.to_string readably) xs)

let pr_str _ xs = T.string' (pr_str_list " " true xs)
let str _ xs = T.string' (pr_str_list "" false xs)

let prn _ xs =
  print_endline (pr_str_list " " true xs);
  T.nil'

let println _ xs =
  print_endline (pr_str_list " " false xs);
  T.nil'

let read_string self = function
  | [ T.String s ] -> (
      (* Read only first expr *)
      Reader.read_str s ()
      |> function
      | Seq.Nil -> T.nil'
      | Cons ((Ok _ as ok), _) -> ok
      | Cons (Error x, _) -> T.errstr (self ^ ": " ^ T.to_string false x))
  | [ _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let slurp self = function
  | [ T.String filename ] -> (
      try T.string' IO.(with_in filename read_all)
      with e -> T.errstr (self ^ ": " ^ Printexc.to_string e))
  | [ _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let readline self = function
  | [ T.String prompt ] -> (
      try
        Printf.printf "%s%!" prompt;
        T.string' (read_line ())
      with e -> (
        match e with
        | End_of_file -> T.nil'
        | e -> T.errstr (self ^ ": " ^ Printexc.to_string e)))
  | [ _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let time_ms self = function
  | [] -> T.int' (int_of_float (1000.0 *. Unix.gettimeofday ()))
  | xs -> invalid_num_args self xs

let meta self = function
  | [ T.List (_, meta) ]
  | [ T.Vector (_, meta) ]
  | [ T.Map (_, meta) ]
  | [ T.Fn (_, meta) ] -> Ok meta
  | [ _ ] -> T.nil'
  | xs -> invalid_num_args self xs

let with_meta self = function
  | [ T.List (x, _); meta ] -> T.list' x ~meta
  | [ T.Vector (x, _); meta ] -> T.vector' x ~meta
  | [ T.Map (x, _); meta ] -> T.map' x ~meta
  | [ T.Fn ((x, _) as fn); meta ] when T.is_macro fn -> T.macro (x, meta)
  | [ T.Fn (x, _); meta ] -> T.fn' x ~meta
  | [ _; _ ] -> invalid_arg self
  | xs -> invalid_num_args self xs

let type_name self = function
  | [ x ] -> T.string' (T.type_name x)
  | xs -> invalid_num_args self xs

let ns =
  let env = Env.make None in
  let set s f = Env.set s (T.fn (f ("Core." ^ s))) env in

  set "=" equal;
  set "throw" throw;

  set "nil?" is_nil;
  set "true?" is_true;
  set "false?" is_false;
  set "string?" is_string;
  set "symbol" symbol;
  set "symbol?" is_symbol;
  set "keyword" keyword;
  set "keyword?" is_keyword;
  set "number?" is_number;
  set "fn?" is_fn;
  set "macro?" is_macro;
  set "char?" is_char;
  set "int?" is_int;
  set "float?" is_float;
  set "seq?" is_seq;

  set "pr-str" pr_str;
  set "str" str;
  set "prn" prn;
  set "println" println;
  set "read-string" read_string;
  set "readline" readline;
  set "slurp" slurp;

  set "<" (int_cmp_binary Int.( < ));
  set "<=" (int_cmp_binary Int.( <= ));
  set ">" (int_cmp_binary Int.( > ));
  set ">=" (int_cmp_binary Int.( >= ));
  set "+" (int_arith_fold Int.( + ));
  set "-" (int_arith_fold Int.( - ));
  set "*" (int_arith_fold Int.( * ));
  set "/" div;
  set "time-ms" time_ms;

  set "list" list;
  set "list?" is_list;
  set "vector" vector;
  set "vector?" is_vector;
  set "hash-map" hash_map;
  set "map?" is_map;
  set "assoc" assoc;
  set "dissoc" dissoc;
  set "get" get;
  set "contains?" contains;
  set "keys" keys;
  set "vals" vals;

  set "sequential?" is_sequential;
  set "cons" cons;
  set "concat" concat;
  set "vec" vec;
  set "nth" nth;
  set "first" first;
  set "rest" rest;
  set "empty?" is_empty;
  set "count" count;
  set "apply" apply;
  set "map" map;

  set "conj" conj;
  set "seq" seq;

  set "meta" meta;
  set "with-meta" with_meta;
  set "atom" atom;
  set "atom?" is_atom;
  set "deref" deref;
  set "reset!" reset;
  set "swap!" swap;

  set "type" type_name;

  env
