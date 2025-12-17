module rec Types : sig
  type 'a with_meta = 'a * t

  and t =
    | Nil
    | Bool of bool
    | Int of int
    | String of string
    | Symbol of string
    | Keyword of string
    | List of t list with_meta
    | Vector of t list with_meta
    | Map of t MalMap.t with_meta
    | Fn of (t list -> (t, t) result) with_meta
    | Atom of t ref
end =
  Types

and MalValue : sig
  type t = Types.t

  val compare : t -> t -> int
end = struct
  type t = Types.t

  let compare = Stdlib.compare
end

and MalMap : (Map.S with type key = MalValue.t) = Map.Make (MalValue)

include Types

module Traverse = List.Traverse (struct
  type 'a t = ('a, Types.t) result

  let return = Result.return
  let ( >>= ) = Result.( >>= )
end)

let rec equal a b =
  match (a, b) with
  | Nil, Nil -> true
  | Bool a, Bool b -> Bool.equal a b
  | Int a, Int b -> Int.equal a b
  | String a, String b | Symbol a, Symbol b | Keyword a, Keyword b ->
      String.equal a b
  | List (a, _), List (b, _)
  | List (a, _), Vector (b, _)
  | Vector (a, _), List (b, _)
  | Vector (a, _), Vector (b, _) -> List.equal equal a b
  | Map (a, _), Map (b, _) -> MalMap.equal equal a b
  | Fn a, Fn b -> Stdlib.(a == b) (* meta matters for functions *)
  | Atom a, Atom b -> equal !a !b
  | _ -> false

let to_string readably =
  let rec aux = function
    | Nil -> "nil"
    | Bool x -> string_of_bool x
    | Int x -> string_of_int x
    | String x when readably -> "\"" ^ String.escaped x ^ "\""
    | String x | Symbol x -> x
    | Keyword x -> ":" ^ x
    | List (x, _) -> List.to_string ~start:"(" ~stop:")" ~sep:" " aux x
    | Vector (x, _) -> List.to_string ~start:"[" ~stop:"]" ~sep:" " aux x
    | Map (x, _) ->
        MalMap.to_list x
        |> List.to_string ~start:"{" ~stop:"}" ~sep:" " (fun (k, v) ->
            Printf.sprintf "%s %s" (aux k) (aux v))
    | Fn _ -> "#<function>"
    | Atom x -> Printf.sprintf "(atom %s)" (aux !x)
  in
  aux

(* Constructor wrappers *)
let nil = Nil
let maltrue = Bool true
let malfalse = Bool false
let bool x = Bool x
let int x = Int x
let string x = String x
let symbol x = Symbol x
let keyword x = Keyword x
let list ?(meta = nil) x = List (x, meta)
let vector ?(meta = nil) x = Vector (x, meta)
let map ?(meta = nil) x = Map (x, meta)
let fn ?(meta = nil) x = Fn (x, meta)
let atom x = Atom (ref x)

(* Constructor wrappers with result *)
let nil' = Ok nil
let maltrue' = Ok maltrue
let malfalse' = Ok malfalse
let bool' x = Ok (bool x)
let int' x = Ok (int x)
let string' x = Ok (string x)
let symbol' x = Ok (symbol x)
let keyword' x = Ok (keyword x)
let list' ?(meta = nil) x = Ok (list x ~meta)
let vector' ?(meta = nil) x = Ok (vector x ~meta)
let map' ?(meta = nil) x = Ok (map x ~meta)
let fn' ?(meta = nil) x = Ok (fn x ~meta)
let atom' x = Ok (atom x)
let errstr x = Error (string x)

let rec map_of_list ?(meta = nil) acc = function
  | [] -> map' acc ~meta
  | k :: v :: xs -> map_of_list (MalMap.add k v acc) xs
  | k :: [] ->
      errstr (Printf.sprintf "Missing value for key: %s" (to_string true k))

let macro_kw = Keyword "__macro"

let macro (f, meta) =
  match meta with
  | Map (m, _) -> fn' f ~meta:(map (MalMap.add macro_kw maltrue m))
  | Nil -> fn' f ~meta:(map (MalMap.add macro_kw maltrue MalMap.empty))
  | _ -> errstr "Failed to create macro: Invalid metadata"

let is_macro (_, meta) =
  match meta with
  | Map (m, _) -> (
      match MalMap.get macro_kw m with
      | Some (Bool true) -> true
      | _ -> false)
  | _ -> false
