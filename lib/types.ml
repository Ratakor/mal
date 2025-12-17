module rec Types : sig
  type t =
    | Nil
    | Bool of bool
    | Int of int
    | String of string
    | Symbol of string
    | Keyword of string
    | List of t list
    | Vector of t list
    | Map of t MalMap.t
    | Fn of {
        value : t list -> (t, t) result;
        is_macro : bool;
      }
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
  | List a, List b | List a, Vector b | Vector a, List b | Vector a, Vector b ->
      List.equal equal a b
  | Map a, Map b -> MalMap.equal equal a b
  | Fn { value = a; is_macro = ma }, Fn { value = b; is_macro = mb } ->
      Bool.equal ma mb && Stdlib.(a == b)
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
    | List x -> List.to_string ~start:"(" ~stop:")" ~sep:" " aux x
    | Vector x -> List.to_string ~start:"[" ~stop:"]" ~sep:" " aux x
    | Map x ->
        MalMap.to_list x
        (* |> List.rev *)
        |> List.to_string ~start:"{" ~stop:"}" ~sep:" " (fun (k, v) ->
            Printf.sprintf "%s %s" (aux k) (aux v))
    | Fn _ -> "#<function>"
    | Atom x -> Printf.sprintf "(atom %s)" (aux !x)
  in
  aux

let bool x = Types.Bool x
let int x = Types.Int x
let string x = Types.String x
let list x = Types.List x
let vector x = Types.Vector x
let map x = Types.Map x
let fn x = Types.Fn { value = x; is_macro = false }
let atom x = Types.Atom (ref x)
let errstr x = Error (Types.String x)

let rec map_of_list acc = function
  | [] -> Ok (Types.Map acc)
  | k :: v :: xs -> map_of_list (MalMap.add k v acc) xs
  | k :: [] ->
      errstr (Printf.sprintf "Missing value for key '%s'" (to_string true k))
