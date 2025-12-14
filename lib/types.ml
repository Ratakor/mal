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
    | Fn of (t list -> (t, string) result)
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

let list x = Types.List x
let vector x = Types.Vector x

let map_of_list x =
  let rec aux acc = function
    | [] -> Ok (Types.Map acc)
    | k :: v :: xs -> aux (MalMap.add k v acc) xs
    | _ :: [] -> Error "Missing value in Map"
  in
  aux MalMap.empty x
