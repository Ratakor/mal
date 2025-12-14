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
    | Fn of (t list -> (t, string) result)
end =
  Types

include Types

let list x = Types.List x
let vector x = Types.Vector x
