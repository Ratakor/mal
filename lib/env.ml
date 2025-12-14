module T = Types
module M = Map.Make (String)

type env = {
  outer : env option;
  map : T.t M.t ref;
}

let make outer = { outer; map = ref M.empty }
let set key value env = env.map := M.add key value !(env.map)

let rec get key env =
  match M.find_opt key !(env.map) with
  | Some x -> Some x
  | None -> Option.(env.outer >>= get key)
