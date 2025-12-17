module Data = Map.Make (String)

type env = {
  outer : env option;
  data : Types.t Data.t ref;
}

let make outer = { outer; data = ref Data.empty }
let set key value env = env.data := Data.add key value !(env.data)

let rec get key env =
  match Data.get key !(env.data) with
  | Some x -> Some x
  | None -> Option.(env.outer >>= get key)
