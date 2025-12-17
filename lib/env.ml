module Data = Map.Make (String)

type env = {
  outer : env option;
  data : Types.t Data.t ref; (* TODO: why is this a ref? *)
}

let make outer = { outer; data = ref Data.empty }
let set key value env = env.data := Data.add key value !(env.data)

let rec get key env =
  match Data.find_opt key !(env.data) with
  | Some x -> Some x
  | None -> Option.(env.outer >>= get key)
