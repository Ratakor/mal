open Printf
module T = Types

let read str = Reader.read_str str
let eval _env ast = ast
let print exp = T.to_string true exp

let rep str =
  read str
  |> Seq.map (function
    | Ok ast -> Ok (eval () ast |> print)
    | Error _ as err -> err)

let () =
  try
    while true do
      printf "user> %!";
      read_line ()
      |> rep
      |> Seq.iter (function
        | Ok x -> printf "%s\n%!" x
        | Error x -> printf "Error: %s\n%!" (T.to_string false x))
    done
  with End_of_file -> print_newline ()
