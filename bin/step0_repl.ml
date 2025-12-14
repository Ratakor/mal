open Printf

let read str = str
let eval _env ast = ast
let print exp = exp
let rep str = str |> read |> eval "" |> print

let () =
  try
    while true do
      printf "user> %!";
      read_line () |> rep |> printf "%s\n"
    done
  with End_of_file -> printf "\n"
