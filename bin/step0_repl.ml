open Printf

let read s = s
let eval s = s
let print s = s
let rep s = s |> read |> eval |> print

let () =
  try
    while true do
      printf "user> %!";
      let line = read_line () in
      rep line |> printf "%s\n"
    done
  with End_of_file -> printf "\n"
