open Printf

let read str = Reader.read_str str
let eval ast = ast
let print exp = Printer.pr_str exp
let rep str = Option.(str |> read >|= eval >|= print)

let () =
  try
    while true do
      printf "user> %!";
      let line = read_line () in
      match rep line with
      | None -> printf "error?\n"
      | Some s -> printf "%s\n" s
    done
  with End_of_file -> print_newline ()
