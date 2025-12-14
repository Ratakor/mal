open Printf

let read str = Reader.read_str str
let eval _env ast = ast
let print exp = Printer.pr_str true exp
let rep str = Result.(str |> read >|= eval "" >|= print)

let () =
  try
    while true do
      printf "user> %!";
      let line = read_line () in
      match rep line with
      | Ok x -> printf "%s\n%!" x
      | Error None -> ()
      | Error (Some x) -> printf "Error: %s\n%!" x
    done
  with End_of_file -> print_newline ()
