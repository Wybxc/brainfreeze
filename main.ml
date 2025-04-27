let () =
  print_string "Enter your name: ";
  let name = read_line () in
  let greeting = CoqExtract.greet name in
  print_endline greeting