open BfParser.Parser_driver

let read_all_lines () =
  let rec read_lines acc =
    try
      let line = read_line () in
      read_lines (acc ^ line ^ "\n")
    with End_of_file -> acc
  in
  read_lines ""

let () =
  let input = read_all_lines () in
  let result = parse_and_format input in
  print_endline result
