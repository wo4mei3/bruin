let () = print_endline "Hello, World!"

let () =
  let argc = Array.length Sys.argv in
  if argc != 2 then (
    Format.printf "Usage: ./bruin [filename]\n";
    exit (-1))
  else
    let fname = Sys.argv.(1) in
    let inchan = open_in fname in
    let filebuf = Lexing.from_channel inchan in
    let dl = Bruin.Parser.top Bruin.Lexer.token filebuf in
    print_endline (Bruin.Ast.show_dl dl)