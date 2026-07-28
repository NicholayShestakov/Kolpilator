open Compile

let () =
  if Array.length Sys.argv < 3 then
    failwith
      "correct run format: dune exec ./main.exe -- <to_compile_file_path> \
       <compiled_file_path>"
  else
    let to_compile = Sys.argv.(1) in
    let compiled = Sys.argv.(2) in
    ToFile.to_file
      (ToAssembly.to_assembly_program
         (ToANF.to_anf_program
            (ToBeginForm.to_begin_form_program
               (Parser.to_tokens_program (Parser.to_char_list to_compile)))))
      compiled
