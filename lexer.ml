type token = Integer | Identifier (*of string?*)| Oparen | Cparen | Obrace | Return_keyword | Constant of int | Semicolon | Cbrace

(* fonction de lexing d'un string *)
let rec lex input_str = (*! à rendre plus lisible grace au currying de la prog fonctionnelle, peut-être impossible à cause de Str.group_end, tenter d'appeller juste str.group_end, et de mettre le 0 en argument ? *)
  let rec aux input_str current tokenlist = 
    if Str.string_match (Str.regexp "[+-]?[0-9]+") input_str 0 then  (*TODO: vérifier *)
      aux (String.sub input_str (Str.group_end 0) ((String.length input_str) - (Str.group_end 0))) (current + Str.group_end 0) (tokenlist @ [Constant (int_of_string (Str.matched_string input_str))]) else
    if Str.string_match (Str.regexp "return[ ]+") input_str 0 then 
      aux (String.sub input_str (Str.group_end 0) ((String.length input_str) - (Str.group_end 0))) (current + Str.group_end 0) (tokenlist @ [Return_keyword]) else
    if Str.string_match (Str.regexp "int[ ]+") input_str 0 then
      aux (String.sub input_str (Str.group_end 0) ((String.length input_str) - (Str.group_end 0))) (current + Str.group_end 0) (tokenlist @ [Integer]) else
    if Str.string_match (Str.regexp "([ ]*") input_str 0 then 
      aux (String.sub input_str (Str.group_end 0) ((String.length input_str) - (Str.group_end 0))) (current + Str.group_end 0) (tokenlist @ [Oparen]) else
    if Str.string_match (Str.regexp ")[ ]*") input_str 0 then 
      aux (String.sub input_str (Str.group_end 0) ((String.length input_str) - (Str.group_end 0))) (current + Str.group_end 0) (tokenlist @ [Cparen]) else
    if Str.string_match (Str.regexp "{[ ]*") input_str 0 then 
      aux (String.sub input_str (Str.group_end 0) ((String.length input_str) - (Str.group_end 0))) (current + Str.group_end 0) (tokenlist @ [Obrace]) else
    if Str.string_match (Str.regexp "}[ ]*") input_str 0 then 
      aux (String.sub input_str (Str.group_end 0) ((String.length input_str) - (Str.group_end 0))) (current + Str.group_end 0) (tokenlist @ [Cbrace]) else
    if Str.string_match (Str.regexp ";[ ]*") input_str 0 then 
      aux (String.sub input_str (Str.group_end 0) ((String.length input_str) - (Str.group_end 0))) (current + Str.group_end 0) (tokenlist @ [Semicolon]) else 
    if Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z_0-9]*[ ]*") input_str 0 then 
      aux (String.sub input_str (Str.group_end 0) ((String.length input_str) - (Str.group_end 0))) (current + Str.group_end 0) (tokenlist @ [Identifier]) else
    tokenlist 
  in aux input_str 0 []

(* lecture du fichier *)
let read_entire_file filename = 
  let channel = open_in filename in
  let content = really_input_string channel (in_channel_length channel) in
  close_in channel;
  content

(* Fonction d'affichage *)
let print_token t = match t with
| Oparen -> Printf.printf "opening parenthesis (\n" 
| Cparen -> Printf.printf "closing parenthesis )\n"
| Obrace -> Printf.printf "opening brace {\n"
| Cbrace -> Printf.printf "closing brace }\n"
| Semicolon -> Printf.printf "semicolon ;\n"
| Return_keyword -> Printf.printf "return keyword return\n"
| Integer -> Printf.printf "type integer int\n"
| Identifier -> Printf.printf "identifier \n" (*! à travailler pour l'affichage *)
| Constant n -> Printf.printf "constant %d\n" n

(* fonction d'écriture *)
let write_token f t = match t with
| Oparen -> Printf.fprintf f "opening parenthesis (\n" 
| Cparen -> Printf.fprintf f "closing parenthesis )\n"
| Obrace -> Printf.fprintf f "opening brace {\n"
| Cbrace -> Printf.fprintf f "closing brace }\n"
| Semicolon -> Printf.fprintf f "semicolon ;\n"
| Return_keyword -> Printf.fprintf f "return keyword return\n"
| Integer -> Printf.fprintf f "type integer int\n"
| Identifier -> Printf.fprintf f "identifier \n" (*! à travailler pour l'affichage *)
| Constant n -> Printf.fprintf f "constant %d\n" n
let test = if (Array.length (Sys.argv)) < 2 then failwith "Veuillez entrer le nom du programme à lex"

(* écriture du fichier *)
let f = if Array.length Sys.argv > 2 then (open_out Sys.argv.(2)) else (open_out "a.txt")
let to_lex = String.concat "" (Str.split (Str.regexp "\n") (read_entire_file (Sys.argv.(1))))
let () = List.iter (write_token f) (lex to_lex)
let () = close_out f

(** Prochaine étape : tester le lexer *)