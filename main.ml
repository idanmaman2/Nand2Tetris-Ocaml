#load "str.cma";;
exception CommandError of string
open Str

let tag_count = ref 0 

let tokenizer tokens namespace = 
  let ftoken = List.hd tokens in
  match ftoken with 
  | "push" -> 
    let segment = List.hd (List.tl tokens) in 
    let value = List.hd ( List.tl ( List.tl tokens) ) in
    let valueToPush = match segment with 
      | "argument" -> String.concat "\n" [ String.cat "@" value ; "D=A" ; "@ARG" ; "A=M" ; "A=A+D" ; "D+M" ]  
      | "local" ->    String.concat "\n" [ String.cat "@" value ; "D=A" ; "@LCL" ; "A=M" ; "A=A+D" ; "D+M"]  
      | "this" ->     String.concat "\n" [ String.cat "@" value ; "D=A" ; "@THIS" ; "A=M" ; "A=A+D" ; "D+M" ]  
      | "that" ->     String.concat "\n" [ String.cat "@" value ; "D=A" ; "@THAT" ; "A=M" ; "A=A+D" ; "D+M"  ]  
      | "temp" ->     String.concat "\n" [ String.cat "@" value ; "D=M" ]  
      | "constant" -> String.concat "\n" [ String.cat "@" value ; "D=A" ]  
      | "static" ->   String.concat "\n" [ String.concat "" ["@" ;  namespace ; "." ; value] ; "D=M" ] 
      | "pointer" ->   match value with 
                        | "0" -> "@THIS\nD=M"
                        | "1" -> "@THAT\nD=M"
                        | _ -> raise (CommandError "Invalid Pointer value..." ) 
      | _ -> raise (CommandError "Wrong Segment in push command" ) in
    String.concat "\n" [ valueToPush  ; "@SP"  ;  "A=M" ; "M=D" ; "@SP" ; "M=M+1" ; ""] 
  | "pop" -> let segment = List.hd (List.tl tokens) in 
    let value = List.hd ( List.tl ( List.tl tokens) ) in
    let valueToPush = match segment with 
      | "argument" -> String.concat "\n" ["@ARG" ; "A=M" ; String.concat "\n" (List.init  (int_of_string value ) (fun x -> "A=A+1"))  ]  
      | "local" ->    String.concat "\n" ["@LCL" ; "A=M" ; String.concat "\n" (List.init  (int_of_string value ) (fun x -> "A=A+1"))  ]    
      | "this" ->     String.concat "\n" ["@THIS" ; "A=M" ; String.concat "\n" (List.init  (int_of_string value ) (fun x -> "A=A+1"))  ]   
      | "that" ->     String.concat "\n" ["@THAT" ; "A=M" ; String.concat "\n" (List.init  (int_of_string value ) (fun x -> "A=A+1"))  ]   
      | "temp" ->     String.concat "\n" [String.cat "@" (string_of_int ((int_of_string value)+5) ) ]  
      | "static" ->   String.concat "\n" [ String.concat "" ["@" ;  namespace ; "." ; value]  ]  
      | "pointer" ->  match value with 
                        | "0" -> "@THIS"
                        | "1" -> "@THAT"
                        | _ -> raise (CommandError "Invalid Pointer value..." ) 
      | _ -> raise (CommandError "Wrong Segment in pop command" ) in
    String.concat "\n" [ "@SP"  ;  "A=M-1" ; "D=M" ; valueToPush ; "M=D" ; "@SP" ; "M=M-1" ; ""] 
  | "or" -> String.concat "\n" ["@SP"; "M=M-1" ; "A=M" ;"A=M" ; "D=M" ; "@SP" ;"A=M-1" ; "M=D|M" ; ""]
  | "and" -> String.concat "\n" ["@SP"; "M=M-1" ; "A=M" ;"A=M" ; "D=M" ; "@SP" ;"A=M-1" ; "M=D&M" ; ""]
  | "neg" -> String.concat "\n" ["@SP" ; "A=M-1" ; "M=-M"]
  | "add" -> String.concat "\n" ["@SP"; "M=M-1" ; "A=M" ;"A=M" ; "D=M" ; "@SP" ;"A=M-1" ; "M=D+M" ; ""]
  | "sub" -> String.concat "\n" ["@SP"; "M=M-1" ; "A=M" ;"A=M" ; "D=M" ; "@SP" ;"A=M-1" ; "M=D-M" ; ""]
  | "not" -> String.concat "\n" ["@SP" ; "A=M-1" ; "M=!M" ;""]
  | "eq" -> 
    tag_count := !tag_count + 1 ; 
    let tag_name = String.cat "TAG_" (string_of_int !tag_count) in 
    let true_tag = String.cat "IF_TRUE" tag_name in 
    let false_tag = String.cat "IF_FALSE" tag_name in
    String.concat "\n" ["@SP" ; "A=M-1" ; "D=M"; "A=A-1" ; "D=D-M" ; (String.cat "@" true_tag) ;"D;JEQ" ; "D=0" ; (String.cat "@" false_tag) ; "0;JMP" ; String.cat (String.cat "(" true_tag ) ")"; "D=-1" ; String.cat (String.cat "(" false_tag ) ")"; "@SP" ; "A=M-1" ; "A=A-1"  ;"M=D" ; "@SP" ;"M=M-1" ; "" ];
  | "gt" -> 
  tag_count := !tag_count + 1 ; 
    let tag_name = String.cat "TAG_" (string_of_int !tag_count) in 
    let true_tag = String.cat "IF_TRUE" tag_name in 
    let false_tag = String.cat "IF_FALSE" tag_name in
    String.concat "\n" ["@SP" ; "A=M-1" ; "D=M"; "A=A-1" ; "D=D-M" ; (String.cat "@" true_tag) ;"D;JGT" ; "D=0" ; (String.cat "@" false_tag) ; "0;JMP" ; String.cat (String.cat "(" true_tag ) ")"; "D=-1" ; String.cat (String.cat "(" false_tag ) ")"; "@SP" ; "A=M-1" ; "A=A-1"  ;"M=D" ; "@SP" ;"M=M-1" ; ""];
  | "lt" -> 
  tag_count := !tag_count + 1 ; 
    let tag_name = String.cat "TAG_" (string_of_int !tag_count) in 
    let true_tag = String.cat "IF_TRUE" tag_name in 
    let false_tag = String.cat "IF_FALSE" tag_name in
    String.concat "\n" ["@SP" ; "A=M-1" ; "D=M"; "A=A-1" ; "D=D-M" ; (String.cat "@" true_tag) ;"D;JLT" ; "D=0" ; (String.cat "@" false_tag) ; "0;JMP" ; String.cat (String.cat "(" true_tag ) ")"; "D=-1" ; String.cat (String.cat "(" false_tag ) ")"; "@SP" ; "A=M-1" ; "A=A-1"  ;"M=D" ; "@SP" ;"M=M-1" ; ""];
  | _ -> 
    let regex = regexp "^([\\s]*[\\/\\/|].*)?$" in 
    if string_match regex ftoken 0 then
        ""
    else
        ""



let() = 
  let arg = Sys.argv.(1) in
  let split_index = match  String.rindex_opt arg '.'  with 
    |None -> String.length arg 
    |Some i -> i in
  let namespace = String.sub arg 0 split_index in 
  let output_file = String.cat namespace ".asm" in
  let fdOutput_file = open_out output_file in 
  let fdInput_file = open_in arg in 
  try
    while true do
      let line = String.trim (input_line fdInput_file) in
      let tokens = String.split_on_char ' ' line in 
      Printf.fprintf fdOutput_file "%s" (tokenizer tokens namespace)
    done
  with End_of_file ->
    close_in fdInput_file;
