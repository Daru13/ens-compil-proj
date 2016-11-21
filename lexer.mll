{
	(* TODO: à retirer, utiliser les types induits par les tokens du parser à la place *)
	open Tokens
	open Big_int

	(* Liste des mots-clés réservés *)
	let keywords = Hashtbl.create 31
	let () = List.iter (fun (id, symbol) -> Hashtbl.add keywords id symbol)
	[
		"access", ACCESS;
		"and", AND;
		(* "and then", AND_THEN; *)
		"begin", BEGIN;
		"else", ELSE;
		"elsif", ELSIF;
		"end", END;
		"false", FALSE;
		"for", FOR;
		"function", FUNCTION;
		"if", IF;
		"in", IN;
		"is", IS;
		"loop", LOOP;
		"new", NEW;
		"not", NOT;
		"null", NULL;
		"or", OR;
		(* "or else", OR_ELSE; *)
		"out", OUT;
		"procedure", PROCEDURE;
		"record", RECORD;
		"rem", REM;
		"return", RETURN;
		"reverse", REVERSE;
		"then", THEN;
		"true", TRUE;
		"type", TYPE;
		"use", USE;
		"while", WHILE;
		"with", WITH
	]

	(* Big integer equal to 2^31 *)
	let max_allowed_int = big_int_of_string "2147483648"
}

(* Alias utiles pour des syntaxes simples *)
let letter 	= ['a'-'z' 'A'-'Z']
let digit 	= ['0'-'9']

(* Identificateur *)
let ident = letter (letter | digit | '_')*

(* Constante numérique entière *)
let number = digit+

(* Blancs *)
let whitespace  = [' ' '\t']+
let newline 	= ['\n']

(* Début et fin de commentaire *)
let comment_begin 	= "--"
let comment_end 	= '\n'

(******************************************************************************)

rule token = parse
| whitespace 		{ token lexbuf }
| newline 			{ Lexing.new_line lexbuf; token lexbuf }
| comment_begin 	{ comment lexbuf }

| ";" 				{ SEMICOLON }
| "," 				{ COMMA }
| "(" 				{ OPEN_PARENTHESIS }
| ")" 				{ CLOSE_PARENTHESIS }
| ":" 				{ COLON }
| "." 				{ DOT }
| ".." 				{ TWO_DOTS }
| ":=" 				{ COLON_EQUAL }

| "=" 				{ EQUAL }
| "/=" 				{ DIFFERENT }

| ">" 				{ COMPARATOR (Greather_than) }
| ">=" 				{ COMPARATOR (Greater_eq) }
| "<" 				{ COMPARATOR (Less_than) }
| "<=" 				{ COMPARATOR (Less_eq) }

| "+" 				{ PLUS }
| "-"				{ MINUS }
| "*" 				{ TIMES }
| "/" 				{ DIV }
| "rem" 			{ REM }

(* TODO: différencier MINUS et NEG (resp. binaire et unaire) ! *)

| number as str		{ 
					  let big_int_number = big_int_of_string str in
					  if gt_big_int big_int_number max_allowed_int then
					  	failwith ("Too large integer: " ^ str)
					  else
					    INT (int_of_big_int big_int_number)
					}

| ident as str		{ let lowercase_id = String.lowercase str in
					  try
						Hashtbl.find keywords lowercase_id
					  with
					  	Not_found -> ID (lowercase_id)
					}

| ''' (_ as c) ''' 	{ CHAR (c) } 

| _ as c 			{ failwith ("Illegal character: " ^ String.make 1 c) }

| eof 				{ EOF }

and comment = parse
| comment_end 		{ token lexbuf }
| _					{ comment lexbuf }