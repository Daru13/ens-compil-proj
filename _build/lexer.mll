{
	open Types
	open Big_int

	(* Liste des mots-clés réservés *)
	let keywords = Hashtbl.create 29
	let () = List.iter (fun (id, symbol) -> Hashtbl.add keywords id symbol)
	[
		"access", TOK_KW_ACCESS;
		"and", TOK_KW_AND;
		"begin", TOK_KW_BEGIN;
		"else", TOK_KW_ELSE;
		"elsif", TOK_KW_ELSIF;
		"end", TOK_KW_END;
		"false", TOK_KW_FALSE;
		"for", TOK_KW_FOR;
		"function", TOK_KW_FUNCTION;
		"if", TOK_KW_IF;
		"in", TOK_KW_IN;
		"is", TOK_KW_IS;
		"loop", TOK_KW_LOOP;
		"new", TOK_KW_NEW;
		"not", TOK_KW_NOT;
		"null", TOK_KW_NULL;
		"or", TOK_KW_OR;
		"out", TOK_KW_OUT;
		"procedure", TOK_KW_PROCEDURE;
		"record", TOK_KW_RECORD;
		"rem", TOK_KW_REM;
		"return", TOK_KW_RETURN;
		"reverse", TOK_KW_REVERSE;
		"then", TOK_KW_THEN;
		"true", TOK_KW_TRUE;
		"type", TOK_KW_TYPE;
		"use", TOK_KW_USE;
		"while", TOK_KW_WHILE;
		"with", TOK_KW_WITH
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
let whitespace = [' ' '\t' '\n']+

(* Début et fin de commentaire *)
let comment_begin 	= "--"
let comment_end 	= '\n'

(******************************************************************************)

rule token = parse
| whitespace 		{ token lexbuf }
| comment_begin 	{ comment lexbuf }

| ";" 				{ TOK_SEMICOLON }
| "," 				{ TOK_COMMA }
| "(" 				{ TOK_OPEN_PARENTHESIS }
| ")" 				{ TOK_CLOSE_PARENTHESIS }
| ":" 				{ TOK_COLON }
| "." 				{ TOK_DOT }
| ".." 				{ TOK_TWO_DOTS }

| "=" 				{ TOK_COMP (COMP_EQ) }
| "/=" 				{ TOK_COMP (COMP_DIFF) }
| ">" 				{ TOK_COMP (COMP_GREATER_TH) }
| ">=" 				{ TOK_COMP (COMP_GREATER_EQ) }
| "<" 				{ TOK_COMP (COMP_LESS_TH) }
| "<=" 				{ TOK_COMP (COMP_LESS_EQ) }

| "+" 				{ TOK_OP_PLUS }
| "-"				{ TOK_OP_MINUS }
| "*" 				{ TOK_OP_TIMES }
| "/" 				{ TOK_OP_DIV }
| "rem" 			{ TOK_OP_REM }

| number as str		{ 
					  let big_int_number = big_int_of_string str in
					  if gt_big_int big_int_number max_allowed_int then
					  	failwith ("Too large integer: " ^ str)
					  else
					    TOK_INT (int_of_big_int big_int_number)
					}

| ident as str		{ let lowercase_id = String.lowercase str in
					  try
						Hashtbl.find keywords lowercase_id
					  with
					  	Not_found -> TOK_ID (lowercase_id)
					}

| ''' (_ as c) ''' 	{ TOK_CHAR (c) } 

| _ as c 			{ failwith ("Illegal character: " ^ String.make 1 c) }

| eof 				{ TOK_EOF }

and comment = parse
| comment_end 		{ token lexbuf }
| _					{ comment lexbuf }