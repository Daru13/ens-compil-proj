(* Types OCaml utilisés par l'analyseur lexical *)

type comparator =
| COMP_EQ
| COMP_DIFF
| COMP_GREATER_TH
| COMP_GREATER_EQ
| COMP_LESS_TH
| COMP_LESS_EQ

type token =
| TOK_ID of string
| TOK_COMP of comparator
| TOK_INT of int
| TOK_CHAR of char
| TOK_EOF
| TOK_SEMICOLON 		(* ; *)
| TOK_COMMA 			(* , *)
| TOK_OPEN_PARENTHESIS 	(* ( *)
| TOK_CLOSE_PARENTHESIS	(* ) *)
| TOK_COLON 			(* : *)
| TOK_DOT 				(* . *)
| TOK_TWO_DOTS			(* .. *)
| TOK_KW_ACCESS 
| TOK_KW_AND 
| TOK_KW_BEGIN 
| TOK_KW_ELSE 
| TOK_KW_ELSIF 
| TOK_KW_END
| TOK_KW_FALSE 
| TOK_KW_FOR 
| TOK_KW_FUNCTION 
| TOK_KW_IF 
| TOK_KW_IN 
| TOK_KW_IS
| TOK_KW_LOOP 
| TOK_KW_NEW 
| TOK_KW_NOT 
| TOK_KW_NULL 
| TOK_KW_OR 
| TOK_KW_OUT
| TOK_KW_PROCEDURE 
| TOK_KW_RECORD 
| TOK_KW_REM 
| TOK_KW_RETURN 
| TOK_KW_REVERSE 
| TOK_KW_THEN
| TOK_KW_TRUE 
| TOK_KW_TYPE 
| TOK_KW_USE 
| TOK_KW_WHILE 
| TOK_KW_WITH
| TOK_OP_PLUS
| TOK_OP_MINUS 			(* Moins binaire/générique ? *)
| TOK_OP_TIMES
| TOK_OP_DIV
| TOK_OP_REM
| TOK_OP_NEG 			(* Négation unaire *) 