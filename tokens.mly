%{

%}

(* Symboles terminaux *)

%token <string> ID
%token <int> INT
%token <char> CHAR
%token EOF

%token SEMICOLON 			(* ; *)
%token COMMA 				(* , *)
%token OPEN_PARENTHESIS 	(* ( *)
%token CLOSE_PARENTHESIS	(* ) *)
%token COLON 				(* : *)
%token COLON_EQUAL 			(* := *)
%token TWO_DOTS				(* .. *)

%token ACCESS BEGIN ELSE ELSIF END FALSE FOR FUNCTION IF IN ISLOOP NEW NULL OUT PROCEDURE RECORD RETURN REVERSE THEN TRUE TYPE USE WHILE WITH

%token OR OR_ELSE
%token AND AND_THEN
%token NOT
%token EQUAL DIFFERENT
%token <Ast.comparator> COMPARATOR
%token PLUS MINUS
%token TIMES DIV REM
%token NEG
%token DOT 					(* . *)

(* Symboles non-terminaux *)



%left OR OR_ELSE
%left AND AND_THEN
%left PLUS MINUS
%left TIMES DIV REM
%left DOT

%start <Ast.program> program

%%
