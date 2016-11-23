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

%left OR OR_ELSE
%left AND AND_THEN
%left PLUS MINUS
%left TIMES DIV REM
%left DOT

(* Symboles non-terminaux *)

%type <Ast.program> program
%type <Ast.declaration> declaration
%type <Ast.fields> fields
%type <Ast.ty> ty
%type <Ast.params> parameters
%type <Ast.param> parameter
%type <Ast.mode> mode
%type <Ast.expression> expression
%type <Ast.instruction> instruction
%type <Ast.binop> operator
%type <Ast.access> access

%start program

%%

program:
  WITH; id_ada_io_1 = ID; SEMICOLON;
  USE; id_ada_io_2 = ID; SEMICOLON;
  PROCEDURE; id_proc_1 = ID; delc_l = declaration*;
  BEGIN; instr_l = instruction+; END; id_proc_2 = ID?; SEMICOLON;
  EOF;
  {

  }

declaration:
| TYPE; id = ID; SEMICOLON;
  {

  }
| TYPE; id_type = ID; IS; ACCESS; id_access = ID; SEMICOLON;
  {

  }
| TYPE; id_type = ID; IS; RECORD; fields_l = fields+; END; RECORD; SEMICOLON;
  {

  }
| ids_l = separated_nonempty_list(COMMA, ID); COLON; t = ty;
  option(COLON_EQUAL; expr = expression); SEMICOLON;
  {

  }
| PROCEDURE; id_proc_1 = ID; param_l = parameters?; IS; decl_l = declaration*;
  BEGIN; instr_l = instruction+; END; id_proc_2 = ID?; SEMICOLON;
  {

  }
| FUNCTION; id_func_1 = ID; param_l = parameters?; IS;
  RETURN; t = ty; IS; decl_l = declaration*;
  BEGIN; instr_l = instruction+; END; id_func_2 = ID?; SEMICOLON;
  {

  }

fields:
  ids_l = separated_nonempty_list(COMMA, ID); COLON; t = ty; SEMICOLON;
  {

  }

ty:
| id = ID;
  {

  }
| ACCESS; id = ID;
  {

  }

parameters:
  delimited(OPEN_PARENTHESIS,
 			param_l = separated_nonempty_list(SEMICOLON, parameter),
 			CLOSE_PARENTHESIS);
  {

  }

parameter:
  ids_l = separated_nonempty_list(COMMA, ID);
  COLON; m = mode?; t = ty;
  {

  }

mode:
| IN;
  {

  }
| IN; OUT;
  {

  }

/******************************************************************************/

expression:
| int = INT;
  {

  }
| c = CHAR;
  {

  }
| TRUE;
  {
  	
  }
| FALSE;
  {
  	
  }
| NULL;
  {
  	
  }
| delimited(OPEN_PARENTHESIS,
 			expr = expression,
 			CLOSE_PARENTHESIS);
  {
  	
  }
| acc = access;
  {
  	
  }
| delimited(expr_1 = expression,
			op 	   = operator,
			expr_2 = expression)
  {
  	
  }
| NOT; expr = expression;
  {
  	
  }
| NEG; expr = expression;
  {
  	
  }
| NEW; id = ID;
  {
  	
  }
| id = ID;
  delimited(OPEN_PARENTHESIS,
 			expr_l = separated_nonempty_list(COMMA, expression),
 			CLOSE_PARENTHESIS);
  {
  	
  }
/* TODO: Ajouter "character ' val ( <expr>,+ )" ! */

instruction:
| acc = access; COLON_EQUAL; expr = expression; SEMICOLON;
  {
  	
  }
| id = ID; SEMICOLON;
  {
  	
  }
| id = ID;
  delimited(OPEN_PARENTHESIS,
 			expr_l = separated_nonempty_list(COMMA, expression),
 			CLOSE_PARENTHESIS);
  SEMICOLON;
  {
  	
  }
| RETURN; expr = expression?; SEMICOLON;
  {
  	
  }
| BEGIN; instr_l = instruction+; END; SEMICOLON;
  {
  	
  }
| IF; if_expr = expression;
  THEN; then_instr_l = instruction+;
  elseif_l = (ELSIF; separated_pair(expression, THEN, instruction+))*;
  (ELSE; else_instr_l = instruction+)?;
  END; IF; SEMICOLON;
  {
  	
  }
| FOR; id = ID; IN; reverse_order = boption(REVERSE);
  expr_start = expression; TWO_DOTS; expr_end = expression;
  LOOP; instr_l = instruction+; END; LOOP; SEMICOLON;
  {
  	
  }
| WHILE; while_expr = expression; LOOP;
  instr_l = instruction+; END; LOOP; SEMICOLON;
  {
  	
  }

operator:
| EQUAL;
  {

  }
| DIFFERENT;
  {

  }
| COMPARATOR;
  {

  }
| PLUS;
  {

  }
| MINUS;
  {

  }
| TIMES;
  {

  }
| DIV;
  {

  }
| REM;
  {

  }
| AND;
  {

  }
| AND_THEN;
  {

  }
| OR;
  {

  }
| OR_ELSE;
  {

  }

access:
| id = ID;
  {

  }
| expr = expression; DOT; id = ID;
  {

  }
  