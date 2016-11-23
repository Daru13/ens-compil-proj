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

%token ACCESS BEGIN ELSE ELSIF END FALSE FOR FUNCTION IF IN IS LOOP NEW NULL OUT PROCEDURE RECORD RETURN REVERSE THEN TRUE TYPE USE WHILE WITH

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
  PROCEDURE; id_proc_1 = ID; decl_l = declaration*;
  BEGIN; instr_l = instruction+; END; id_proc_2 = ID?; SEMICOLON;
  EOF;
  {
  	if id_ada_io_1 <> "Ada.Text_IO" then
  		failwith "'Ada.Text_IO' is required by mini-ADA"
  	else if id_ada_io_1 <> id_ada_io_2 then
  		failwith "Both 'with' and 'use' must concern 'Ada.Text_IO'"
  	else
  		(id_proc_1, decl_l, instr_l)
  }

declaration:
| TYPE; id = ID; SEMICOLON;
  {
  	Delc_type(id)
  }
| TYPE; id_type = ID; IS; ACCESS; id_access = ID; SEMICOLON;
  {
  	Decl_access(id_type, id_access)
  }
| TYPE; id_type = ID; IS; RECORD; fields_l = fields+; END; RECORD; SEMICOLON;
  {
  	Decl_record(id_type, fields_l)
  }
| ids_l = separated_nonempty_list(COMMA, ID); COLON; t = ty;
  expr = option(COLON_EQUAL; e = expression { e }); SEMICOLON;
  {
  	Decl_vars(ids_l, t, expr)
  }
| PROCEDURE; id_proc_1 = ID; param_l = parameters?; IS; decl_l = declaration*;
  BEGIN; instr_l = instruction+; END; id_proc_2 = ID?; SEMICOLON;
  {
  	if id_proc_1 <> id_proc_2 then
  		failwith "Procedure identifiers do not match (" ^ id_proc_1 ^ " and " ^ id_proc_2 ^ ")"
  	else
  		Decl_procedure(id_proc_1, param_l, decl_l, instr_l)
  }
| FUNCTION; id_func_1 = ID; param_l = parameters?; IS;
  RETURN; t = ty; IS; decl_l = declaration*;
  BEGIN; instr_l = instruction+; END; id_func_2 = ID?; SEMICOLON;
  {
  	if id_proc_1 <> id_proc_2 then
  		failwith "Function identifiers do not match (" ^ id_func_1 ^ " and " ^ id_func_2 ^ ")"
  	else
  		Decl_procedure(id_func_1, param_l, decl_l, instr_l)
  }

fields:
  ids_l = separated_nonempty_list(COMMA, ID); COLON; t = ty; SEMICOLON;
  {
  	(ids_l, ty)
  }

ty:
| id = ID;
  {
  	Ty_var(id)
  }
| ACCESS; id = ID;
  {
  	Ty_access(id)
  }

parameters:
  delimited(OPEN_PARENTHESIS,
 			separated_nonempty_list(SEMICOLON, parameter),
 			CLOSE_PARENTHESIS);
  {
  	param_l
  }

parameter:
  ids_l = separated_nonempty_list(COMMA, ID);
  COLON; m = mode?; t = ty;
  {
  	(ids_l, m, t)
  }

mode:
| IN; 		{ Mod_in }
| IN; OUT; 	{ Mod_inOut }

/******************************************************************************/

expression:
| int = INT;
  {
  	let value = Expr_int(int) in
  	{value = value; pos = $startpos * $endpos}
  }
| c = CHAR;
  {
  	let value = Expr_char(c) in
  	{value = value; pos = $startpos * $endpos}
  }
| TRUE;
  {
  	let value = Expr_bool(true) in
  	{value = value; pos = $startpos * $endpos}
  }
| FALSE;
  {
  	let value = Expr_bool(false) in
  	{value = value; pos = $startpos * $endpos}	
  }
| NULL;
  {
  	let value = Expr_null in
  	{value = value; pos = $startpos * $endpos}
  }
| delimited(OPEN_PARENTHESIS,
 			expression,
 			CLOSE_PARENTHESIS);
  {
  	expr
  }
| acc = access;
  {
  	let value = Expr_bool(true) in
  	{value = value; pos = $startpos * $endpos}
  }
| expr_1 = expression;
  op 	 = operator;
  expr_2 = expression;
  {
  	let value = Expr_binop(expr_l, op, expr_2) in
  	{value = value; pos = $startpos * $endpos}
  }
| NOT; expr = expression;
  {
  	let value = Expr_unop(UnOp_not, expr) in
  	{value = value; pos = $startpos * $endpos}
  }
| NEG; expr = expression;
  {
  	let value = Expr_unop(UnOp_negative, expr) in
  	{value = value; pos = $startpos * $endpos}
  }
| NEW; id = ID;
  {
  	let value = Expr_new(id) in
  	{value = value; pos = $startpos * $endpos}
  }
| id = ID;
  delimited(OPEN_PARENTHESIS,
 			separated_nonempty_list(COMMA, expression),
 			CLOSE_PARENTHESIS);
  {
  	let value = Expr_call(id, expr_l) in
  	{value = value; pos = $startpos * $endpos}
  }
/* TODO: Ajouter "character ' val ( <expr>,+ )" ! */

instruction:
| acc = access; COLON_EQUAL; expr = expression; SEMICOLON;
  {
  	Instr_set(acc, expr)
  }
| id = ID; SEMICOLON;
  {
  	Instr_call(id, [])
  }
| id = ID;
  delimited(OPEN_PARENTHESIS,
 			separated_nonempty_list(COMMA, expression),
 			CLOSE_PARENTHESIS);
  SEMICOLON;
  {
  	Instr_call(id, expr_l)
  }
| RETURN; expr = expression?; SEMICOLON;
  {
  	Instr_return(expr)
  }
| BEGIN; instr_l = instruction+; END; SEMICOLON;
  {
  	Instr_block(instr_l)
  }
| IF; if_expr = expression;
  THEN; then_instr_l = instruction+;
  elseif_l = list(ELSIF; sp = separated_pair(expression, THEN, instruction+) { sp });
  else_instr_l = option(ELSE; i = instruction+ { i });
  END; IF; SEMICOLON;
  {
  	let if_then = (if_expr, Instr_block(then_instr_l)) in 
  	let if_elseif = if_then :: elseif_l in

  	Instr_if(if_elseif, Instr_block(else_instr_l))
  }
| FOR; id = ID; IN; reverse_order = boption(REVERSE);
  expr_start = expression; TWO_DOTS; expr_end = expression;
  LOOP; instr_l = instruction+; END; LOOP; SEMICOLON;
  {
  	Instr_for(id, reverse_order, expr_start, expr_end, Instr_block(instr_l))
  }
| WHILE; while_expr = expression; LOOP;
  instr_l = instruction+; END; LOOP; SEMICOLON;
  {
  	Instr_while(while_expr, Instr_block(instr_l))
  }

operator:
| EQUAL; 		{ BinOp_equal }
| DIFFERENT; 	{ BinOp_different }
| COMPARATOR; 	{ BinOp_compare }
| PLUS; 		{ BinOp_plus }
| MINUS; 		{ BinOp_minus }
| TIMES; 		{ BinOp_multiply }
| DIV; 			{ BinOp_divide }
| REM; 			{ BinOp_remainder }
| AND; 			{ BinOp_and }
| AND_THEN; 	{ BinOp_andThen }
| OR; 			{ BinOp_or }
| OR_ELSE; 		{ BinOp_orElse }

access:
| id = ID; 							{ Acc_var(id) }
| expr = expression; DOT; id = ID; 	{ Acc_field(expression, id) }
