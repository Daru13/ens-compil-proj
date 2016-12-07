%{
	open Ast

	(* exception Syntax_error of position * string *)
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

%token ADA_TEXT_IO
%token GET_ASCII

%token ACCESS BEGIN ELSE ELSIF END FALSE FOR FUNCTION IF IN IS LOOP NEW NULL OUT PROCEDURE RECORD RETURN REVERSE THEN TRUE TYPE USE WHILE WITH

%token OR OR_ELSE
%token AND AND_THEN
%token NOT
%token EQUAL DIFFERENT
%token <Ast.comparator> COMPARATOR
%token PLUS MINUS
%token TIMES DIV REM
%token DOT

(* Priorités et associativités *)

%left OR OR_ELSE
%left AND AND_THEN
%nonassoc NOT
%left EQUAL DIFFERENT
%left COMPARATOR
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
%type <Ast.var_or_field> access

%start program

%%

program:
  WITH; ADA_TEXT_IO; SEMICOLON;
  USE; ADA_TEXT_IO; SEMICOLON;
  PROCEDURE; id_proc_1 = ID; IS; decl_l = declaration*;
  BEGIN; instr_l = instruction+; END; id_proc_2 = ID?; SEMICOLON;
  EOF;
  {
  		(* TODO : tester id_proc_2 si défini *)
  		(id_proc_1, decl_l, instr_l)
  }
| error
  {
  	let pos = ($symbolstartpos, $endpos) in
  	raise (Syntax_error (pos, "invalid program syntax"))
  }

declaration:
| TYPE; id = ID; SEMICOLON;
  {
  	let value = Decl_type(id) in
  	{value = value; pos = ($startpos, $endpos)}
  }
| TYPE; id_type = ID; IS; ACCESS; id_access = ID; SEMICOLON;
  {
  	let value = Decl_access(id_type, id_access) in
  	{value = value; pos = ($startpos, $endpos)}
  }
| TYPE; id_type = ID; IS; RECORD; fields_l = fields+; END; RECORD; SEMICOLON;
  {
  	let value = Decl_record(id_type, fields_l) in
  	{value = value; pos = ($startpos, $endpos)}
  }
| ids_l = separated_nonempty_list(COMMA, ID); COLON; t = ty;
  expr = option(COLON_EQUAL; e = expression { e }); SEMICOLON;
  {
  	let value = Decl_vars(ids_l, t, expr) in
  	{value = value; pos = ($startpos, $endpos)}
  }
| PROCEDURE; id_proc_1 = ID; opt_param_l = parameters?; IS; decl_l = declaration*;
  BEGIN; instr_l = instruction+; END; id_proc_2 = ID?; SEMICOLON;
  {
	let param_l = match opt_param_l with
	| Some(l) 	-> l
	| None 		-> []
	in

	let value = match id_proc_2 with
  	| Some(id) ->
	  	if id_proc_1 <> id then
	  		failwith ("Procedure identifiers do not match (" ^ id_proc_1 ^ " and " ^ id ^ ")")
	  	else
	  		Decl_procedure(id_proc_1, param_l, decl_l, instr_l)
	| None ->
		Decl_procedure(id_proc_1, param_l, decl_l, instr_l)
	in

	{value = value; pos = ($startpos, $endpos)}
  }
| FUNCTION; id_func_1 = ID; opt_param_l = parameters?;
  RETURN; t = ty; IS; decl_l = declaration*;
  BEGIN; instr_l = instruction+; END; id_func_2 = ID?; SEMICOLON;
  {
  	let param_l = match opt_param_l with
  	| Some(l) 	-> l
  	| None 		-> []
  	in

  	let value = match id_func_2 with
  	| Some(id) ->
	  	if id_func_1 <> id then
	  		failwith ("Function identifiers do not match (" ^ id_func_1 ^ " and " ^ id ^ ")")
	  	else
	  		Decl_function(id_func_1, param_l, t, decl_l, instr_l)
	| None ->
		Decl_function(id_func_1, param_l, t, decl_l, instr_l)
	in

	{value = value; pos = ($startpos, $endpos)}
  }
| error
  {
  	let pos = ($symbolstartpos, $endpos) in
  	raise (Syntax_error (pos, "invalid declaration syntax"))
  }

fields:
|  ids_l = separated_nonempty_list(COMMA, ID); COLON; t = ty; SEMICOLON;
  {
  	(ids_l, t)
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
|  param_l = delimited(OPEN_PARENTHESIS,
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

/*****************************************************************************/

expression:
| int = INT;
  {
  	let value = Expr_int(int) in
  	{value = value; pos = ($startpos, $endpos)}
  }
| c = CHAR;
  {
  	let value = Expr_char(c) in
  	{value = value; pos = ($startpos, $endpos)}
  }
| TRUE;
  {
  	let value = Expr_bool(true) in
  	{value = value; pos = ($startpos, $endpos)}
  }
| FALSE;
  {
  	let value = Expr_bool(false) in
  	{value = value; pos = ($startpos, $endpos)}	
  }
| NULL;
  {
  	let value = Expr_null in
  	{value = value; pos = ($startpos, $endpos)}
  }
| expr = delimited(OPEN_PARENTHESIS,
 				   expression,
 				   CLOSE_PARENTHESIS);
  {
  	expr
  }
| acc = access;
  {
  	let value = Expr_access(acc) in
  	{value = value; pos = ($startpos, $endpos)}
  }
| expr_1 = expression;
  op 	 = operator;
  expr_2 = expression;
  {
  	let value = Expr_binop(expr_1, op, expr_2) in
  	{value = value; pos = ($startpos, $endpos)}
  }
| NOT; expr = expression;
  {
  	let value = Expr_unop(UnOp_not, expr) in
  	{value = value; pos = ($startpos, $endpos)}
  }
| MINUS; expr = expression;
  {
  	let value = Expr_unop(UnOp_negative, expr) in
  	{value = value; pos = ($startpos, $endpos)}
  }
| NEW; id = ID;
  {
  	let value = Expr_new(id) in
  	{value = value; pos = ($startpos, $endpos)}
  }
| id = ID;
  expr_l = delimited(OPEN_PARENTHESIS,
 					 separated_nonempty_list(COMMA, expression),
 					 CLOSE_PARENTHESIS);
  {
  	let value = Expr_call(id, expr_l) in
  	{value = value; pos = ($startpos, $endpos)}
  }
| GET_ASCII;
  expr = delimited(OPEN_PARENTHESIS,
 				   expression,
 				   CLOSE_PARENTHESIS);
  {
  	let value = Expr_ascii(expr) in
  	{value = value; pos = ($startpos, $endpos)}
  }
| error
  {
  	let pos = ($symbolstartpos, $endpos) in
  	raise (Syntax_error (pos, "invalid expression syntax"))
  }

instruction:
| acc = access; COLON_EQUAL; expr = expression; SEMICOLON;
  {
  	let value = Instr_set(acc, expr) in
  	{value = value; pos = ($startpos, $endpos)}
  }
| id = ID; SEMICOLON;
  {
  	let value = Instr_call(id, []) in
  	{value = value; pos = ($startpos, $endpos)}
  }
| id = ID;
  expr_l = delimited(OPEN_PARENTHESIS,
 					 separated_nonempty_list(COMMA, expression),
 					 CLOSE_PARENTHESIS);
  SEMICOLON;
  {
  	let value = Instr_call(id, expr_l) in
  	{value = value; pos = ($startpos, $endpos)}
  }
| RETURN; expr = expression?; SEMICOLON;
  {
  	let value = Instr_return(expr) in
  	{value = value; pos = ($startpos, $endpos)}
  }
| BEGIN; instr_l = instruction+; END; SEMICOLON;
  {
  	let value = Instr_block(instr_l) in
  	{value = value; pos = ($startpos, $endpos)}
  }
| IF; if_expr = expression;
  THEN; then_instr_l = instruction+;
  elseif_l = list(ELSIF; sp = separated_pair(expression, THEN, instruction+) { sp });
  else_opt_instr_l = loption(ELSE; i = instruction+ { i });
  END; IF; SEMICOLON;
  {
  	let if_then = (if_expr, then_instr_l) in 
  	let if_elseif = if_then :: elseif_l in

  	let value = Instr_if(if_elseif, else_opt_instr_l) in 
  	{value = value; pos = ($startpos, $endpos)}
  }
| FOR; id = ID; IN; reverse_order = boption(REVERSE);
  expr_start = expression; TWO_DOTS; expr_end = expression;
  LOOP; instr_l = instruction+; END; LOOP; SEMICOLON;
  {
  	let value = Instr_for(id, reverse_order, expr_start, expr_end, instr_l) in
  	{value = value; pos = ($startpos, $endpos)}
  }
| WHILE; while_expr = expression; LOOP;
  instr_l = instruction+; END; LOOP; SEMICOLON;
  {
  	let value = Instr_while(while_expr, instr_l) in
  	{value = value; pos = ($startpos, $endpos)}
  }
| error
  {
  	let pos = ($symbolstartpos, $endpos) in
  	raise (Syntax_error (pos, "invalid instruction syntax"))
  }

%inline operator:
| EQUAL; 			{ BinOp_equal }
| DIFFERENT; 		{ BinOp_different }
| c = COMPARATOR; 	{ BinOp_compare(c) }
| PLUS; 			{ BinOp_plus }
| MINUS; 			{ BinOp_minus }
| TIMES; 			{ BinOp_multiply }
| DIV; 				{ BinOp_divide }
| REM; 				{ BinOp_remainder }
| AND; 				{ BinOp_and }
| AND_THEN; 		{ BinOp_andThen }
| OR; 				{ BinOp_or }
| OR_ELSE; 			{ BinOp_orElse }

// TODO: AndThen et OrElse !!

access:
| id = ID; 							{ Acc_var(id) }
| expr = expression; DOT; id = ID; 	{ Acc_field(expr, id) }
