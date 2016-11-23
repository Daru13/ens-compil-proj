
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
| ids_l = separated_nonempty_list(COMMA, ID); COLON; t = type;
  option(COLON_EQUAL; expr = expression); SEMICOLON;
  {

  }
| PROCEDURE; id_proc_1 = ID; param_l = parameters?; IS; decl_l = declaration*;
  BEGIN; instr_l = instruction+; END; id_proc_2 = ID?; SEMICOLON;
  {

  }
| FUNCTION; id_func_1 = ID; param_l = parameters?; IS;
  RETURN; t = type; IS; decl_l = declaration*;
  BEGIN; instr_l = instruction+; END; id_func_2 = ID?; SEMICOLON;
  {

  }

fields:
  ids_l = separated_nonempty_list(COMMA, ID); COLON; t = type; SEMICOLON;
  {

  }

type:
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
  COLON; m = mode?; t = type;
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
  