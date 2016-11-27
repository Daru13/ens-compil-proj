(* Syntaxe abstraite de Mini-ADA *)

type position = Lexing.position * Lexing.position
				  
type ident = string

type comparator =
| Greater_than
| Greater_eq
| Less_than
| Less_eq

     
(****** EXPRESSIONS ******)

type binop =
| BinOp_equal
| BinOp_different
| BinOp_compare of comparator
| BinOp_plus
| BinOp_minus
| BinOp_multiply
| BinOp_divide
| BinOp_remainder
| BinOp_and
| BinOp_andThen
| BinOp_or 
| BinOp_orElse

type unop =
| UnOp_not
| UnOp_negative
    
type expression = {
	value : expr_value;
	pos : position
}

and access_var =
| Acc_var of ident
| Acc_field of expression * ident

and expr_value =
| Expr_int of int
| Expr_char of char
| Expr_bool of bool
| Expr_null
| Expr_access of access_var
| Expr_binop of expression * binop * expression
| Expr_unop of unop * expression
| Expr_new of ident
| Expr_call of ident * expression list
| Expr_ascii of expression (* character'val *)



(****** DECLARATIONS AND INSTRUCTIONS ******)

type ty =
| Ty_var of ident
| Ty_access of ident
		 
type fields = ident list * ty

type mode =
| Mod_in
| Mod_inOut

type param  = ident list * mode option * ty
type params = param list

and declaration = {
   value : decl_value;
   pos : position
}

and decl_value =
| Decl_type of ident
| Decl_access of ident * ident
| Decl_record of ident * fields list
| Decl_vars of ident list * ident * expression option
| Decl_procedure of ident * params * declaration list * instruction list
| Decl_function of ident * params * ty * declaration list * instruction list

and instr_block = instruction list
and instruction = {
  value : instr_value;
  pos : position
}

and instr_value =
| Instr_set of access_var * expression
| Instr_call of ident * expression list
| Instr_return of expression option
| Instr_if of (expression * instr_block) list * instr_block
| Instr_for of ident * bool * expression * expression * instr_block
| Instr_while of expression * instr_block


(****** PROGRAMME MINI-ADA ******)

type program = ident * declaration list * instruction list

