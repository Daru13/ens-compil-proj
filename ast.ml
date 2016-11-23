(* Syntaxe abstraite de Mini-ADA *)

type ident = string

type comparator =
  |Greater_than
  |Greater_eq
  |Less_than
  |Less_eq

     
(****** EXPRESSIONS ******)

type binop =
| BinOp_equal
| BinOp_different
| BinOp_compare of comparator
| BinOp_plus
| BinOp_minus
| BinOp_multiply
| BInOp_divide
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
	pos : Lexing.position * Lexing.position
}
 and expr_value =
| Expr_int of int
| Expr_char of char
| Expr_bool of bool
| Expr_null
| Expr_access of ident
| Expr_binop of expression * binop * expression
| Expr_unop of unop * expression
| Expr_new of ident
| Expr_call of ident * expression list
(* TODO : dernière option à ajouter ? *)


(****** DECLARATIONS AND INSTRUCTIONS ******)
type ty =
| Ty_var of ident
| Ty_access of ident
		 
type field = ident list * ty

type mode =
| Mod_in
| Mod_inOut

type params = (ident list * mode option * ty) list

type access =
| Acc_var of ident
| Acc_field of expression * ident
			      
and declaration =
| Decl_type of ident
| Decl_access of ident * ident
| Decl_record of ident * field list
| Decl_vars of ident list * ident * expression option
| Decl_procedure of ident * params * declaration list * instruction list
| Decl_function of ident * params * ty * declaration list * instruction list

and instruction =
| Instr_set of access * expression
| Instr_call of ident * expression list
| Instr_return of expression option
| Instr_if of (expression * instruction list) list
| Instr_for of ident * expression * expression * instruction list
| Instr_while of expression * instruction list



(****** PROGRAMME MINI-ADA ******)

type program = ident * declaration list * instruction list

