(* Syntaxe abstraite de Mini-ADA *)

type ident = string

(****** PROGRAMME MINI-ADA ******)

type program = ident * declaration list * instruction list


(****** DECLARATIONS ******)

type declaration =
| Decl_type of ident
| Decl_access of ident * ident
| Decl_record of ident * field list
| Decl_vars of ident list * ident * Some expression
| Decl_procedure of ident * Some params * declaration list * instruction list
| Decl_function of ident * Some params * ty * declaration list * instruction list

type field = ident list * ty

type ty =
| Ty_var of ident
| Ty_access of ident

type params = (ident list, Some mode, ty) list

type mode =
| Mod_in
| Mod_inOut

(****** EXPRESSIONS ******)

type expression = {
	val : expr_value;
	pos : Lexing.position * Lexing.position
}

type expr_value =
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


(****** INSTRUCTIONS ******)

type instruction =
| Instr_set of access * expression
| Instr_call of ident * expression list
| Instr_return of Some expression
| Instr_if of (expression * instruction list) list
| Instr_for of ident * expression * expression * instruction list
| Instr_while of expression * instruction list

type access =
| Acc_var of ident
| Acc_field of expression * ident
