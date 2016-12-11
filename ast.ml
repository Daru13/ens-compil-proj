(* Syntaxe abstraite de Mini-ADA *)

type position = Lexing.position * Lexing.position
type ident = string

(* Type d'une opération d'inégalité *)
type comparator =
| Greater_than
| Greater_eq
| Less_than
| Less_eq

(* Exceptions utiles à l'analyseur syntaxique *)
exception Syntax_error of position * string
exception Unmatching_identifiers of position * string
     
(*****************************************************************************)
(*                          OPERATEURS ET EXPRESSIONS                        *)
(*****************************************************************************)

(* Opérateur binaire *)
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

(* Opérateur unaire *)
type unop =
| UnOp_not
| UnOp_negative

(* Expression (+ sa position) *)
type expression = {
	value : expr_value;
	pos : position
}

(* Correspond au non-terminal <access> dans la grammaire de Mini-ADA *)
and var_or_field =
| Acc_var of ident
| Acc_field of expression * ident

(* Types d'expressions disponibles *)
and expr_value =
| Expr_int of int
| Expr_char of char
| Expr_bool of bool
| Expr_null
| Expr_access of var_or_field
| Expr_binop of expression * binop * expression
| Expr_unop of unop * expression
| Expr_new of ident
| Expr_call of ident * expression list
| Expr_ascii of expression (* character'val *)

(*****************************************************************************)
(*               TYPES, PARAMETRES, DECLARATIONS ET INSTRUCTIONS             *)
(*****************************************************************************)

(* Type *)
type ty =
| Ty_var of ident
| Ty_access of ident

(* Liste d'identifiants d'un même type donné *)
type fields = ident list * ty

(* Mode de passage d'un argument *)
type mode =
| Mod_in
| Mod_inOut

(* Paramètre formel de fonction ou procédure *)
type param  = ident list * mode option * ty

(* Liste de paramètres *)
type params = param list

(* Déclaration (+ sa position) *)
and declaration = {
   value : decl_value;
   pos : position
}

(* Types de déclarations disponibles *)
and decl_value =
| Decl_type of ident
| Decl_access of ident * ident
| Decl_record of ident * fields list
| Decl_vars of ident list * ty * expression option 
| Decl_procedure of ident * params * declaration list * instruction list
| Decl_function of ident * params * ty * declaration list * instruction list

(* Constructeur Decl_vars :
 * Decl_vars reçoit, dans l'ordre :
 * 1. une liste d'identifiants de variables à déclarer
 * 2. le type de ces variables
 * 3. optionellement, une expression leur donnant une valeur initiale
 *)

(* Constructeurs Decl_procedure et Decl_function :
 * Decl_procedure reçoit, dans l'ordre :
 * 1. l'identifiant de la procédure
 * 2. la liste (potentiellement vide) des arguments formels
 * 3. la liste des déclarations locales
 * 4. la liste des instructions à effectuer
 * Idem pour Decl_function, mais le troisième paramètre (ty) indique
 * quel est le type de retour de la fonction déclarée
 *)

(* Liste d'instructions *)
and instr_list = instruction list

(* Instruction (+ sa position) *)
and instruction = {
  value : instr_value;
  pos : position
}

(* Types d'instructions disponibles *)
and instr_value =
| Instr_set of var_or_field * expression
| Instr_call of ident * expression list
| Instr_return of expression option
| Instr_block of instr_list (* begin ... end *)
| Instr_if of (expression * instr_list) list * instr_list
| Instr_for of ident * bool * expression * expression * instr_list
| Instr_while of expression * instr_list

(* Constructeur Instr_if :
 * Instr_if reçoit, dans l'ordre :
 * 1. une liste de couples expression à tester / liste d'instructions à
 *    exécuter si l'expression est vraie, à évaluer dans l'ordre.
 *    Cela correspond au if, et à tous les potentiels else if.
 * 2. une liste potentiellement vide d'instructions à exécuter sinon,
 *    qui correspond au else.
 *)

 (* Constructeur Instr_for :
 * Instr_for reçoit, dans l'ordre :
 * 1. l'identifiant de la variable sur laquelle itérer
 * 2. un booléen vrai si on itère à l'envers (en décrémentant)
 * 3. l'expression égale à la valeur initiale de ladite variable
 * 4. l'expression égale à la valeur finale de ladite variable
 * 5. une liste d'instructions à exécuter à chaque itération
 *)

(*****************************************************************************)
(*                            PROGRAMME MINI-ADA                             *)
(*****************************************************************************)

(* Racine de l'arbre de syntaxe abstraite *)
(* Dans l'ordre :
 * 1. id de la procédure principale
 * 2. liste des déclarations globales
 * 3. liste des instructions de cette procédure globale
 *)
type program = ident * declaration list * instruction list

