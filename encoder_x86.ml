open X86_64
open Ast
open Symbol_table

(*****************************************************************************)
(*                              VARIABLES GLOBALES                           *)
(*****************************************************************************)

(* Taille d'une adresse/case mémoire (?) en octet *)
let addr_size = 8

(* Opérande (registre %rax) utilisée pour les évaluations d'expressions *)
(* Note : si modifié, le code doit être mis à jour ! *)
let expr_reg = (reg rax)

(* Compteur utilisé pour les labels uniques (boucles, branchements...) *)
let label_unique_id = ref 0

(*****************************************************************************)
(*                                  UTILITAIRES                              *)
(*****************************************************************************)

(* Fonction temporaire, utile pour débug en renvoyant un type text
   représentant une instruction inutile, pour les parties de la production
   de code non-encore écrites *)
let void_instr () = movq (reg rax) (reg rax)

(* Création d'étiquette unique dont le préfixe est spécifié, et renvoyé
   suivi de label_unique_id, qui est incrémenté *)
let get_unique_label prefix =
	let new_label = prefix ^ (string_of_int !label_unique_id) in
	label_unique_id := !label_unique_id + 1;

	new_label
;;

(*****************************************************************************)
(*                          ACCESS AUX VARIABLES/CHAMPS                      *)
(*****************************************************************************)

(* TODO *)

(*****************************************************************************)
(*                                 EXPRESSIONS                               *)
(*****************************************************************************)

let encode_expr_int n =
	movslq (imm n) rax
;;

let encode_expr_char c =
	let char_int = int_of_char c in
	movslq (imm char_int) rax
;;

let encode_expr_bool b =
	let bool_int = match b with
	| true  -> 1
	| false -> 0
	in
	movslq (imm bool_int) rax
;;

let encode_expr_null () =
	movslq (imm 0) rax
;;

(* TODO *)
let rec encode_expr_access var_field =
	void_instr ()
(*
	let asm = void_instr in

	match var_field with
	| Acc_var (id) ->
		(* Propriétés utiles de la variable à laquelle accéder *)
		let nb_above_levels = 0 in
		let is_in_out = false in
		let is_param = false in
		let offset = 0 in
		let nb_params = 0 in

		(* Calcul de l'adresse de référence dans %r15 *)
		let asm = asm ++ movq (reg rbp) (reg r15) in
		for i = 0 to nb_above_levels do
			let asm = asm ++ (ind ~index:2 ~scale:addr_size r15) (reg r15)
		done

		(* Distinction des variables locales et des paramètres pour le
		   calcul de la case mémoire à traiter, dont le contenu est mis
		   dans le registre %rax *)
		let asm = asm ++ match is_param with
		| true -> (* TODO : scale = 1 selon calcul offset ! *)
			movq (ind ~ofs:(2 * addr_size) ~index:offset r15) (reg rax)

			(* Distinction des modes IN (copie) et IN_OUT (adresse) :
			   Si IN_OUT, alors il faut charger la valeur à l'adresse
			   contenue dans le registre %rax dans lui-même *)
			begin match is_in_out with
			| true ->
				(* TODO *)
			| false ->
				(* TODO *)
			end

		| false ->
			movq (ind ~index:offset r15) (reg rax)
		in

		asm

	| Acc_field (expr, id) ->
		(* L'évaluation de l'expression renvoit l'adresse de la première
		   case mémoire de l'enregistrement *)
		let asm = asm ++ encode_expression expr in

		(* Accès au i-ème champ TODO *)
		let offset = 0 in
		(* TODO *)
		asm
*)

and encode_expr_binop op expr_1 expr_2 =

	(* Registres contenant les évaluations des deux expressions *)
	let reg_expr_1 = (reg r15) in
	let reg_expr_2 = (reg rax) in

	(* Différentes évaluations des expressions, utilisées plus tard *)
	let asm_eval_expr = encode_expression expr_1
					 ++ movq expr_reg reg_expr_1
					 ++ encode_expression expr_2 in

	(* La ou les instructions à effectuer dépendent de l'opérateur binaire
	   Les booléens sur 8 bits sont systématiquement étendus sur 64 bits *)
	match op with
	| BinOp_equal ->
		asm_eval_expr ++
		cmpq reg_expr_1 reg_expr_2 ++
		sete (reg r15b) ++
		movsbq (reg r15b) rax

	| BinOp_different ->
		asm_eval_expr ++
		cmpq reg_expr_1 reg_expr_2 ++
		setne (reg r15b) ++
		movsbq (reg r15b) rax

	| BinOp_compare (comparator) ->
		(* Quatre types de comparaisons sont définis *)
		let asm_set = begin match comparator with
		| Greater_than -> setl (reg r15b)
		| Greater_eq -> setle (reg r15b)
		| Less_than -> setg (reg r15b)
		| Less_eq -> setge (reg r15b)
		end in
		cmpq reg_expr_1 reg_expr_2 ++
		asm_set ++
		movsbq (reg r15b) rax

	| BinOp_plus ->
		asm_eval_expr ++
		addq reg_expr_1 reg_expr_2

	| BinOp_minus ->
		asm_eval_expr ++
		subq reg_expr_1 reg_expr_2

	| BinOp_multiply ->
		asm_eval_expr ++
		imulq reg_expr_1 reg_expr_2

	| BinOp_divide ->
		(* TODO : simplifier cette étape ? quid de l'ordre d'évaluation ? *)
		asm_eval_expr ++

		movq reg_expr_2 (reg r14) ++
		movq reg_expr_1 (reg rax) ++
		cqto ++
		idivq (reg r14) (* quotient mis dans %rax = rax par idivq *)

	| BinOp_remainder ->
		(* TODO : simplifier cette étape ? quid de l'ordre d'évaluation ? *)
		asm_eval_expr ++

		movq reg_expr_2 (reg r14) ++
		movq reg_expr_1 (reg rax) ++
		cqto ++
		idivq (reg r14) ++
		movq (reg rdx) (reg rax)

	| BinOp_and ->
		asm_eval_expr ++
		addq reg_expr_1 reg_expr_2 ++
		cmpq (imm 2) reg_expr_2 ++
		sete (reg r15b) ++
		movsbq (reg r15b) rax

	| BinOp_andThen ->
		let and_label_false = get_unique_label "and_false_" in
		let and_label_end = get_unique_label "and_end_" in

		encode_expression expr_1 ++
		testq reg_expr_1 reg_expr_1 ++
		je and_label_false ++
		encode_expression expr_2 ++
		testq reg_expr_2 reg_expr_2 ++
		je and_label_false ++
		movq (imm 1) (reg rax) ++
		jmp and_label_end ++
		label and_label_false ++
		movq (imm 0) (reg rax) ++
		label and_label_end

	| BinOp_or ->
		asm_eval_expr ++
		addq reg_expr_1 reg_expr_2 ++
		setg (reg r15b) ++
		movsbq (reg r15b) rax

	| BinOp_orElse ->
		let or_label_false = get_unique_label "or_false_" in
		let or_label_end = get_unique_label "or_end_" in

		encode_expression expr_1 ++
		testq reg_expr_1 reg_expr_1 ++
		je or_label_false ++
		encode_expression expr_2 ++
		testq reg_expr_2 reg_expr_2 ++
		je or_label_false ++
		movq (imm 1) (reg rax) ++
		jmp or_label_end ++
		label or_label_false ++
		movq (imm 0) (reg rax) ++
		label or_label_end

and encode_expr_unop op expr =
	(* La ou les instructions à effectuer dépendent de l'opérateur unaire *)
	match op with
	| UnOp_not ->
		encode_expression expr ++
		cmpq (reg rax) (reg rax) ++
		cmovne (imm 0) (reg rax) ++
		cmove  (imm 1) (reg rax)

	| UnOp_negative ->
		encode_expression expr ++
		negq (reg rax)

(* TODO *)
and encode_expr_new id =
	let size_of_id = 0 in

	(* Appel de la fonction malloc de la bibliothèque standard pour allouer
	   de la mémoire sur le tas dont on renvoit l'adresse de la première case *)
	movq (imm size_of_id) (reg rdi) ++ (* convention de passage d'argument *)
	call "malloc"
	(* Par convention, la valeur de retour est déjà dans %rax *)

(* TODO *)
and encode_expr_call id expr_l =
	void_instr ()

and encode_expr_ascii expr =
	(* Seul l'octet de poids faible est retenu lors d'une transformation
	   en caractère ASCII (0-255) *)
	encode_expression expr ++ movsbq (reg al) rax

(* TODO : problème avec champs d'enregistrements portant le même nom ? *)
and encode_expression (expr: expression) =
	let expr = expr.value in 

	match expr with
	| Expr_int (n) ->
		encode_expr_int n

	| Expr_char (c) ->
		encode_expr_char c

	| Expr_bool (b) ->
		encode_expr_bool b

	| Expr_null ->
		encode_expr_null ()

	| Expr_access (var_field) ->
		encode_expr_access var_field

	| Expr_binop (expr_1, op, expr_2) ->
		encode_expr_binop op expr_1 expr_2

	| Expr_unop (op, expr) ->
		encode_expr_unop op expr

	| Expr_new (id) ->
		encode_expr_new id

	| Expr_call (id, expr_l) ->
		encode_expr_call id expr_l

	| Expr_ascii (expr) ->
		encode_expr_ascii expr
;;

(*****************************************************************************)
(*                             FONCTIONS/PROCEDURES                          *)
(*****************************************************************************)

(* TODO *)
(* A utiliser avant chaque appel de fonction, dans l'appelant *)
let save_context () = 
	void_instr ()
;;

(* TODO *)
(* A utiliser après chaque appel de fonction, dans l'appelé *)
let load_context () =
	void_instr ()
;;

(* TODO *)
(* Evalue et empile les expressions de la liste fournie *)
let push_arguments expr_l = 
	let rec push_arg text l =
		match l with
		| [] ->
			text
		| expr :: tail ->
			let text_expr = encode_expression expr in
			let new_text  = text
						 ++ text_expr
						 ++ (pushq expr_reg) in
			push_arg new_text tail;
	in

	(* TODO : init text ? *)
	push_arg (void_instr ()) expr_l
;;

(* TODO *)
(* Réserve l'espace nécessaire pour les variables locales d'une fonction *)
let alloc_local_space id =
	void_instr ()
;;

(* A utiliser au début de chaque définition de fonction *)
let enter_called_funct id =
	(* Empile le pointeur de pile courant %rsp, et met l'adresse de pile
	   courante das %rbp *)
	let text_push = pushq (reg rsp) in
	let text_movq = movq (reg rsp) (reg rbp) in

	(* Alloue de l'espace pour les variables locales *)
	let text_alloc = alloc_local_space id in

	text_push ++ text_movq ++ text_alloc
;;

(* A utiliser à la fin de chaque définition de fonction *)
let leave_called_funct id =
	(* Libère l'espace de pile utilisé et saute vers l'appelant *)
	(* TODO : vérifier libération mémoire arguments, via argu ret ? *)
	leave ++ ret
;;

(*****************************************************************************)
(*                                 INSTRUCTIONS                              *)
(*****************************************************************************)

let encode_instr_set var_field expr =
	let addr 	  = 0 (* TODO *) in
	let text_expr = encode_expression expr in
	let text_set  = movq expr_reg (ind ~ofs:addr ~scale:addr_size rbp) in

	text_expr ++ text_set
;;

let encode_instr_call id expr_l =
	(* TODO: cas particuliers des opérations de lecture/écriture ! *)

	let text_save = save_context () in

	(* Evalue puis empile chaque instruction formant un paramètre *)
	let text_args = push_arguments expr_l in

	(* Empile le pointeur de frame courant %rbp *)
	let text_push = pushq (reg rbp) in

	(* Empile l'adresse de retour (instruction suivante) et saute *)
	(* TODO : trouver le bon identifiant en cas de déf récursives ! *)
	let text_call = call id in

	(* Libération de la pile effectuée par l'appelant avec leave ? TODO *)
	let text_load = load_context () in

	text_save ++ text_push ++ text_call ++ text_load
;;

let encode_instr_return opt_expr =
	(* S'il y a une valeur de retour, elle est évaluée et placée dans un
	   registre défini par convention, en l'occurence %rax *)
	match opt_expr with
	| None ->
		(* TODO: vider pile locale, restaurer frame, puis ret *)
		ret
	| Some(expr) ->
		encode_expression expr ++ ret
;;

let rec encode_instr_list instr_l =
	(* Itération sur la liste des instructions à encoder *)
	List.fold_left (fun text instr -> text ++ (encode_instruction instr))
		(void_instr ()) instr_l

and encode_instr_block instr_l =
	encode_instr_list instr_l

and encode_instr_if if_test_instr_l else_instr_l =
	(* Création d'une étiquette unique de fin de branchement *)
	let if_label_next = get_unique_label "if_next" in

	(* Itération sur chaque couple condition-instructions *)
	let encode_branch asm (test_expr, instr_l) =
		let label_false = get_unique_label "if_false" in
		asm ++ encode_expression test_expr (* Test *)
			++ testq expr_reg expr_reg
			++ je label_false 			   (* Saut potentiel si faux *)
			++ encode_instr_list instr_l   (* Instructions de la branche *)
			++ jmp if_label_next 		   (* Saut vers la fin du branchement *)
			++ label label_false
	in
	let asm = List.fold_left encode_branch (void_instr ()) if_test_instr_l in

	(* Instructions du else, si existantes, et étiquette de fin *)
	let asm = asm ++ encode_instr_list else_instr_l
				  ++ label if_label_next in

	asm

and encode_instr_for id reverse begin_expr end_expr instr_l =
	(* Création de deux étiquettes uniques pour cette boucle *)
	let for_label_test = get_unique_label "for_test" in
	let for_label_next = get_unique_label "for_next" in

	(* Initialisation de la variable de boucle *)
	let addr = 0 (* TODO *) in

	let asm = encode_expression begin_expr
			++ movq expr_reg (ind ~ofs:addr ~scale:addr_size rbp) in

	(* Etiquette de début, test et saut potentiel *)
	let asm = asm ++ label for_label_test in
	let asm = asm ++ encode_expression end_expr
				  ++ movq expr_reg (reg r14)
				  ++ movq (ind ~ofs:addr ~scale:addr_size rbp) (reg r15)
				  ++ cmpq (reg r14) (reg r15)
				  ++ je for_label_next in

	(* Instructions répétées *)
	let asm = asm ++ encode_instr_list instr_l in

	(* Incrément ou décrément *)
	let asm = asm ++ movq (ind ~ofs:addr ~scale:addr_size rbp) (reg r15)
				  ++ if reverse then decq (reg r15) else incq (reg r15)
				  ++ movq (reg r15) (ind ~ofs:addr ~scale:addr_size rbp) in

	(* Saut vers le début et étiquette de fin *)
	let asm = asm ++ jmp for_label_test
				  ++ label for_label_next in

	asm

and encode_instr_while test_expr instr_l =
	(* Création de deux étiquettes uniques pour cette boucle *)
	let while_label_test = get_unique_label "while_test" in
	let while_label_next = get_unique_label "while_next" in

	(* Etiquette de début, test et saut potentiel *)
	let asm = label while_label_test in
	let asm = asm ++ encode_expression test_expr
				  ++ testq expr_reg expr_reg
				  ++ je while_label_next in

	(* Instructions répétées *)
	let asm = asm ++ encode_instr_list instr_l in

	(* Saut vers le début et étiquette de fin *)
	let asm = asm ++ jmp while_label_test
				  ++ label while_label_next in

	asm

and encode_instruction (instr:instruction) =
	let instr = instr.value in

	match instr with
	| Instr_set (var_field, expr) ->
		encode_instr_set var_field expr

	| Instr_call (id, expr_l) ->
		encode_instr_call id expr_l

	| Instr_return (opt_expr) ->
		encode_instr_return opt_expr

	| Instr_block (instr_l) ->
		encode_instr_block instr_l

	| Instr_if (if_test_instr_l, else_instr_l) ->
		encode_instr_if if_test_instr_l else_instr_l

	| Instr_for (id, reverse, begin_expr, end_expr, instr_l) ->
		encode_instr_for id reverse begin_expr end_expr instr_l

	| Instr_while (test_expr, instr_l) ->
		encode_instr_while test_expr instr_l
;;

(*****************************************************************************)
(*                                 DECLARATIONS                              *)
(*****************************************************************************)

(* Type utilisé pour représenter une fonction/procédure à encoder en x86,
   contenant les informations suivantes :
   - un identifiant (ident) et un préfixe (ident)
   - une table des symboles (sym_table)
   - une liste d'instructions à effectuer (instr_list)
*)
type f_decl = ident * ident * sym_table * instr_list

(* Liste de fonctions/procédures à encoder en x86 *)
let function_to_encode_l = ref []

let add_function_to_encode (f:f_decl) =
	function_to_encode_l := f :: !function_to_encode_l
;;

(* Type du contexte utilisé lors de l'ajout des déclarations
   Il permet de conserver certaines informations utiles pour ajouter
   des symboles ou des déclarations à encoder :
   - un préfixe d'identifiant de fonction/procédure (indent)
   - le niveau de profondeur actuel (level)
   - la taille de l'offset courant pour les variables locales (offset)
   - la table des symboles actuelle (sym_table)
*)
type decl_context = {
	prefix 		: ident;
	level 		: level;
	local_offset: offset;
	symbols 	: sym_table
}

(*****************************************************************************)

let encode_decl_type id context =
	let type_size  = 0 in (* TODO *)
	let new_symbol = (id, (Sym_type type_size)) in

	{
		prefix 		 = context.prefix;
		level 		 = context.level;
		local_offset = context.local_offset;
		symbols 	 = add_symbol context.symbols new_symbol
	}
;;

let encode_decl_access id id_type context =
	(* TODO ? *)
	context
;;

let encode_decl_record id fields_l context =
	(* TODO ? *)
	context
;;

(* TODO : expression inutile ici ? ou pas ? *)
let encode_decl_vars id_l t opt_expr context =
	let offset    = ref context.local_offset in
	let type_size = 0 in (* TODO *)

	(* Ajout de chaque variable comme nouveau symbole *)
	let add_var_as_symbol symbols id =
		let var_sym = (id, Sym_variable (Var_local, context.level,
										 type_size, !offset)) in
		offset := !offset + type_size; (* offset mis à jour *)

		add_symbol symbols var_sym
	in
	let symbols = List.fold_left add_var_as_symbol
								 context.symbols
								 id_l in

	{
		prefix 		 = context.prefix;
		level 		 = context.level;
		local_offset = !offset;
		symbols 	 = symbols
	}
;;

let encode_decl_procedure id params decl_l instr_l context =
	(* TODO *)
	context
;;

let encode_decl_function id params t decl_l instr_l context =
	(* TODO *)
	context
;;

let encode_declaration (decl:declaration) context =
	let decl = decl.value in

	match decl with
	| Decl_type (id) ->
		encode_decl_type id context

	| Decl_access (id, id_type) ->
		encode_decl_access id id_type context

	| Decl_record (id, fields_l) ->
		encode_decl_record id fields_l context

	| Decl_vars (id_l, t, opt_expr)  ->
		encode_decl_vars id_l t opt_expr context

	| Decl_procedure (id, params, decl_l, instr_l) ->
		encode_decl_procedure id params decl_l instr_l context

	| Decl_function (id, params, t, decl_l, instr_l) ->
		encode_decl_function id params t decl_l instr_l context
;;

