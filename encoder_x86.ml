open X86_64
open Ast

(*****************************************************************************)
(*                              VARIABLES GLOBALES                           *)
(*****************************************************************************)

(* Taille d'une adresse/case mémoire (?) en octet *)
let addr_size = 8 ;;

(* Registre dans lequel se trouve le résultat d'une évaluation d'expression *)
let expr_reg = (reg rax) ;;

(* Compteur utilisé pour les labels uniques (boucles, branchements...) *)
let label_unique_id = ref 0 ;;

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

(* TODO *)

let encode_expression expr =
	void_instr ()
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
	(* Empile le pointeur de frame courant %rbp, puis le rend égal à %rsp *)
	let text_push = pushq (reg rbp) in
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
	void_instr ()
;;

let rec encode_instr_block instr_l =
	let rec eval_instr text l =
		match l with
		| [] ->
			text
		| instr :: tail ->
			let new_text = text ++ (encode_instruction instr.value) in
			eval_instr new_text tail
	in

	(* TODO : init text ? *)
	eval_instr (void_instr ()) instr_l

and encode_instr_if tests_instr_l else_instr_l =
	void_instr ()

and encode_instr_for id reverse begin_expr end_expr instr_l =
	(* Création d'une étiquette unique pour cette boucle *)
	let for_label = get_unique_label "for" in

	(* Initialisation de la variable de boucle *)
	let addr = 0 (* TODO *) in

	let text_begin = encode_expression begin_expr
				  ++ movq expr_reg (ind ~ofs:addr ~scale:addr_size rbp) in

	(* Etiquette de début de boucle et instructions répétées *)
	let text_label = label for_label in
	let text_instr =
		List.fold_left (fun text instr -> text ++ (encode_instruction instr.value))
		text_label instr_l in

	(* Incrément ou décrément *)
	let text_update = movq (ind ~ofs:addr ~scale:addr_size rbp) (reg r15)
				   ++ if reverse then decq (reg r15) else incq (reg r15)
				   ++ movq (reg r15) (ind ~ofs:addr ~scale:addr_size rbp) in

	(* Comparaison et saut potentiel *)
	let text_end = encode_expression end_expr
				++ movq expr_reg (reg r14) in

	let text_jump = cmpq (reg r14) (reg r15)
				 ++ jne for_label in

	text_begin ++ text_label ++ text_instr
	++ text_update ++ text_end ++ text_jump

and encode_instr_while test_expr instr_l =
	void_instr ()

and encode_instruction instr =
	match instr with
	| Instr_set (var_field, expr) ->
		encode_instr_set var_field expr

	| Instr_call (id, expr_l) ->
		encode_instr_call id expr_l

	| Instr_return (opt_expr) ->
		encode_instr_return opt_expr

	| Instr_block (instr_l) ->
		encode_instr_block instr_l

	| Instr_if (tests_instr_l, else_instr_l) ->
		encode_instr_if tests_instr_l else_instr_l

	| Instr_for (id, reverse, begin_expr, end_expr, instr_l) ->
		encode_instr_for id reverse begin_expr end_expr instr_l

	| Instr_while (test_expr, instr_l) ->
		encode_instr_while test_expr instr_l
;;
