open X86_64
open Ast
open Typer
open Symbol_table
       
(*****************************************************************************)
(*                              VARIABLES GLOBALES                           *)
(*****************************************************************************)

(* Taille d'une adresse/case mémoire (?) en octet *)
let addr_size = 8

(* Séparateur utilisé dans les labels de fonctions *)
let sep = 'I'

let int_size = 4 (* pour l'instant les fonctions sont robustes au changement de int_size *)
(* Opérande (registre %rax) utilisée pour les évaluations d'expressions *)
(* Note : si modifié, le code doit être mis à jour ! *)
let expr_reg = (reg rax)

(* Compteur utilisé pour les labels uniques (boucles, branchements...) *)
let label_unique_id = ref 0

(*****************************************************************************)
(*                                  UTILITAIRES                              *)
(*****************************************************************************)
(* Exponnentiation d'asm *)

let repeat asm n =
  let rec aux asm n acc = match n with
    |0 -> acc
    |_ -> aux asm (n-1) (asm++acc) in
  aux asm n nop;;
  
(* Fonction temporaire, utile pour débug en renvoyant un type text
   représentant une instruction inutile, pour les parties de la production
   de code non-encore écrites *)
let void_instr () = (* movq (reg rax) (reg rax) *) nop


(* search qui renvoie le label de la fonction cherchée *)

let defix label = String.sub label 0 (String.rindex label sep);;

let rec get_fun_lab id caller_lab map_l = match map_l with
  |[] -> raise Not_found
  |hd::tl -> try (
	       match Tmap.find id hd with
	       |Val(Function(_,_)),_ -> caller_lab ^ (Char.escaped sep) ^ id
	       |_ -> failwith "typing shouldn't have been ok") with
	     |Not_found -> get_fun_lab id (defix caller_lab) tl
;;
  
  
(* Création d'étiquette unique dont le préfixe est spécifié, et renvoyé
   suivi de label_unique_id, qui est incrémenté *)
let get_unique_label prefix =
	let new_label = prefix ^ (string_of_int !label_unique_id) in
	label_unique_id := !label_unique_id + 1;

	new_label
;;

(* map qui concatène les listes. Tail-recursive. *)
let op_map l f =
  let rec aux2 l_y acc = match l_y with
    |[] -> acc
    |hd::tl -> aux2 tl (hd::acc) in
  let rec aux l acc = match l with
    |[] -> acc
    |hd::tl -> aux tl (aux2 (f hd) acc) in
  List.rev (aux l []);;

let add_to_dll decl dll =
	match dll with
	| hd :: tl -> (hd @ [decl]) :: tl
	| [] -> [[decl]]
;;

(*****************************************************************************)
(*                          ACCESS AUX VARIABLES/CHAMPS                      *)
(*****************************************************************************)
type signature = (ident*expr_type*bool) list * expr_type;;

let rec size_exp_type et = match et with (* Donne la taille d'un élément de type "et", 
  en octets *)
  |Prim _ -> int_size
  |Access _ -> addr_size
  |Record l -> List.fold_left (fun s (id,t) -> s+(size_exp_type t)) 0 l
  |Function _ -> failwith "size of fun...l58";;
(*
let locate_field expr id map_l =
  let rec locate_field_in_list l id map_l = match l with
    |[] -> raise Not_found
    |(i,et)::tl when i = id -> 0
    |(i,et)::tl -> (size_exp_type et)+(locate_field_in_list tl id map_l)
  match type_expr expr map_l with
  |Record l -> 
 *)
let locate_arg arg ((l,t):signature) =
  (* Renvoie le nombre d'octets à remonter, depuis le premier argument rencontré,
     pour retrouver l'argument d'identifiant arg *)
  let rec aux l acc = match l with
    |(i,t,b)::tl when i = arg -> Some(acc,b)
    |(i,t,b)::tl -> aux tl (acc - 1)
    |[] -> None in
  aux l (List.fold_left (fun s (id,t,b) -> s+(size_exp_type t)) 0 l);;

let locate_var id (dl:declaration list) ml =
  (* Renvoie le nombre d'octets à descendre, depuis la première variable locale
     rencontrée, pour retrouver celle d'identifiant id *)
  let decl_filt (d:declaration) = match d.value with
    |Decl_type _ -> []
    |Decl_access _ -> []
    |Decl_record _ -> []
    |Decl_procedure _ -> []
    |Decl_function _ -> []
			  (* Ces decls ne mènent pas à l'affectation de 
    quoi que ce soit *)
    |Decl_vars (idl,ty,eo) ->
      match ty with
      |Ty_access _ -> List.map (fun id -> (id,addr_size)) idl
      |Ty_var id ->
	match search id ml with
	|Some(Type r,_) -> begin match !r with
			 |None -> failwith "typing went bad"
			 |Some(et) -> let size = size_exp_type et in
				      List.map (fun id -> (id,size)) idl end
	|_ -> failwith "typing went bad" in  
  let rec aux2 properl sacc = match properl with
    |[] -> (false,sacc)
    |(i,s)::tl when i = id -> (true,sacc)
    |(i,s)::tl -> aux2 tl (sacc+s) in
  let rec aux l acc = match l with
    |[] -> None
    |hd::tl -> match (aux2 (decl_filt hd) 0) with
	       |(true,n) -> Some(acc+n)
	       |(false,n) -> aux tl (acc+n) in
  aux dl 0;;

  (* Pour localiser un identifiant totalement, on doit pouvoir remonter. On 
suppose qu'on a une liste de liste de déclarations, et une liste de signatures *)

let locate_any id ml dll sl =
  (* Trouve les cordonnées (i,j) de la variable id la plus proche. i représente
la hauteur de contexte, j la distance du pointeur rbp *)
  let rec aux id ml dll sl height = match sl with
    |sign::stl ->
      begin
	match (locate_arg id sign) with
	|Some (n,b) -> (height,n+3,b)
	|None -> match dll with
		 |dl::dtl ->begin match (locate_var id dl ml) with
			     |Some(n) -> (height,(-1-n),true)
			     |None -> aux id ml dtl stl (height+1) end
		 |[] -> failwith "shouldn't happen right ? l123" end
    |[] -> (* Top context *)
      match dll with
      |dl::dtl ->begin match (locate_var id dl ml) with
		       |Some(n) -> (height,(-1-n),true)
		       |None -> failwith ("id : "^id^" is unlocatable") end
      |[] -> failwith "shouldn't happen right ? l129" in
  aux id ml dll sl 0;;
      
(* TODO *)

(*****************************************************************************)
(*                                 EXPRESSIONS                               *)
(*****************************************************************************)

let encode_expr_int n =
	movq (imm n) (reg rax)
;;

let encode_expr_char c =
	let char_int = int_of_char c in
	movq (imm char_int) (reg rax)
;;

let encode_expr_bool b =
	let bool_int = match b with
	| true  -> 1
	| false -> 0
	in
	movq (imm bool_int) (reg rax)
;;

let encode_expr_null () =
	movq (imm 0) (reg rax)
;;

(* TODO *)
let rec encode_expr_access var_field (ml,sl,dll) caller_lab =
	match var_field with
	| Acc_var (id) -> begin
	   match search id ml with
	   |Some(Val(Function([],t)),b) -> encode_expr_call id [] (ml,sl,dll) caller_lab
	   |_ ->
		(* Propriétés utiles de la variable à laquelle on veut accéder *)
	     let nb_above_levels = 0 in (* TODO *)
	     let is_param_in_out = false in (* TODO *)
	     let offset = 0 in (* TODO *)
	     
	     (* Calcul de l'adresse de référence dans %r15 *)
	     let asm = comment ("\tVar access to " ^ id)
		       ++ movq (reg rbp) (reg r15) ++
			 repeat (movq (ind ~ofs: (2 * addr_size) r15) (reg r15))
				nb_above_levels in
	     
	     (* S'il s'agit d'une variable locale ou d'un paramètre IN,
		   il suffit de récupérer le contenu de la case mémoire placée en
		   %r15 + offset.

		   S'il s'agit d'un paramètre IN_OUT, alors c'est la valeur de
		   la case mémoire pointée par celle en %r15 + offset
		   qu'il faut renvoyer*)
	     asm ++ begin match is_param_in_out with
			  | true ->
			     movq (ind ~ofs: offset r15) (reg rax) ++
			       movq (ind rax) expr_reg
			  | false -> 
			     movq (ind ~ofs: offset r15) expr_reg
		    end
			end 
	| Acc_field (expr, id) ->
		(* Non supporté à l'heure actuelle... *)
		nop

and encode_expr_binop op expr_1 expr_2 (ml,sl,dll) caller_lab =

	(* Registres contenant les évaluations des deux expressions *)
	let reg_expr_1 = (reg r15) in
	let reg_expr_2 = (reg rax) in

	(* Différentes évaluations des expressions, utilisées plus tard *)
	let asm_eval_expr = encode_expression expr_1 (ml,sl,dll) caller_lab
					 ++ movq expr_reg reg_expr_1
					 ++ encode_expression expr_2 (ml,sl,dll) caller_lab in

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
		let and_label_false = get_unique_label "And_false_" in
		let and_label_end = get_unique_label "And_end_" in

		encode_expression expr_1 (ml,sl,dll) caller_lab ++
		testq reg_expr_1 reg_expr_1 ++
		je and_label_false ++
		encode_expression expr_2 (ml,sl,dll) caller_lab++
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
		let or_label_false = get_unique_label "Or_false_" in
		let or_label_end = get_unique_label "Or_end_" in

		encode_expression expr_1 (ml,sl,dll) caller_lab ++
		testq reg_expr_1 reg_expr_1 ++
		je or_label_false ++
		encode_expression expr_2 (ml,sl,dll) caller_lab ++
		testq reg_expr_2 reg_expr_2 ++
		je or_label_false ++
		movq (imm 1) (reg rax) ++
		jmp or_label_end ++
		label or_label_false ++
		movq (imm 0) (reg rax) ++
		label or_label_end

and encode_expr_unop op expr (ml,sl,dll) caller_lab=
	(* La ou les instructions à effectuer dépendent de l'opérateur unaire *)
	match op with
	| UnOp_not ->
		encode_expression expr (ml,sl,dll) caller_lab ++
		movq (imm 0) (reg r14) ++
		movq (imm 1) (reg r15) ++
		cmpq (reg rax) (reg rax) ++
		cmovne (reg r14) (reg rax) ++
		cmove  (reg r15) (reg rax)

	| UnOp_negative ->
		encode_expression expr (ml,sl,dll) caller_lab++
		negq (reg rax)

and encode_expr_new id =
	let size_of_id = addr_size in (* TODO : mémoire à alloure, en octets *)

	(* Appel de la fonction malloc de la bibliothèque standard pour allouer
	   de la mémoire sur le tas dont on renvoit l'adresse de la première case *)
	movq (imm size_of_id) (reg rdi) ++ (* convention de passage d'argument *)
	call "malloc"
	(* Par convention, la valeur de retour est déjà dans %rax *)

and encode_expr_call id expr_l (ml,sl,dll) caller_lab =
	let asm = comment ("\tExpression call to " ^ id)
		   ++ save_registers () in
	let function_label = get_fun_lab id caller_lab ml in
	
	let asm = asm ++
		(* Evalue puis empile chaque instruction formant un paramètre *)
		(push_arguments expr_l (ml,sl,dll) caller_lab) ++
		(* Empile le pointeur de frame courant *)
		pushq (reg rbp) ++
		(* Empile l'adresse de retour (instruction suivante) et saute *)
		call function_label ++
		(* Dépile le pointeur de frame empilé avant l'appel *)
		popq r15
		
		(* TODO : dépiler les arguments mis sur la place *)
	in

	(* Restauration du contexte sauvegardé *)
	let asm = asm ++ load_registers () in

	asm

and encode_expr_ascii expr (ml,sl,dll) caller_lab =
	(* Seul l'octet de poids faible est retenu lors d'une transformation
		 en caractère ASCII (0-255) *)
	encode_expression expr (ml,sl,dll) caller_lab ++ movsbq (reg al) rax

(* TODO : problème avec champs d'enregistrements portant le même nom ? *)
and encode_expression (expr: expression) (ml,sl,dll) caller_lab =
	match expr.value with
	| Expr_int (n) ->
		encode_expr_int n

	| Expr_char (c) ->
		encode_expr_char c

	| Expr_bool (b) ->
		encode_expr_bool b

	| Expr_null ->
		encode_expr_null ()

	| Expr_access (var_field) ->
		encode_expr_access var_field (ml,sl,dll) caller_lab

	| Expr_binop (expr_1, op, expr_2) ->
		encode_expr_binop op expr_1 expr_2 (ml,sl,dll) caller_lab

	| Expr_unop (op, expr) ->
		encode_expr_unop op expr (ml,sl,dll) caller_lab

	| Expr_new (id) ->
		encode_expr_new id

	| Expr_call (id, expr_l) ->
	        encode_expr_call id expr_l (ml,sl,dll) caller_lab

	| Expr_ascii (expr) ->
	   encode_expr_ascii expr (ml,sl,dll) caller_lab


(*****************************************************************************)
(*                             FONCTIONS/PROCEDURES                          *)
(*****************************************************************************)

(* Evalue et empile les expressions de la liste fournie *)
and push_arguments expr_l (ml,sl,dll) caller_lab = 
	let push_arg asm expr =
		let is_arg_in_out = false in (* TODO *)

		(* Si le paramètre est IN_OUT, on place son adresse sur la pile,
		   et non sa valeur ! *)
		match is_arg_in_out with
		| true ->
			(* Propriétés utiles de la variable à laquelle on veut accéder *)
			let nb_above_levels = 0 in (* TODO *)
			let offset = 0 in (* TODO *)

			(* Calcul de l'adresse de référence dans %r15 *)
			let asm = movq (reg rbp) (reg r15) ++
			repeat (movq (ind ~ofs: (2 * addr_size) r15) (reg r15))
				   nb_above_levels in

			(* Adresse de la variable IN OUT *)
			asm ++ addq (imm offset) (reg r15)
				++ pushq (reg r15)

		| false ->
			encode_expression expr (ml,sl,dll) caller_lab ++
			pushq (reg rax)
	in

	comment "\tArgs are pushed (for a fct call)" ++
	List.fold_left push_arg (void_instr ()) expr_l

(* A utiliser avant chaque appel de fonction, dans l'appelant *)
and save_registers () = 
	(* Aucun registre utile à sauvegarder, mais la fonction est
	   présente au cas où ! *)
	nop

(* A utiliser après chaque appel de fonction, dans l'appelé *)
and load_registers () =
	(* Aucun registre utile à sauvegarder, mais la fonction est
	   présente au cas où ! *)
	nop
;;

(* Réserve l'espace nécessaire pour les variables locales d'une fonction *)
let alloc_local_space id =
	let local_space_size = 0 in (* TODO *)
	subq (imm local_space_size) (reg rsp)
;;

(* A utiliser au début de chaque définition de fonction *)
let enter_called_funct id =
	(* Empile le pointeur de pile courant %rsp, et met l'adresse de pile
	   courante das %rbp *)
	let asm = comment ("\tEnter definition of fct " ^ id)
		   ++ pushq (reg rsp)
		   ++ movq (reg rsp) (reg rbp) in

	(* Alloue de l'espace pour les variables locales (+ variables de boucles) *)
	asm ++ alloc_local_space id
;;

(* A utiliser à la fin de chaque définition de fonction *)
let leave_called_funct =
	(* Libère l'espace de pile utilisé et saute vers l'appelant *)
	movq (reg rbp) (reg rsp) ++ popq r15 ++ ret
;;

(*****************************************************************************)
(*                                 INSTRUCTIONS                              *)
(*****************************************************************************)
let encode_instr_set var_field expr (ml,sl,dll) caller_lab =
  match var_field with
  | Acc_field (e,id) -> nop
	  (* let asm = encode_expression e (ml,sl,dll) caller_lab in
			     
		 let text_expr = encode_expression expr in
		 let text_set  = movq expr_reg (ind ~ofs:addr ~scale:addr_size rbp) in

		 text_expr ++ text_set
	  *)
  | Acc_var id ->
	  	let (lvl,offset,b) = locate_any id ml dll sl in
		let asm = encode_expression expr (ml,sl,dll) caller_lab in
		let asm = asm ++ movq (reg rbp) (reg r15) in
		let asm = asm
			   ++ repeat (movq (ind ~ofs: (2 * addr_size) r15) (reg r15)) lvl in

		asm ++ match (offset > 0) with
			   | true ->
					movq (ind ~ofs: offset r15) (reg r15)
					++ movq (reg rax) (ind ~ofs: offset r15)
			   | false ->
					movq (reg rax) (ind ~ofs: offset r15)
		 
;;

let encode_instr_call id expr_l (ml,sl,dll) caller_lab =
	let asm = comment ("\tCall function " ^ id) in

	(* Sauvegarde du contexte *)
	let asm = asm ++ save_registers () in

	(* Gestion des fonctions préféfinies *)
	let asm = asm ++ match id with
	| "put" ->
		(* Evaluation de l'entier à afficher, déplacement dans %rdi,
		   et appel de la putchar() *)
		encode_expression (List.hd expr_l) (ml,sl,dll) caller_lab ++
		movq expr_reg (reg rdi) ++
		call "putchar"

	| "new_line" ->
		(* Appel de printf() avec l'adresse d'une chaîne globale contenant
		   un simple saut de ligne *)
		movq (ilab "NewLine") (reg rdi) ++
		movq (imm 0) (reg rax) ++
		call "printf"

	| _ ->
		let function_label = get_fun_lab id caller_lab ml in

		asm ++
		(* Evalue puis empile chaque instruction formant un paramètre *)
		(push_arguments expr_l (ml,sl,dll) caller_lab) ++
		(* Empile le pointeur de frame courant *)
		pushq (reg rbp) ++
		(* Empile l'adresse de retour (instruction suivante) et saute *)
		call function_label ++
		(* Dépile le pointeur de frame empilé avant l'appel *)
		popq r15

		(* TODO : dépiler les arguments mis sur la place *)
	in

	(* Restauration du contexte sauvegardé *)
	let asm = asm ++ load_registers () in

	asm
;;

let encode_instr_return opt_expr (ml,sl,dll) caller_lab =
	(* S'il y a une valeur de retour, elle est évaluée et placée dans un
	   registre défini par convention, en l'occurence %rax *)
	match opt_expr with
	| None ->
		leave_called_funct
	| Some(expr) ->
	   (encode_expression expr (ml,sl,dll) caller_lab)
	   ++ leave_called_funct
;;

let rec encode_instr_list instr_l (ml,sl,dll) caller_lab =
	(* Itération sur la liste des instructions à encoder *)
	List.fold_left
		(fun text instr -> text ++ (encode_instruction instr (ml,sl,dll) caller_lab))
		(void_instr ())
		instr_l
	
and encode_instr_block instr_l (ml,sl,dll) caller_lab =
	encode_instr_list instr_l (ml,sl,dll) caller_lab

and encode_instr_if if_test_instr_l else_instr_l (ml,sl,dll) caller_lab =
	(* Création d'une étiquette unique de fin de branchement *)
	let if_label_next = get_unique_label "If_next" in

	let asm = comment "\tIf branch begins here" in

	(* Itération sur chaque couple condition-instructions *)
	let encode_branch asm (test_expr, instr_l) =
		let label_false = get_unique_label "If_false" in

		asm ++ encode_expression test_expr (ml,sl,dll) caller_lab (* Test *)
			++ testq expr_reg expr_reg
			(* Saut potentiel si faux *)
			++ je label_false 			   
		   (* Instructions de la branche *)
		    ++ encode_instr_list instr_l (ml,sl,dll) caller_lab
			(* Saut vers la fin du branchement *)
			++ jmp if_label_next 		   
			++ label label_false
	in
	let asm = List.fold_left encode_branch asm if_test_instr_l in

	(* Instructions du else, si existantes, et étiquette de fin *)
	let asm = asm ++ encode_instr_list else_instr_l (ml,sl,dll) caller_lab
				  ++ label if_label_next in

	asm

and encode_instr_for id reverse begin_expr end_expr instr_l (ml,sl,dll) caller_lab =
	(*
	(* Création de deux étiquettes uniques pour cette boucle *)
	let for_label_test = get_unique_label "For_test" in
	let for_label_next = get_unique_label "For_next" in

	(* Initialisation de la variable de boucle *)
	let addr = 0 (* TODO *) in

	let asm = encode_expression begin_expr (ml,sl,dll) caller_lab
			++ movq expr_reg (ind ~ofs:addr ~scale:addr_size rbp) in

	(* Etiquette de début, test et saut potentiel *)
	let asm = asm ++ label for_label_test in
	let asm = asm ++ encode_expression end_expr (ml,sl,dll) caller_lab
				  ++ movq expr_reg (reg r14)
				  ++ movq (ind ~ofs:addr ~scale:addr_size rbp) (reg r15)
				  ++ cmpq (reg r14) (reg r15)
				  ++ je for_label_next in

	(* Instructions répétées *)
	let asm = asm ++ encode_instr_list instr_l (ml,sl,dll) caller_lab in

	(* Incrément ou décrément *)
	let asm = asm ++ movq (ind ~ofs:addr ~scale:addr_size rbp) (reg r15)
				  ++ if reverse then decq (reg r15) else incq (reg r15)
				  ++ movq (reg r15) (ind ~ofs:addr ~scale:addr_size rbp) in

	(* Saut vers le début et étiquette de fin *)
	let asm = asm ++ jmp for_label_test
				  ++ label for_label_next in

	asm *) nop

and encode_instr_while test_expr instr_l (ml,sl,dll) caller_lab =
	(* Création de deux étiquettes uniques pour cette boucle *)
	let while_label_test = get_unique_label "While_test" in
	let while_label_next = get_unique_label "While_next" in

	(* Etiquette de début, test et saut potentiel *)
	let asm = comment "\tWhile loop begins here"
		   ++ label while_label_test in
	let asm = asm ++ encode_expression test_expr (ml,sl,dll) caller_lab
				  ++ testq expr_reg expr_reg
				  ++ je while_label_next in

	(* Instructions répétées *)
	let asm = asm ++ encode_instr_list instr_l (ml,sl,dll) caller_lab in

	(* Saut vers le début et étiquette de fin *)
	let asm = asm ++ jmp while_label_test
				  ++ label while_label_next in

	asm

and encode_instruction (instr:instruction) (ml,sl,dll) caller_lab =
	match instr.value with
	| Instr_set (var_field, expr) ->
		encode_instr_set var_field expr (ml,sl,dll) caller_lab

	| Instr_call (id, expr_l) ->
		encode_instr_call id expr_l (ml,sl,dll) caller_lab

	| Instr_return (opt_expr) ->
		encode_instr_return opt_expr (ml,sl,dll) caller_lab

	| Instr_block (instr_l) ->
		encode_instr_block instr_l (ml,sl,dll) caller_lab

	| Instr_if (if_test_instr_l, else_instr_l) ->
		encode_instr_if if_test_instr_l else_instr_l (ml,sl,dll) caller_lab

	| Instr_for (id, reverse, begin_expr, end_expr, instr_l) ->
		encode_instr_for id reverse begin_expr end_expr instr_l (ml,sl,dll) caller_lab

	| Instr_while (test_expr, instr_l) ->
		encode_instr_while test_expr instr_l (ml,sl,dll) caller_lab
;;

(*****************************************************************************)
(*                             ENCODAGE DU PROGRAMME                         *)
(*****************************************************************************)

(*
let set_all_functions_to_encode (p_id, p_decl_l, p_instr_l) context_tree =
	let symbols = get_empty_symbol_table () in
	let root_prefix = "" in

	(* Procédure principale à encoder *)
	let main_procedure_to_encode = (p_id, root_prefix, symbols, p_instr_l, nop) in
	add_function_to_encode main_procedure_to_encode;

	(* Parcours en largeur visitant l'ensemble des déclarations du
	   programme, et mémorisant toutes les fonctions et procédures qu'il
	   faut encoder, cf. functions_to_encode_l *)

	(* Initialisation de la FIFO *)
	let decl_queue = Queue.create () in
	
	List.iter (fun e -> Queue.add (root_prefix, e) decl_queue)
			  p_decl_l;

	let get_local_alloc_asm decl_l map_l sign_l decl_l_l =
		let size_ty t =
			match t with
			| Ty_access _ -> addr_size
			| Ty_var id ->
				match search id map_l with
				| Some(Type (r), _) ->
					match !r with
					| None -> failwith "should not happen"
					| Some expr_t ->
						size_exp_type expr_t
		in 

		let rec aux (decl_l:declaration list) asm decl_l_l offset =
			let incr_asm size_t (asm, offset) id =
				((asm ++ movq expr_reg (ind ~ofs:offset rbp)), offset + size_t)
			in

			match decl_l with
			| [] ->
				asm
			| head :: tail ->
				match head.value with
				| Decl_vars (id_l, t, None) ->
					let offset = offset + ((List.length id_l) * (size_ty t)) in
					aux tail asm (add_to_dll head decl_l_l) offset

				| Decl_vars (id_l, t, Some(expr)) ->
					let size_t = size_ty t in
					let (asm_assign,new_offset) = 
					List.fold_left (incr_asm size_t) (asm,offset) id_l in
					let asm = asm 
						   ++ encode_expression expr
						   ++ asm_assign in
					aux tail asm (add_to_dll head decl_l_l) new_offset

				| _ ->
					aux tail asm decl_l_l offset
			in

		aux decl_l nop ([]::decl_l_l) 0
	in

	(* Ajout récursif de toutes les fonctions et procédures *)
	let rec add_all_f_decl context_tree map_l sign_l decl_l_l =
		try
			let (prefix, (decl:declaration)) = Queue.pop decl_queue in
			let decl = decl.value in 

			match decl with
			| Decl_procedure (id, _, decl_l, instr_l) ->
				let assign_asm =
					get_local_alloc_asm decl_l map_l sign_l decl_l_l in

				let f_to_encode = (id, prefix, symbols, instr_l, assign_asm) in
				add_function_to_encode f_to_encode;

				let f_prefix = prefix ^ id in
				List.iter (fun e -> Queue.add (f_prefix, e) decl_queue)
			  			   decl_l;
				add_all_f_decl ()

			| Decl_function (id, _, _, decl_l, instr_l) ->
				let assign_asm =
					get_local_alloc_asm decl_l map_l sign_l decl_l_l in

				let f_to_encode = (id, prefix, symbols, instr_l, assign_asm) in
				add_function_to_encode f_to_encode;

				let f_prefix = prefix ^ id in
				List.iter (fun e -> Queue.add (f_prefix, e) decl_queue)
						   decl_l;
				add_all_f_decl ()

			| _ ->
				add_all_f_decl ()
		with
		| Queue.Empty -> ()
	in

	add_all_f_decl ();
;;

let encode_all_functions =
	let encode_f asm (id, prefix, symbols, instr_l, assign_asm) =
		let f_label = prefix ^ id in (* TODO : remettre séparateur ! *)

		asm ++
		label f_label ++
		assign_asm ++
		encode_instr_list instr_l
	in

	List.fold_left encode_f (void_instr ()) !functions_to_encode_l
;;
*)

let encode_fun (id,prefix,instr_l,assign_asm) (ml,sl,dll) caller_lab=
  (match prefix with
   |"" -> label id
   |_ -> label (prefix ^ (Char.escaped sep) ^ id)) ++
      enter_called_funct id ++ 
      assign_asm ++
      (List.fold_left (fun asm instr -> asm++(encode_instruction
						instr (ml,sl,dll) caller_lab)) nop instr_l)++
      leave_called_funct
  (* à une fonction associe son code *)

let add_to_dll decl dll =
  match dll with
  |hd::tl -> (hd@[decl]) ::tl
  |[] -> [[decl]];;
  
let get_local_alloc_asm decl_l map_l sign_l decl_l_l caller_lab=
  let size_ty t =
    match t with
    | Ty_access _ -> addr_size
    | Ty_var id ->
       match search id map_l with
       | Some(Type (r), _) -> begin
	  match !r with
	  | None -> failwith "should not happen l910"
	  | Some expr_t ->
	     (size_exp_type expr_t) end
       |None -> failwith ("Alloc : Type "^ id ^ " was not found")
       |_ -> failwith ("should not happen l913 :" ^ id ^ " is not a type...")
  in

  let rec aux (decl_l:declaration list) asm decl_l_l offset =
    let incr_asm size_t (asm, offset) id =
      ((asm ++ movq expr_reg (ind ~ofs:offset rbp)), offset + size_t)
    in

    match decl_l with
    | [] ->
       asm
    | head :: tail ->
       match head.value with
       | Decl_vars (id_l, t, None) ->
	  let offset = offset + ((List.length id_l) * (size_ty t)) in
	  aux tail asm (add_to_dll head decl_l_l) offset

       | Decl_vars (id_l, t, Some(expr)) ->
	  let size_t = size_ty t in
	  let (asm_assign,new_offset) =
	    List.fold_left (incr_asm size_t) (asm,offset) id_l in
	  let asm = asm
		    ++ encode_expression expr (map_l,sign_l,decl_l_l) caller_lab
		    ++ asm_assign in
	  aux tail asm (add_to_dll head decl_l_l) new_offset

       | _ ->
	  aux tail asm decl_l_l offset
  in

  aux decl_l nop ([]::decl_l_l) 0
;;

let run_through decl_l cont_tree map_l sign_l decl_l_l =
  let rec aux (decl_l:declaration list) cont_tree map_l sign_l decl_l_l asmacc prefixacc =
    match decl_l with
    | [] -> asmacc
    | hd::tl ->
       match hd.value with
       |Decl_procedure (id,_,decl_li,instr_l)
       |Decl_function (id,_,_,decl_li,instr_l) ->
	 let f_context = Tmap.find id (cont_tree.subtree) in
	 let f_map_l = (f_context.node)::map_l in
	 let assign_asm = get_local_alloc_asm decl_li f_map_l sign_l decl_l_l prefixacc in
	 (* On se prépare à encoder f, et ses sous fonctions *)
	 let f_to_encode = (id, prefixacc, instr_l, assign_asm) in
	 let f_prefix = match prefixacc with
	   |"" -> id
	   |_ -> prefixacc ^ (Char.escaped sep) ^ id in
	 
	 let f_sign_l = match search id map_l with
	   |Some(dt,_) -> begin match dt with
				|Val(Function(a,b)) -> (a,b)::sign_l
				|_ -> failwith "should not happen l962"
			  end
	   |_ -> failwith "should not happen l964" in
	 let next_asm = aux decl_li f_context f_map_l f_sign_l ([]::decl_l_l) asmacc f_prefix in

	 aux tl cont_tree map_l sign_l decl_l_l
	     ((encode_fun f_to_encode
			  (f_map_l,f_sign_l, (decl_li::decl_l_l)) f_prefix)::next_asm) prefixacc
       |_ -> aux tl cont_tree map_l sign_l (add_to_dll hd decl_l_l) asmacc prefixacc 
  in
  aux decl_l cont_tree map_l sign_l decl_l_l [] ""
;;

let encode_text_segment (id,dl,il) context_tree =
	(* Débute par la définition de main *)
	let asm = glabel "main"

		   (* Appel à la procédure principale *)
		   ++ call id

		   (* Terminaison propre du programme *)
		   ++ movq (imm 0) (reg rax)
		   ++ ret in

	(* Puis encode toutes les procédures/fonctions *)
	let (maindecl:declaration) = {value = Decl_procedure(id,[],dl,il);
				      pos = (Lexing.dummy_pos,Lexing.dummy_pos)} in
	let upper_context_tree = {node = Tmap.singleton id (Val(Function([],Prim(Nulltype)))
							   ,false);
				  subtree = (Tmap.singleton id (context_tree))} in
	let encoded_functions = run_through [maindecl]
					    upper_context_tree [upper_context_tree.node;init_map]
					    [] [] in
	
	List.fold_left (fun x y -> x++y) asm encoded_functions
;;

let encode_data_segment () =
	(* Chaîne contenant un saut de ligne pour la procédure new_line *)
	label "NewLine" ++ (string "\n")
;;

let encode_program prog output_file context_tree =
	(* Génère l'assembleur des segments text et data *)
	let asm_text = encode_text_segment prog context_tree in
	let asm_data = encode_data_segment () in

	let asm_prog = {
		text = asm_text;
		data = asm_data
	} in

	(* Ecrit le code assembleur en sortie *)
	print_in_file output_file asm_prog;
;;
 
