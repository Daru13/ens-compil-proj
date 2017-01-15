open X86_64
open Ast
open Typer
open Symbol_table
       
(*****************************************************************************)
(*                              VARIABLES GLOBALES                           *)
(*****************************************************************************)

(* Taille d'une adresse/case mémoire (?) en octet *)
let addr_size = 8
let sep = "";;
let int_size = 4 (* pour l'instant les fonctions sont robustes au changement de int_size *)
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
let void_instr () = (* movq (reg rax) (reg rax) *) nop

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
  
let locate_arg arg ((l,t):signature) =
  (* Renvoie le nombre d'octets à remonter, depuis le premier argument rencontré,
     pour retrouver l'argument d'identifiant arg *)
  let rec aux l acc = match l with
    |(i,t,b)::tl when i = arg -> Some(acc,b)
    |(i,t,b)::tl -> aux l (acc - 1)
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
      |Ty_access _ -> List.map (fun id -> (id,8)) idl
      |Ty_var id ->
	match search id ml with
	|Some(Type r) -> begin match !r with
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
		 |dl::dtl ->begin match (locate_var id dl (List.hd ml)) with
			     |Some(n) -> (height,(-1-n),true)
			     |None -> aux id (List.tl ml) dtl stl (height+1) end
		 |[] -> failwith "shouldn't happen right ? l123" end
    |[] -> (* Top context *)
      match dll with
      |dl::dtl ->begin match (locate_var id dl (List.hd ml)) with
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
		movq (imm 0) (reg r14) ++ (* TODO : optimiser ? *)
		movq (imm 1) (reg r15) ++
		cmpq (reg rax) (reg rax) ++
		cmovne (reg r14) (reg rax) ++
		cmove  (reg r15) (reg rax)

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

(* A utiliser avant chaque appel de fonction, dans l'appelant *)
let save_registers () = 
	void_instr ()
;;

(* A utiliser après chaque appel de fonction, dans l'appelé *)
let load_registers () =
	void_instr ()
;;

(* Evalue et empile les expressions de la liste fournie *)
let push_arguments expr_l = 
	let push_arg asm expr =
		asm ++
		encode_expression expr ++
		pushq expr_reg
	in

	List.fold_left push_arg (void_instr ()) expr_l
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
	let asm = pushq (reg rsp)
		   ++ movq (reg rsp) (reg rbp) in

	(* Alloue de l'espace pour les variables locales (+ variables de boucles) *)
	let asm = asm ++ alloc_local_space id in

	asm
;;

(* A utiliser à la fin de chaque définition de fonction *)
let leave_called_funct id =
	(* Libère l'espace de pile utilisé et saute vers l'appelant *)
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
	(* Sauvegarde du contexte *)
	let asm = save_registers () in

	(* Gestion des fonctions préféfinies *)
	let asm = asm ++ match id with
	| "put" ->
		(* Evaluation de l'entier à afficher, déplacement dans %rdi,
		   et appel de la putchar() *)
		encode_expression (List.hd expr_l) ++
		movq expr_reg (reg rdi) ++
		call "putchar"

	| "new_line" ->
		(* Appel de printf() avec l'adresse d'une chaîne globale contenant
		   un simple saut de ligne *)
		movq (ilab "NewLine") (reg rdi) ++
		movq (imm 0) (reg rax) ++
		call "printf"

	| _ ->
		(* TODO : déterminer préfixe *)
		let function_label = "" ^ id in

		asm ++
		(* Evalue puis empile chaque instruction formant un paramètre *)
		push_arguments expr_l ++
		(* Empile l'adresse de retour (instruction suivante) et saute *)
		call function_label
	in

	(* Restauration du contexte sauvegardé *)
	let asm = asm ++ load_registers () in

	asm
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
type f_decl = ident * ident * sym_table * instr_list * text

(* Liste de fonctions/procédures à encoder en x86 *)
let functions_to_encode_l = ref []

let add_function_to_encode (f:f_decl) =
	functions_to_encode_l := f :: !functions_to_encode_l
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

(* Renvoit une liste d'identifiants utilisés par des boucles for *)
(*
let find_for_loops_variables instr_l =
	let is_for_instr instr =
		match instr with
		| Instr_for _ -> true
		| _ 		  -> false
	in
	let get_for_var (id, _, _, _, _) =
		id
	in
	
	let for_instr_l = List.find_all is_for_instr instr_l in
	List.map get_for_var for_instr_l
;;
*)

let rec encode_decl_procedure id params decl_l instr_l context =
(*	(* La table des symboles utilisée par la procédure doit (en plus)
	   contenir ses paramètres, suivis de ses variables locales, et
	   enfin de ses variables de boucle *)
	let proc_decl_symbols = context.symbols in

	(* Paramètres *)
	let add_param_as_sym (id_l, mode_opt, ) symbols =

	let proc_decl_symbols = List.fold_left


	(* On met également à jour le préfixe et le niveau *)
	let proc_decl_level  = context.level + 1 in
	let proc_decl_prefix = id ^ context.prefix in

	(* La procédure est ajoutée à la liste des déclarations à encoder *)
	let proc_decl = (id, context.prefix, proc_decl_symbols, instr_l) in
	add_function_to_encode proc_decl;

	(* Enfin, la procédure est ajoutée au contexte renvoyé *)
	let symbols = add_symbol context.symbols 
	{
		prefix 		 = context.prefix;
		level 		 = context.level;
		local_offset = !offset;
		symbols 	 = symbols
	}
*)
	(* TODO *)
	context

and encode_decl_function id params t decl_l instr_l context =
	(* TODO *)
	context

and encode_declaration (decl:declaration) context =
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
  let encode_fun (id,prefix,instr_l,assign_asm) =
  label (prefix ^ sep ^ id) ++
    assign_asm ++
    (List.fold_left (fun asm instr -> asm++(encode_instruction instr)) nop instr_l);;
    
  (* à une fonction associe son code *)

let add_to_dll decl dll =
  match dll with
  |hd::tl -> (hd@[decl]) ::tl
  |[] -> [[decl]];;
  
let get_local_alloc_asm decl_l map_l sign_l decl_l_l =
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
       |_ -> failwith "should not happen l913"
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

  aux decl_l nop ([]::decl_l_l) 0 ;;

let run_through decl_l cont_tree map_l sign_l decl_l_l =
  let rec aux (decl_l:declaration list) cont_tree map_l sign_l decl_l_l asmacc prefixacc =
    match decl_l with
    | [] -> asmacc
    | hd::tl ->
       match hd.value with
       |Decl_procedure (id,_,decl_li,instr_l)
       |Decl_function (id,_,_,decl_li,instr_l) ->
	 let assign_asm = get_local_alloc_asm decl_li map_l sign_l decl_l_l in
	 (* On se prépare à encoder f, et ses sous fonctions *)
	 let f_to_encode = (id, prefixacc, instr_l, assign_asm) in
	 let f_prefix = prefixacc ^ sep ^ id in
	 let f_context = Tmap.find id (cont_tree.subtree) in
	 let f_map_l = (f_context.node)::map_l in
	 let f_sign_l = match search id map_l with
	   |Some(dt,_) -> begin match dt with
				|Val(Function(a,b)) -> (a,b)::sign_l
				|_ -> failwith "should not happen l962"
			  end
	   |_ -> failwith "should not happen l964" in
	 let next_asm = aux decl_li f_context f_map_l f_sign_l ([]::decl_l_l) asmacc f_prefix in

	 aux tl cont_tree map_l sign_l decl_l_l ((encode_fun f_to_encode)::next_asm) prefixacc
       |_ -> aux tl cont_tree map_l sign_l decl_l_l asmacc prefixacc 
  in
  aux decl_l cont_tree map_l sign_l decl_l_l [] "";;
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
	let () = match encoded_functions with
	  |[] -> print_string "Vide";
	  |_ -> print_string "Non vide"; in
	
	List.fold_left (fun x y -> x++y) asm encoded_functions
;;

let encode_data_segment () =
	(* Chaîne contenant un saut de ligne pour la procédure new_line *)
	label "NewLine" ++ (string "\n")
;;

let encode_program prog output_file context_tree =
	

	(* Remplit la liste des fonctions à encoder *)
	

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
 
