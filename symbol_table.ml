(* Table des symboles construite et utilisée pendant la production de code *)

type ident  = string
type offset = int
type size 	= int
type level 	= int

type arg_mode =
| Arg_In
| Arg_Out

(* Types de variables *)
type var_type =
| Var_local
| Var_arg of arg_mode
| Var_for

(* Types de symboles *)
type symbol_type =
| Sym_variable of var_type * level * size (* ? *) * offset (* décalage *)
| Sym_type of size (* taille du type *)
| Sym_function of size (* arguments *) * size (* var. locales *)

(* Symbole de la table *)
type symbol = ident * symbol_type

(* Table des symboles *)
type sym_table = symbol list

(*****************************************************************************)

let get_empty_symbol_table () =
	[]
;;

let add_symbol symbols sym =
	sym :: symbols
;;

let find_symbol symbols id =
	List.find (fun (id_sym, _) -> id = id_sym)
			  symbols
;;
