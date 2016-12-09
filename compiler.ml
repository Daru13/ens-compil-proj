open Arg
open Lexing

(* En cas d'arguments anonymes non attendus en ligne de commande *)
exception Unexpected_argument of string
exception Invalid_source_file

(* Paramètres de la ligne de commande *)
let param_parse_only  = ref false
let param_type_only   = ref false
let param_source_file = ref ""

let parse_arguments () =
	(* Liste des paramètres disponibles en ligne de commande *)
	let available_params = [
			("--parse-only", Set (param_parse_only),
			 "(default: false) Parse input, then exit");
			("--type-only" , Set (param_type_only) ,
			 " (default: false) Parse and type input, then exit");
		] in

	(* Fonction appelée quand un argument anonyme s est rencontré *)
	let anon_arg_fct s =
		if !param_source_file = "" then
			param_source_file := s
		else
			raise (Unexpected_argument s)
	in

	(* Message débutant la description des arguments attendus ("usage") *)
	let usage_msg = "Usage: adac <source file> [--parse-only] [--type-only]" in

	parse available_params anon_arg_fct usage_msg
;;

let open_source_file () =
	let source_file = !param_source_file in

	(* On ouvre le fichier source en lecture, s'il est bien défini *)
	if Sys.file_exists source_file then
		open_in source_file
	else
		raise Invalid_source_file
;;

let print_error (pos_start, pos_end) msg =
	let file 	= !param_source_file in
	let line 	= pos_start.pos_lnum in
	let c_start = pos_start.pos_cnum - pos_start.pos_bol in
	let c_end 	= pos_end.pos_cnum - pos_end.pos_bol in

	Printf.eprintf "File \"%s\", line %d, characters %d-%d:\n" file line c_start c_end;
	Printf.eprintf "Error: %s\n" msg
;;

let handle_exception e =
	match e with
	| Invalid_source_file ->
		Printf.eprintf "Fatal error: unable to open the specified source file.\n";
		exit 2
	| Lexer.Illegal_character(pos) ->
		print_error pos "illegal character.";
		exit 1
	| Lexer.Too_large_integer(pos) ->
		print_error pos "out of bounds integer.";
		exit 1
	| Ast.Syntax_error(pos, msg) ->
		print_error pos msg;
		exit 1
	| Ast.Unmatching_identifiers(pos, msg) ->
		print_error pos msg;
		exit 1
	| Newtyper.Type_error(pos) ->
		let type_error_msg = "invalid types" in
		print_error pos type_error_msg;
		exit 1
	| _ as e ->
		raise e; (* TODO: enlever cette ligne, utile pour debug *)
		(* Printf.eprintf "Fatal error: uncatched exception.\n";
		exit 2 *)
;;

let compile () =	
	try 
		(* Lecture de la ligne de commande *)
		parse_arguments ();

		(* Ouverture du fichier source, et lexbuf associé *)
		let source_file = open_source_file () in
		let lexbuf		= Lexing.from_channel source_file in

		(* TODO : fermeture de fichier ? *)
		(* Analyse lexicale et syntaxique *)
		let abstract_syntax = Parser.program Lexer.token lexbuf in
		if !param_parse_only then exit 0;

		(* Analyse sémantique et typage *)
		let typed_program = Newtyper.context_program abstract_syntax in
		if !param_type_only then exit 0;

		(* Fin de la compilation, sans erreur *)
		close_in source_file;
		exit 0;
	with
		_ as e -> handle_exception e
;;

let _ = compile ()
