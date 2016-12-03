open Arg

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
			 "(default: false) Only arse input, and exit");
			("--type-only" , Set (param_type_only) ,
			 " (default: false) Only parse and type input, and exit");
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

let compile =
	(* Lecture de la ligne de commande *)
	parse_arguments ();

	(* Ouverture du fichier source, et lexbuf associé *)
	let source_file = open_source_file () in
	let lexbuf		= Lexing.from_channel source_file in

	try (* TODO : fermeture de fichier ? *)
		(* Analyse lexicale et syntaxique *)
		let abstract_syntax = Parser.program Lexer.token lexbuf in
		if !param_parse_only then exit 0;

		(* Analyse sémantique et typage *)
		(* TODO *)
		if !param_type_only then exit 0;

		(* Fin de la compilation, sans erreur *)
		exit 0;
	with
		_ -> exit 2
;;

let () = compile
