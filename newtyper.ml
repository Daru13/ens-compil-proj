(* La ligne suivante doit être commentée pour que le code compile, et décommentée pour interpreter
sur Emacs *)
(*#load "ast.cmo";;
 *)
open Ast

module Tmap = Map.Make(String);; (* Il y aura deux Tmap : types et variables *)

  
exception TypeError of position

type primitive = (* Ce sont les types de base du langage Mini ADA *)
  |Integer
  |Boolean
  |Character
  |Nulltype

type expr_type = (* Tout type définissable en Mini ADA rentre dans ce type *)
  |Prim of primitive
  |Access of expr_type
  |Record of (ident*expr_type) list 
  |Function of (ident*expr_type) list * expr_type (* La liste représente les arguments, 
                                                 le dernier adatype le type de retour *)

type decl_type =
  |Val of expr_type
  |Type of expr_type option (* Option pour les déclarations de types sans définition *)
		     
(* L'utilisation des listes pour les records et les fonctions dans adatype n'est pas optimal, 
On préfèrera l'utilisation d'ABR (Set), pour optimiser le temps de recherche.
Cependant, on doit alors faire de adatype un module récursif, ce qui est non trivial et encore
expérimental. Il pourrait être intéressant de demander son avis à J-C Filliatre *)
					
(* On pourrait souhaiter séparer les identifiants de types, et les identifiants de variables.
J'aimerais bien qu'on discute de ça, c'est ce qui reste le moins clair pour moi dans la question du
typage *)

(* Afin de gérer les différents niveaux de contextes, on utilisera non pas une Map 
mais une liste de Maps, de la plus locale à la plus globale *) 
let rec search id ml = try (Some(Tmap.find id (List.hd ml))) with (* search permet de trouver la
définition la plus locale d'un identifiant *)
		       |Not_found -> search id (List.tl ml)
		       |Failure _ -> None;;

let is_available ml id = match ml with
	|[] -> true
	|hd::tl -> not (Tmap.mem id hd);;
let type_binop o t p = match o,t with (* Typage d'une opération binaire *)
  |BinOp_equal,_|BinOp_different,_
  |BinOp_compare _,Prim(Integer)
  |BinOp_and,Prim(Boolean)
  |BinOp_or,Prim(Boolean)
  |BinOp_andThen,Prim(Boolean)
  |BinOp_orElse,Prim(Boolean) -> Prim(Boolean)
  |BinOp_plus,Prim(Integer)
  |BinOp_minus,Prim(Integer)
  |BinOp_multiply,Prim(Integer)
  |BinOp_divide,Prim(Integer)
  |BinOp_remainder,Prim(Integer) -> Prim(Integer)
  |_ -> raise (TypeError p);;

let rec type_expr (e:expression) ml = match e.value with (* Typage d'une expression quelconque *)
  |Expr_int _ -> Prim(Integer)
  |Expr_char _ -> Prim(Character)
  |Expr_bool _ -> Prim(Boolean)
  |Expr_null -> Prim(Nulltype)
  |Expr_access ac -> type_acc ac ml e.pos
  |Expr_binop (a,o,b) -> begin match (type_expr a ml,type_expr b ml) with
			       |t1,t2 when t1 = t2 -> (type_binop o t1 e.pos)
			       |_ -> raise (TypeError e.pos) end
  |Expr_unop (o,ee) -> begin match o,(type_expr ee ml) with
		       |UnOp_not,Prim(Boolean) -> Prim(Boolean)
		       |UnOp_negative,Prim(Integer) -> Prim(Integer)
		       |_ -> raise (TypeError e.pos) end
  |Expr_new id -> begin match search id ml with
		  |Some(Type(Some t)) -> Access t
		  |_ -> raise (TypeError e.pos) end
  |Expr_call (idf,l) ->
    let ll = List.map (fun x -> type_expr x ml) l in (*List eff. arg. types *)
    begin match search idf ml with
	  |Some(Val(Function (arg,ret))) when (List.for_all2 (fun x y -> (x = snd y)) ll arg) -> ret
	  |_ -> raise (TypeError e.pos) end
  |Expr_ascii exp -> match type_expr exp ml with
		     |Prim(Integer) -> Prim(Character)
		     |_ -> raise (TypeError e.pos)
				 
and type_acc ac ml p = match ac with
  |Acc_var id -> begin match search id ml with
		 |Some(Val(t)) -> t
		 |_ -> raise (TypeError p) end
  |Acc_field (e,id) -> match type_expr e ml with
		       |Record l -> List.assoc id l
		       |Access (Record l) -> List.assoc id l
		       |_ -> raise (TypeError p);;
  
(* Des bisous historiques <3 *)
  
(* TODO : Unification des maps *)
let type_decl (d:declaration) ml = 
  
  let type_ty t p= match t with (* À un ty associe son type *)
    |Ty_var id -> begin match search id ml with
			|None -> raise (TypeError p)
			|Some dt -> match dt with
				    |Val _ |Type None -> raise (TypeError p)
				    |Type Some(x) -> x end
    |Ty_access id -> match search id ml with
		     |None -> raise (TypeError p)
		     |Some dt -> match dt with
				 |Val _ -> raise (TypeError p)
				 |Type None -> Access(Prim(Nulltype))
				 |Type Some(x) -> Access(x) in
  
  let type_record r p = (* Tail-recursive. À la définition d'un record associe son type *)
    let rec aux r l t acc p = match l with
      |hd::tl -> aux r tl t ((hd,t)::acc) p
      |[] -> match r with
	     |[] -> acc
	     |hd::tl -> aux tl (fst hd) (type_ty (snd hd) p) acc p in
    match r with
    |[] -> raise (TypeError p) (* Should Not Happen *)
    |hd::tl -> aux tl (fst hd) (type_ty (snd hd) p) [] p in

  let type_vars idl t eo p =
    match List.for_all (is_available ml) idl with
    |true -> begin (* Jusqu'ici, on a vérifié qu'aucun identifiant
de variable n'était déja utilisé, aussi bien par un type que par une variable *)
	     let typ = match search t ml with
	       |Some (Type (Some(t))) -> t
	       |_ -> raise (TypeError p) in
	      match eo with
	      |None -> List.map (fun id -> (id,Val(typ))) idl
	      |Some e when (type_expr e ml = typ) -> List.map (fun id -> (id,Val(typ))) idl
	      |_ -> raise (TypeError p)
	    end
    |false -> raise (TypeError d.pos) in
  
  match d.value with
  |Decl_type id -> begin match is_available ml id with
			 |false -> [(id,Type(None))]
			 |_ -> raise (TypeError d.pos) end
  |Decl_access (id1,id2) -> begin match (search id1 ml),(search id2 ml) with
				  |None,Some(Type(Some(t))) -> [(id1,Type(Some(Access(t))))]
				  |None,Some(Type(None)) ->
				    [(id1,Type(Some(Access(Prim(Nulltype)))))]
				  |_ -> raise (TypeError d.pos) end
  |Decl_record (id,l) -> begin match is_available ml id with
				 |true -> [(id,Type(Some(Record(type_record l d.pos))))]
				 |false -> raise (TypeError d.pos) end
  |Decl_vars (idl,t,eo) -> type_vars idl t eo d.pos
  |Decl_procedure (id,ps,ldec,lins) -> [] (* à faire *)
  |Decl_function (id,ps,ty,ldec,lins) -> [] (* à faire *)
			 

					   
					   

(* TODO : Typage des instructions
---> Création de contexte par une liste de maps
Créer search *)
    
