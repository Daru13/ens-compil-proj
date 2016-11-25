(* La ligne suivante doit être commentée pour que le code compile, et décommentée pour interpreter
sur Emacs *)
(*#load "ast.cmo";;
 *)
open Ast

module Tmap = Map.Make(String);;

  
exception TypeError of position

type primitive = (* Ce sont les types de base du langage Mini ADA *)
  |Integer
  |Boolean
  |Character
  |Nulltype

type adatype = (* Tout type définissable en Mini ADA rentre dans ce type *)
  |Prim of primitive
  |Access of adatype
  |Record of (ident*adatype) list 
  |Function of (ident*adatype) list * adatype (* La liste représente les arguments, 
                                                 le dernier adatype le type de retour *)

(* L'utilisation des listes pour les records et les fonctions dans adatype n'est pas optimal, 
On préfèrera l'utilisation d'ABR (Set), pour optimiser le temps de recherche.
Cependant, on doit alors faire de adatype un module récursif, ce qui est non trivial et encore
expérimental. Il pourrait être intéressant de demander son avis à J-C Filliatre *)
					
(* On pourrait souhaiter séparer les identifiants de types, et les identifiants de variables.
J'aimerais bien qu'on discute de ça, c'est ce qui reste le moins clair pour moi dans la question du
typage *)
	     
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
  
let rec type_expr (e:expression) m = match e.value with (* Typage d'une expression quelconque *)
  |Expr_int _ -> Prim(Integer)
  |Expr_char _ -> Prim(Character)
  |Expr_bool _ -> Prim(Boolean)
  |Expr_null -> Prim(Nulltype)
  |Expr_access id -> Access(Tmap.find id m)
  |Expr_binop (a,o,b) -> begin match (type_expr a m,type_expr b m) with
			       |t1,t2 when t1 = t2 -> (type_binop o t1 e.pos)
			       |_ -> raise (TypeError e.pos) end
  |Expr_unop (o,ee) -> begin match o,(type_expr ee m) with
		       |UnOp_not,Prim(Boolean) -> Prim(Boolean)
		       |UnOp_negative,Prim(Integer) -> Prim(Integer)
		       |_ -> raise (TypeError e.pos) end
  |Expr_new id -> Tmap.find id m
  |Expr_call (idf,l) ->
    let ll = List.map (fun x -> type_expr x m) l in (*List eff. arg. types *)
    begin match (Tmap.find idf m) with
	  |Function (arg,ret) when (List.for_all2 (fun x y -> (x = snd y)) ll arg) -> ret
	  |_ -> raise (TypeError e.pos) end
  |Expr_ascii exp -> match type_expr exp m with
		     |Prim(Integer) -> Prim(Character)
		     |_ -> raise (TypeError e.pos);;
  
