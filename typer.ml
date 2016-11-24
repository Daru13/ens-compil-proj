(* Commenter la ligne suivante pour compiler *)
#load "ast.cmo";;
open Ast
       
type posi = Lexing.position*Lexing.position

exception TypeError of posi
					      
module Tmap = Map.Make(String);;

type adatype =
  |Integer
  |Character
  |Boolean
  |R
  |Access_R
  |Typenull

let rec type_expr e m =
  let type_binop o t u p =
    let error = raise (TypeError p) in
    match o with
    |BinOp_equal|BinOp_different -> begin match t,u with
				    |a,b when a = b -> Boolean
				    |_ -> error end
    |BinOp_compare _ -> begin match t,u with
			      |Integer,Integer -> Boolean
			      |_ -> error end
    |BinOp_plus|BinOp_minus|BinOp_multiply|BInOp_divide|BinOp_remainder ->
							 begin match t,u with
							       |Integer,Integer -> Integer
							       |_ -> error end
    |BinOp_and|BinOp_andThen|BinOp_or|BinOp_orElse ->
				       match t,u with
				       |Boolean,Boolean -> Boolean
				       |_ -> error in
  match e.value with
  |Expr_int _ -> Integer
  |Expr_char _ -> Character
  |Expr_bool _ -> Boolean
  |Expr_null -> Typenull
  |Expr_access i -> Tmap.find i m
  |Expr_binop (a,o,b) -> let t1 = type_expr a m in
		       let t2 = type_expr b m in
		       type_binop o t1 t2 e.pos
  |Expr_unop (o,a) -> begin
		      let t = type_expr a m in match o,t with
					       |UnOp_negative,Integer -> Integer
					       |UnOp_not,Boolean -> Boolean
					       |_ -> raise (TypeError e.pos) end
  |Expr_new i -> Tmap.find i m
  |Expr_call (i,l) -> Tmap.find i m;;
