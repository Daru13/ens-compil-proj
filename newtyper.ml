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
  |Function of (ident*expr_type*bool) list * expr_type (* La liste représente les arguments, 
                                                 le dernier adatype le type de retour *)
let equiv t1 t2 =
  let rec aux a b = match a,b with
    |Prim(x),Prim(y) -> x=y
    |Access(x), Access(y) -> (aux x y)
    |Record l, Record ll -> l = ll
    |Function (l,x),Function (ll,y) -> (l,x) = (ll,y)
    |Access x,Prim(Nulltype)|Prim(Nulltype),Access x -> true
    |_ -> false in
  aux t1 t2;;
				    
type decl_type =
  |Val of expr_type
  |Type of expr_type option (* Option pour les déclarations de types sans définition *)

type context_tree = {
  node : (decl_type * bool) Tmap.t;
  subtree : context_tree Tmap.t;
  (* à un identifiant de fonction (ou de procédure) associe son contexte *)
}
						   
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
let merge_maplist ml =
  let f k x y = match x,y with
    |None,None -> None
    |None,Some(a)|Some(a),None -> Some(a)
    |Some(a),Some(b) -> Some(a) in
  let rec aux ml acc = match ml with
    |[] -> acc
    |hd::tl -> aux tl (Tmap.merge f acc hd) in
  aux ml (Tmap.empty);;
  
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
  |Expr_int _ -> (Prim(Integer),false)
  |Expr_char _ -> (Prim(Character),false)
  |Expr_bool _ -> (Prim(Boolean),false)
  |Expr_null -> (Prim(Nulltype),false)
  |Expr_access ac -> type_acc ac ml e.pos
  |Expr_binop (a,o,b) -> begin match (type_expr a ml,type_expr b ml) with
			       |(t1,b1),(t2,b2) when (equiv t1 t2)
				-> ((type_binop o t1 e.pos),false)
			       |_ -> raise (TypeError e.pos) end
  |Expr_unop (o,ee) -> begin match o,(type_expr ee ml) with
		       |UnOp_not,(Prim(Boolean),_) -> (Prim(Boolean),false)
		       |UnOp_negative,(Prim(Integer),_) -> (Prim(Integer),false)
		       |_ -> raise (TypeError e.pos) end
  |Expr_new id -> begin match search id ml with
			|Some(Type(Some t), b) -> (Access t,false)
			|Some(Type(None),b) -> (Access(Prim(Nulltype)),false)
			|_ -> raise (TypeError e.pos) end
  |Expr_call (idf,l) ->
    let ll = List.map (fun x -> type_expr x ml) l in (*List eff. arg. types *)
    begin match search idf ml with
	  |Some(Val(Function (arg,ret)),b)
	       when (List.for_all2 (fun (t1,b1) (t2,b2)
				    -> (equiv t1 t2)&&(b1|| (not b2)))
				   ll (List.map (fun (x,y,z) -> (y,z)) arg)) -> (ret,false)
	  |_ -> raise (TypeError e.pos) end
  |Expr_ascii exp -> match type_expr exp ml with
		     |Prim(Integer),_ -> (Prim(Character),false)
		     |_ -> raise (TypeError e.pos)
				 
and type_acc ac ml p = match ac with
  |Acc_var id -> begin match search id ml with
		 |Some(Val(t),b) -> (t,b)
		 |_ -> raise (TypeError p) end
  |Acc_field (e,id) -> match type_expr e ml with
		       |(Record l,b) -> begin try (List.assoc id l,b) with
					  |Not_found -> raise (TypeError e.pos) end
		       |(Access (Record l),b) -> begin try (List.assoc id l,true) with
						   |Not_found -> raise (TypeError e.pos) end
		       |_ -> raise (TypeError e.pos);;
  
(* Des bisous historiques <3 *)

let type_ty t ml p= match t with (* À un ty associe son type *)
  |Ty_var id -> begin match search id ml with
		      |None -> raise (TypeError p)
		      |Some (dt,b) -> match dt with
				      |Val _ |Type None -> raise (TypeError p)
				      |Type Some(x) -> x end
  |Ty_access id -> match search id ml with
		   |None -> raise (TypeError p)
		   |Some (dt,b) -> match dt with
				   |Val _ -> raise (TypeError p)
				   |Type None -> Access(Prim(Nulltype))
				   |Type Some(x) -> Access(x) 

(* TODO : Unification des maps *)
let rec type_decl (d:declaration) ml cm= 
   
  let type_record r p = (* Tail-recursive. À la définition d'un record associe son type *)
    let rec aux r l t acc p = match l with
      |hd::tl -> aux r tl t ((hd,t)::acc) p
      |[] -> match r with
	     |[] -> acc
	     |hd::tl -> aux tl (fst hd) (type_ty (snd hd) ml p) acc p in
    match r with
    |[] -> raise (TypeError p) (* Should Not Happen *)
    |hd::tl -> List.rev  (aux tl (fst hd) (type_ty (snd hd) ml p) [] p) in

  let type_vars idl t eo p =
    match List.for_all (is_available ml) idl with
    |true -> begin (* Jusqu'ici, on a vérifié qu'aucun identifiant
de variable n'était déja utilisé, aussi bien par un type que par une variable *)
	     let typ =  type_ty t ml p in match eo with
	      |None -> List.map (fun id -> (id,(Val(typ),true))) idl
	      |Some e when (equiv (fst (type_expr e ml)) typ) -> List.map (fun id -> (id,(Val(typ),true))) idl
	      |_ -> raise (TypeError p)
	    end
    |false -> raise (TypeError d.pos) in
  let type_params l p=
    let rec aux l ttl typ b acc = match l with
      |hd::tl -> aux tl ttl typ b ((hd,typ,b)::acc)
      |[] -> match ttl with
	     |(idl,mo,newty)::tl1 ->
	       begin match mo with
		     |None|Some(Mod_in) -> aux idl tl1 (type_ty newty ml p) false acc
		     |Some(Mod_inOut) -> aux idl tl1 (type_ty newty ml p) true acc end
	     |[] -> acc in
    match l with
    |[] -> []
    |(idl,mo,ty)::tl -> match mo with
			|None|Some(Mod_in) -> List.rev (aux idl tl (type_ty ty ml p) false [])
			|Some(Mod_inOut) -> List.rev (aux idl tl (type_ty ty ml p) true []) in
  
  let type_fun id params rettype p = match is_available ml id with
    |false -> raise (TypeError p)
    |true -> (id,Function ((type_params params p), rettype)) in 
  let val_and_context_bindings =
    match d.value with
    |Decl_type id -> begin match is_available ml id with
			   |true -> [(id,(Type(None),true))],[]
			   |_ -> raise (TypeError d.pos) end
    |Decl_access (id1,id2) -> begin match (search id1 ml),(search id2 ml) with
				    |None,Some(Type(Some(t)),b)
				     -> [(id1,(Type(Some(Access(t))),true))],[]
				    |None,(Some(Type(None),b)) ->
				      [(id1,(Type(Some(Access(Prim(Nulltype)))),true))],[]
				    |_ -> raise (TypeError d.pos) end
    |Decl_record (id,l) -> begin match is_available ml id with
				 |true -> [(id,(Type(Some(Record(type_record l d.pos))),true))],[]
				 |false -> raise (TypeError d.pos) end
    |Decl_vars (idl,t,eo) -> type_vars idl t eo d.pos,[]
    |Decl_procedure (id,ps,ldec,lins) ->
      let (k,v as bind) = type_fun id ps (Prim(Nulltype)) (d.pos) in
      let cont = context_fun id ps (Prim(Nulltype)) ldec lins ((Tmap.add k (Val(v),true)
									 (List.hd ml))
								 ::(List.tl ml)) (d.pos) in
      ([(k,(Val(v),true))],[cont])
    |Decl_function (id,ps,rty,ldec,lins) ->
      let (k,v as bind) = type_fun id ps (type_ty rty ml d.pos) (d.pos) in
      let cont = context_fun id ps (type_ty rty ml d.pos) ldec lins ((Tmap.add k (Val(v),true) (List.hd ml))
						  ::(List.tl ml)) (d.pos) in
      ([(k,(Val(v),true))],[cont]) in
   
  ((List.fold_left (fun m (k,v) -> Tmap.add k v m) (List.hd ml) (fst val_and_context_bindings)::(List.tl ml)),
   List.fold_left (fun m (k,v) -> Tmap.add k v m) cm (snd val_and_context_bindings))
and check_block l ml typ = (* /!\ Fonction probablement à modifier pour les histoires de return *)
  let rec aux l acc = match l with
    |[] -> acc
    |hd::tl -> aux tl (type_instr hd ml typ) in
  aux l false

and type_instr (i:instruction) ml typ =
 	
  match i.value with
  |Instr_set (vof,e) -> begin match type_acc vof ml i.pos with
			|t,true -> begin match type_expr e ml with
					 |tt,_ when (equiv t tt) -> false
					 |_ -> raise (TypeError i.pos) end
			|_ -> raise (TypeError i.pos) end
  |Instr_call (idf,l) ->
    let ll = List.map (fun x -> type_expr x ml) l in (*List eff. arg. types *)
    begin match search idf ml with
	  |Some(Val(Function (arg,ret)),b)
	       when (List.for_all2
		       (fun (t1,b1) (t2,b2) -> (equiv t1 t2)&&(b1||(not b2)))
		       ll (List.map (fun (x,y,z) -> (y,z)) arg) ) -> false
	  |_-> raise (TypeError i.pos) end		   
  |Instr_return eo -> begin match eo with
		      |None when (equiv typ (Prim(Nulltype))) -> true
		      |Some e -> begin match type_expr e ml with
				       |t,_ when (equiv t typ) -> true
				       |_ -> raise (TypeError i.pos) end
		      |_ -> raise (TypeError i.pos)
		      end
  |Instr_block l -> (check_block l ml typ)
  |Instr_if (ll,els) -> let check (e,l) = match type_expr e ml with
			  |t,_ when (equiv t (Prim(Boolean))) -> check_block l ml typ
			  |_ -> raise (TypeError i.pos) in
			(List.for_all check ll) && (check_block els ml typ)
  |Instr_for (id,b,ebeg,eend,bloc) ->
    begin
      match ((type_expr ebeg ml),(type_expr eend ml),(is_available ml id)) with
      |((Prim(Integer),_), (Prim(Integer),_) , (true)) ->
	check_block bloc ((Tmap.add id (Val(Prim(Integer)),false) (List.hd ml))::List.tl ml) typ
      |_ -> raise (TypeError i.pos) end

  |Instr_while (cond,bloc) -> match type_expr cond ml with
			      |t,_ when (equiv t (Prim(Boolean))) -> check_block bloc ml typ
			      |_ -> raise (TypeError i.pos)

and context_fun id params rtype ldec lins ml p= 
  (* Initialiser la nouvelle map avec les paramètres, en fonction de in et out *)
  let new_map params ml =
    let rec aux l ttl typ b macc = match l with
      |hd::tl -> aux tl ttl typ b (Tmap.add hd (Val(typ),b) macc)
      |[] -> match ttl with
	     |(l,mo,t)::tl1 -> begin
			       match mo with
			       |None|Some(Mod_in) -> aux l tl1 (type_ty t ml p) false macc
			       |Some(Mod_inOut) -> aux l tl1 (type_ty t ml p) true macc end
	     |[] -> macc in
    match params with
    |[] -> (Tmap.empty::ml)
    |(l,mo,t)::tl -> match mo with
		     |None|Some(Mod_in) -> (aux l tl (type_ty t ml p) false (Tmap.empty))::ml
		     |Some(Mod_inOut) -> (aux l tl (type_ty t ml p) true (Tmap.empty))::ml in
  (* Y ajouter toutes les déclarations, y compris de fonctions ---> gestion de leur contextes *)
  let handle_decl ldec ml mc = List.fold_left
				     (fun (l,c) d -> type_decl d l c) (ml,mc) ldec in
  (* Vérifier que l'on obtient bien une fonction en testant les instructions *)
  let handle_inst lins ml mc = match check_block lins ml rtype with
    |false -> raise (TypeError p)
    |true -> {node = List.hd ml ; subtree = mc} in
  (* Renvoyer une association (id,context_tree) *)
  let mapl = handle_decl ldec (new_map params ml) (Tmap.empty) in
  (id,handle_inst lins (fst mapl) (snd mapl));;

let context_program ((id,decll,insl):program) = 
  let init_list = [("Integer",(Type(Some(Prim(Integer))),true));
		   ("Boolean",(Type(Some(Prim(Boolean))),true));
		   ("Character",(Type(Some(Prim(Character))),true));
		   ("Nulltype",(Type(Some(Prim(Nulltype))),true))] in
  let init_map = List.fold_left (fun m (k,v) -> Tmap.add k v m) (Tmap.empty) init_list in
  context_fun id [] (Prim(Nulltype)) decll insl [init_map] (Lexing.dummy_pos,Lexing.dummy_pos);;
