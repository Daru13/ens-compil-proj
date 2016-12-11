(* La ligne suivante doit être commentée pour que le code compile, et décommentée pour interpreter
sur Emacs *)
(*#load "ast.cmo";;
 *)
open Ast

module Tmap = Map.Make(String);; (* Il y aura deux Tmap : types et variables *)


exception Type_error of position*string

type primitive = (* Ce sont les types de base du langage Mini ADA *)
  |Integer
  |Boolean
  |Character
  |Nulltype

type expr_type = (* Tout type définissable en Mini ADA rentre dans ce type *)
  |Prim of primitive
  |Access of (expr_type option) ref
  |Record of (ident*expr_type) list
  |Function of (ident*expr_type*bool) list * expr_type (* La liste représente
                                                          les arguments,
                                        le dernier adatype le type de retour *)

let equiv t1 t2 = (* Égalité de types *)
  let rec aux a b = match a,b with
    |Prim(x),Prim(y) -> x=y
    |Access(x), Access(y) -> (!x)==(!y) (* L'égalité de types de pointeurs n'est vérifiée que s'ils
                                    pointent vers le même type *)
    |Record l, Record ll -> l = ll
    |Function (l,x),Function (ll,y) -> (l,x) = (ll,y)
    |Access x,Prim(Nulltype)|Prim(Nulltype),Access x -> true
    |Function ([],x),y -> aux x y
    |x,Function([],y) -> aux x y
    |_ -> false in
  aux t1 t2;;

type decl_type = (* Ce sont les types qui seront présents dans nos Tmap. Ainsi,
                    on prendra en compte les id de variables et de types *)
  |Val of expr_type
  |Type of (expr_type option) ref (* Option pour les déclarations de types
                               sans définition *) 
let to_string decl_typ =
  let rec aux exp_typ = match exp_typ with
    |Prim Integer -> "Integer"
    |Prim Boolean -> "Boolean"
    |Prim Character -> "Character"
    |Prim Nulltype -> "Nulltype"
    |Access t -> "Access to "^(match !t with
			       |None -> "nothing"
			       |Some(tt) -> aux tt)
    |Record l -> "Record{"^(List.fold_left (fun s (id,t) -> s^"; "^id^" : "^(aux t)) "" l)^";}"
    |Function (_,t) -> "Function returning "^(aux t) in
  match decl_typ with
  |Val(t) -> "Value of "^(aux t)
  |Type(x) -> match !x with
	      |None -> "Type (undefined yet)"
	      |Some t -> "Type "^(aux t);;
  
type context_tree = {
  node : (decl_type * bool) Tmap.t;
  subtree : context_tree Tmap.t;
}
let string_of_ml_ids ml =
  let rec aux l ml = match l with
    |(id,_)::[] -> id^(aux [] ml)
    |(id,_)::tl -> id^", "^(aux tl ml)
    |[] -> match ml with
	   |hd::tl -> "
		       "^(aux (Tmap.bindings hd) tl)
	   |[] -> "" in
  aux [] ml;;
  (* à un identifiant de fonction (ou de procédure) associe son contexte *)


(* L'utilisation des listes pour les records dans adatype n'est pas optimal,
   On préfèrera l'utilisation d'ABR (Set), pour optimiser le temps de recherche.
   Cependant, on simplifie ainsi la syntaxe *)


(* Afin de gérer les différents niveaux de contextes, on utilisera non pas une
   Map mais une liste de Maps, de la plus locale à la plus globale *)

let rec search id ml = try (Some(Tmap.find id (List.hd ml))) with
  (* search permet de trouver la
     définition la plus locale d'un identifiant *)
		       |Not_found -> search id (List.tl ml)
		       |Failure _ -> None;;

let is_available ml id = match ml with (* Vérifie qu'un identifiant est
                                          utilisable dans une déclaration *)
  |[] -> true
  |hd::tl -> not (Tmap.mem id hd);;
  
let merge_maplist ml = (* Obsolète, mais on ne sait jamais *)
  let f k x y = match x,y with
    |None,None -> None
    |None,Some(a)|Some(a),None -> Some(a)
    |Some(a),Some(b) -> Some(a) in
  let rec aux ml acc = match ml with
    |[] -> acc
    |hd::tl -> aux tl (Tmap.merge f acc hd) in
  aux ml (Tmap.empty);;

let check_for_def ml p =
  let is_defined k v = match v with
    |Type(t),_ when !t = None -> false
    |_ -> true in
  match ml with
  |[] -> true
  |hd::tl -> Tmap.for_all is_defined hd;;
  
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
  |_ -> raise (Type_error (p,"Wrong type for this binop"));;

let rec type_expr (e:expression) ml = match e.value with
  (* Typage d'une expression quelconque *)
  |Expr_int _ -> (Prim(Integer),false) (* Entier constant *)
  |Expr_char _ -> (Prim(Character),false) (* Charactère constant *)
  |Expr_bool _ -> (Prim(Boolean),false) (* Booléen constant *)
  |Expr_null -> (Prim(Nulltype),false) (* null *)
  |Expr_access ac -> type_acc ac ml e.pos (* Accès à une variable
                                             /*\ Ne pas confondre avec access *)
  |Expr_binop (a,o,b) -> begin match (type_expr a ml,type_expr b ml) with
			       |(t1,b1),(t2,b2) when (equiv t1 t2)
				-> ((type_binop o t1 e.pos),false)
			       |_ -> raise (Type_error (e.pos,"Wrong type for this binop")) end
  |Expr_unop (o,ee) -> begin match o,(type_expr ee ml) with
      (* Typage des opérateurs unaires *)
		       |UnOp_not,(Prim(Boolean),_) -> (Prim(Boolean),false)
		       |UnOp_negative,(Prim(Integer),_) -> (Prim(Integer),false)
		       |_ -> raise (Type_error (e.pos,"Wrong type for this unop")) end
  |Expr_new id -> begin match search id ml with
      (* Instanciation d'un record, en donnant un pointeur vers ce dernier *)
			|Some(Type(re), b) -> (Access re,false)
			|None -> raise (Type_error (e.pos,"Undeclared id "^id))
			|Some(Val(_),_) -> raise (Type_error (e.pos, id^" is a value, not a type"))
		  end
  |Expr_call (idf,l) -> ((check_fun_call idf l ml (e.pos)),false) 
  (* f(x1,x2...,xn) *)
			  
  |Expr_ascii exp -> match type_expr exp ml with
		     |Prim(Integer),_ -> (Prim(Character),false)
		     |_ -> raise (Type_error (e.pos,"character'val TBM"))

and type_acc ac ml p = match ac with
  (* Typage d'un accès à une variable ou à un champs d'un Record *)
  |Acc_var id -> begin match search id ml with (* Cas variable *)
		       |Some(Val(Function([],t)),b) -> (t,false)
		       |Some(Val(t),b) -> (t,b)
		       |_ -> raise (Type_error (p,"acc_var TBM")) end
  |Acc_field (e,id) -> match type_expr e ml with (* Cas champs Record *)
		       |(Record l,b) -> begin try (List.assoc id l,b) with
					  |Not_found -> raise (Type_error (e.pos,"acc_record TBM")) end
		       |(Access r,b) -> begin match !r with
						|Some(Record l) -> begin
								   try (List.assoc id l,true) with
								   |Not_found ->
								     raise
								       (Type_error
									  (e.pos,
									   "acc_access_record TBM"))
								 end
						|_ ->raise (Type_error (e.pos,"Not a record, can't "
									      ^"access a field !"))
					  end
		       |_ -> raise (Type_error (e.pos,"Not a record, can't access a field !"))
and check_fun_call idf l ml p =
  let fst (a,b,c) = a in
  let snd (a,b,c) = b in
  let trd (a,b,c) = c in
  let rec aux lform leff = match lform,leff with
    |(hd1::tl1),(hd2::tl2) -> begin match (type_expr hd2 ml) with
				    |t,b when (equiv t (snd hd1)) ->
				      begin match b,(trd hd1) with
					    |false,true -> raise
							     (Type_error (p,
									  "Call to "^idf^
									    " : arg "^(fst hd1)^
									      "has to be a left val"))
					    |_ -> aux tl1 tl2
				      end						    
				    |_ -> raise (Type_error (p,"Call to "^idf^
								 " : wrong type for arg "^(fst hd1)))
			      end
    |hd::tl,[] -> raise (Type_error (p,"Call to "^idf^" : argument(s) missing !"))
    |[],hd::tl -> raise (Type_error (p,"Call to "^idf^" : too many arguments !"))
    |[],[] -> true in
  match (search idf ml) with
  |None -> raise (Type_error (p,idf^" is not declared"))
  |Some(Val(Function(arg,ret)),_) -> begin  match (aux arg l) with
					    |false -> failwith "This is absolutely impossible !"
					    |true -> ret
				     end
  |Some(t,_) -> raise (Type_error (p,idf^" is not a function : "^(to_string t)))
	
(* Des bisous historiques <3 *)

let rec check_proc_call idf l ml p =
  let fst (a,b,c) = a in
  let snd (a,b,c) = b in
  let trd (a,b,c) = c in
  let rec aux lform leff = match lform,leff with
    |(hd1::tl1),(hd2::tl2) -> begin match (type_expr hd2 ml) with
				    |t,b when (equiv t (snd hd1)) ->
				      begin match b,(trd hd1) with
					    |false,true -> raise
							     (Type_error (p,
									  "Call to "^idf^
									    " : arg "^(fst hd1)^
									      "has to be a left val"))
					    |_ -> aux tl1 tl2
				      end						    
				    |_ -> raise (Type_error (p,"Call to "^idf^
								 " : wrong type for arg "^(fst hd1)))
			      end
    |hd::tl,[] -> raise (Type_error (p,"Call to "^idf^" : argument(s) missing !"))
    |[],hd::tl -> raise (Type_error (p,"Call to "^idf^" : too many arguments !"))
    |[],[] -> true in
  match (search idf ml) with
  |None -> raise (Type_error (p,idf^" is not declared"))
  |Some(Val(Function(arg,Prim(Nulltype))),_) -> begin  match (aux arg l) with
					    |false -> failwith "This is absolutely impossible !"
					    |true -> Prim(Nulltype)
				     end
  |Some(t,_) -> raise (Type_error (p,idf^" is not a procedure : "^(to_string t)))

let type_ty t ml p= match t with (* À un ty associe son type *)
  |Ty_var id -> begin match search id ml with
		      |None -> raise (Type_error (p,"type_ty (var) TBM"))
		      |Some (dt,b) -> match dt with
				      |Val _ -> raise (Type_error (p,id^" is not a type"))
				      |Type r -> match !r with
						 |None -> raise (Type_error (p, id^" is not defined"))
						 |Some ft -> ft end
  |Ty_access id -> match search id ml with
		   |None -> raise (Type_error (p,id^" was not declared"))
		   |Some (dt,b) -> match dt with
				   |Val _ -> raise (Type_error (p,id^" is not a type"))
				   |Type r -> Access r

let rec type_decl (d:declaration) ml cm=
  (* Typage de l'identifiant déclaré. La fonction renvoie la liste de map
     incrémentée. Si une fonction est déclarée, on ajoute à cm le binding entre
     l'identifiant de cette fonction, et son contexte *)

  let type_record r ml p = (* À la définition d'un record associe son type *)
    let check_rec r p = (* Vérifie que le record ne contient pas deux champs de même id *)
      (* pseudo linéaire, tail-recursive *)
      let rec aux l last = match l with
	|(id,t)::tl when id = last -> raise (Type_error (p,"There are two fields of name "
							   ^id^" in this record"))
	|(id,t)::tl -> aux tl id
	|[] -> r in
      let pseudo_compare (id1,t1) (id2,t2) = String.compare id1 id2 in
      let sorted_r = List.sort pseudo_compare r in
      aux sorted_r "" in
    let rec aux r l t acc p = match l with
      |hd::tl -> aux r tl t ((hd,t)::acc) p
      |[] -> match r with
	     |[] -> acc
	     |hd::tl -> aux tl (fst hd) (type_ty (snd hd) ml p) acc p in
    match r with
    |[] -> raise (Type_error (p,"Should Not Happen"))
    |hd::tl -> List.rev  (check_rec (aux tl (fst hd) (type_ty (snd hd) ml p) [] p) p) in

  let type_vars idl t eo p = (* Type une déclaration de variable(s) *)
    (* On vérifie que tous les identifiants des variables à déclarer sont
       disponibles *)
    match List.for_all (is_available ml) idl with
    |true -> begin
        (* On regarde si une expression est donnée, et, le cas échéant, si son
           type correspond au type donné. *)
	     let typ =  type_ty t ml p in match eo with
	      |None -> List.map (fun id -> (id,(Val(typ),true))) idl
	      |Some e when (equiv (fst (type_expr e ml)) typ) -> List.map (fun id -> (id,(Val(typ),true))) idl
	      |_ -> raise (Type_error (p,"The expression type is not the same as the given type"))
	    end
    |false -> raise (Type_error (d.pos,"Some id(s) are already declared")) in
  (* On renvoie la liste de bindings à rajouter à notre liste de maps *)

  let type_params l p= (*Typage de paramètres afin de les insérer dans
                         le constructeur Function *)

    (* aux est alambiquée car tail-recursive. Elle "applatit" la liste params,
       en distribuant le type et le mode à tous les identifiants (tout en
       "traduisant" ces types et modes, bien entendu) *)
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
  (* List.rev afin que les arguments soient dans le bon ordre *)

  let type_fun id params rettype p = (* Typage d'une fonction *)
    (* On vérifie que son identifiant est disponible. Si oui, on renvoie son
       type, et que deux paramètres n'ont pas le même nom.
       Les vérifications supplémentaires sont faites dans une fonction
       mutuellement récursive : context_fun *)
     let check_fun r p = (* Vérifie que la fonction n'a pas deux params de même id *)
      (* pseudo linéaire, tail-recursive *)
      let rec aux l last = match l with
	|(id,mo,t)::tl when id = last -> raise (Type_error (p,"There are two fields of name "
							   ^id^" in this record"))
	|(id,mo,t)::tl -> aux tl id
	|[] -> (id,Function (r, rettype) ) in
      let pseudo_compare (id1,mo1,t1) (id2,mo2,t2) = String.compare id1 id2 in
      let sorted_r = List.sort pseudo_compare r in
      aux sorted_r "" in

    match is_available ml id with
    |false -> raise (Type_error (p,id^" is already declared"))
    |true -> check_fun (type_params params p) p in

  let val_and_context_bindings = (* Listes des bindings à ajouter respectivement
                                    à ml, et à cm *)
    match d.value with
    |Decl_type id -> begin match is_available ml id with
        (* Cas d'un type déclaré non défini, on ne fait que vérifier la
           disponibilité de l'identifiant *)
			   |true -> [(id,(Type(ref(None)),true))],[]
			   |_ -> raise (Type_error (d.pos,id^" is not available")) end
    |Decl_access (id1,id2) -> begin match (is_available ml id1),(search id2 ml) with
        (* Cas d'un type défini comme pointeur vers un autre type. On vérifie
           que l'identifiant du premier est libre, et celui du deuxième utilisé
           par un type. *)
				    |true,Some(Type(r),b)
				     -> [(id1,(Type(ref (Some(Access(r)))),true))],[]
				    |true,Some(Val(_),_) -> raise (Type_error (d.pos,
									       id2^
										 " is not a type"))
				    |true,None -> raise (Type_error (d.pos, id2^" is not declared"))
				    |false,_ -> raise (Type_error (d.pos, id1^" is not available"))
			      end
    |Decl_record (id,l) -> begin match is_available ml id with
        (* Cas d'un type défini comme Record. On s'assure que son identifiant
           est disponible, et que les types le composant sont bien définis *)
				 |true -> let newml = (Tmap.add id (Type(ref (None)),true) (List.hd ml))::
							(List.tl ml) in
				   [(id,(Type(ref(Some (Record(type_record l newml d.pos)))),true))],[]
				 |false -> begin
					   match search id ml with
					   |Some(Type(t),b) when !t = None ->
					     let () = (t:= Some(Record(type_record l ml d.pos))) in								   [],[]
					   |_ -> raise (Type_error (d.pos, id^" is not available"))
					 end
			   end
    |Decl_vars (idl,t,eo) -> type_vars idl t eo d.pos,[]
    |Decl_procedure (id,ps,ldec,lins) when (check_for_def ml (d.pos))->
      (* Une procédure est considérée comme une fonction renvoyant un élément de
         type Nulltype.*)
      (* On vérifie que tous les types sont définis *)
      
      (* On type la fonction préalablement *)
      let (k,v as bind) = type_fun id ps (Prim(Nulltype)) (d.pos) in
      (* On construit son contexte (et on vérifie qu'elle est bien formée) *)
      let cont = context_fun id ps (Prim(Nulltype)) ldec lins ((Tmap.add k (Val(v),true)
									 (List.hd ml))
								 ::(List.tl ml)) (d.pos) in
      (* Si tout s'est bien passé, on renvoie les résultats *)
      ([(k,(Val(v),true))],[cont])
    |Decl_function (id,ps,rty,ldec,lins) when (check_for_def ml (d.pos))->
      (* On type la fonction préalablement *)
      let (k,v as bind) = type_fun id ps (type_ty rty ml d.pos) (d.pos) in
      (* On construit son contexte (et on vérifie qu'elle est bien formée) *)
      let cont = context_fun id ps (type_ty rty ml d.pos) ldec lins ((Tmap.add k (Val(v),true) (List.hd ml))
						  ::(List.tl ml)) (d.pos) in
      (* Si tout s'est bien passé, on renvoie les résultats *)
      ([(k,(Val(v),true))],[cont])
    |_ -> raise (Type_error (d.pos,"Some Type is frozen while declaring new function/procedure"))
  in

  (* On rajoute les bindings obtenus dans ml et cm, qu'on renvoie ensuite *)
  ((List.fold_left (fun m (k,v) -> Tmap.add k v m) (List.hd ml) (fst val_and_context_bindings)::(List.tl ml)),
   List.fold_left (fun m (k,v) -> Tmap.add k v m) cm (snd val_and_context_bindings))

and check_block l ml typ = (* /!\ Fonction probablement à modifier pour les histoires de return *)
  (* Vérifie qu'une liste d'instructions est conforme (sinon renvoie Type_error),
     et renvoie true ssi sa dernière instruction est un return (correct) *)
  let rec aux l acc = match l with
    |[] -> acc
    |hd::tl -> aux tl (type_instr hd ml typ) in
  aux l false

and type_instr (i:instruction) ml typ =
  (* Vérifie qu'une instruction est correcte (sinon renvoie Type_error), et
     renvoie true ssi c'est un return correct *)
  match i.value with

  (* Affectation d'une valeur (e) à une variable (vof). On vérifie que les types
     coïncident *)
  |Instr_set (vof,e) -> begin match type_acc vof ml i.pos with
			|t,true -> begin match type_expr e ml with
					 |tt,_ when (equiv t tt) -> false
					 |_ -> raise (Type_error (i.pos,"Types don't match")) end
			|_ -> raise (Type_error (i.pos,"Not a left value")) end

  (* Appel d'une fonction. On vérifie que les arguments sont corrects :
     les types coïncident, et si un paramètre formel est inOut, l'arg
     lui correspondant est valeur gauche *)
  |Instr_call (idf,l) -> begin match (check_proc_call idf l ml (i.pos)) with
			 |_ -> false end

  (* Renvoie d'une valeur (eo), on vérifie que le type de eo est le même que
     celui spécifié au début de la déclaration de fonction. Si eo = None le type
     doit être NullType *)
  |Instr_return eo -> begin match eo with
		      |None when (equiv typ (Prim(Nulltype))) -> true
		      |Some e -> begin match type_expr e ml with
				       |t,_ when (equiv t typ) -> true
				       |_ -> raise (Type_error (i.pos,"Not the right return type"))
				 end
		      |_ -> raise (Type_error (i.pos,"Returning void in a function"))
		      end

  (* Bloc d'instructions, on renvoie true ssi le bloc fini par un return correct
  *)
  |Instr_block l -> (check_block l ml typ)

  (* If...Then...Else if...Then...Else...
     On vérifie que toutes les conditions sont des expressions de type booléen.
     On renvoie true ssi chacun des blocs fini par un return correct *)
  |Instr_if (ll,els) -> let check (e,l) = match type_expr e ml with
			  |t,_ when (equiv t (Prim(Boolean))) -> check_block l ml typ
			  |_ -> raise (Type_error (i.pos,"condition is not boolean")) in
			(List.for_all check ll) && (check_block els ml typ)

  (* For (id in ebeg,eend),do(bloc) : on vérifie que id est dispo,
     que ebeg et eend sont des expressions de type entier,
     On renvoie true ssi le bloc fini par un return correct (ça n'a pas de sens)
  *)
  |Instr_for (id,b,ebeg,eend,bloc) ->
    begin
      match ((type_expr ebeg ml),(type_expr eend ml),(is_available ml id)) with
      |((Prim(Integer),_), (Prim(Integer),_) , (true)) ->
	check_block bloc ((Tmap.add id (Val(Prim(Integer)),false) (List.hd ml))::List.tl ml) typ
      |_ -> raise (Type_error (i.pos,"Something wrong in for")) end
  (* While(cond), do(bloc) : on vérifie que cond est une expression de type
     booléen, on renvoie true ssi le bloc fini par un return
     (ça n'a pas de sens) *)
  |Instr_while (cond,bloc) -> match type_expr cond ml with
			      |t,_ when (equiv t (Prim(Boolean))) -> check_block bloc ml typ
			      |_ -> raise (Type_error (i.pos,"contion is not boolean"))

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
    |false when (rtype = Prim(Nulltype)) -> {node = List.hd ml; subtree = mc}
    |true -> {node = List.hd ml ; subtree = mc}
    |_ -> raise (Type_error (p,id^" doesn't return a value"))
  in
  (* Renvoyer une association (id,context_tree) *)
  let mapl = handle_decl ldec (new_map params ml) (Tmap.empty) in
  match check_for_def (fst mapl) (List.hd lins) with
  |true -> (id,handle_inst lins (fst mapl) (snd mapl))
  |false -> raise (Type_error (((List.hd lins).pos),"Type declared and undefined before instructions"));; 
let context_program ((id,decll,insl):program) =
  (* Init_map est ainsi la map contenant les types définis de base dans le
     langage. Ainsi, ils peuvent être shadowés par des déclarations nouvelles *)
  let init_list = [("integer",(Type(ref(Some(Prim(Integer)))),true));
		   ("boolean",(Type(ref(Some(Prim(Boolean)))),true));
		   ("character",(Type(ref(Some(Prim(Character)))),true));
		   ("nulltype",(Type(ref(Some(Prim(Nulltype)))),true));
		   ("put",(Val(Function([("c",Prim(Character),false)],Prim(Nulltype))),true));
		   ("new_line",(Val(Function([],Prim(Nulltype))),true))
		  ] in
  let init_map = List.fold_left (fun m (k,v) -> Tmap.add k v m) (Tmap.empty) init_list in
  context_fun id [] (Prim(Nulltype)) decll insl [Tmap.empty;init_map] (Lexing.dummy_pos,Lexing.dummy_pos);;
