(* La ligne suivante doit être commentée pour que le code compile, et décommentée pour interpreter
sur Emacs *)
(*#load "ast.cmo";;
 *)
open Ast

module Tmap = Map.Make(String);; (* Il y aura deux Tmap : types et variables *)


exception Type_error of position

type primitive = (* Ce sont les types de base du langage Mini ADA *)
  |Integer
  |Boolean
  |Character
  |Nulltype

type expr_type = (* Tout type définissable en Mini ADA rentre dans ce type *)
  |Prim of primitive
  |Access of expr_type
  |Record of (ident*expr_type) list
  |Function of (ident*expr_type*bool) list * expr_type (* La liste représente
                                                          les arguments,
                                        le dernier adatype le type de retour *)

let equiv t1 t2 = (* Égalité de types *)
  let rec aux a b = match a,b with
    |Prim(x),Prim(y) -> x=y
    |Access(x), Access(y) -> (aux x y)
    |Record l, Record ll -> l = ll
    |Function (l,x),Function (ll,y) -> (l,x) = (ll,y)
    |Access x,Prim(Nulltype)|Prim(Nulltype),Access x -> true
    |_ -> false in
  aux t1 t2;;

type decl_type = (* Ce sont les types qui seront présents dans nos Tmap. Ainsi,
                    on prendra en compte les id de variables et de types *)
  |Val of expr_type
  |Type of expr_type option (* Option pour les déclarations de types
                               sans définition *)

type context_tree = {
  node : (decl_type * bool) Tmap.t;
  subtree : context_tree Tmap.t;
  (* à un identifiant de fonction (ou de procédure) associe son contexte *)
}

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
  |_ -> raise (Type_error p);;

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
			       |_ -> raise (Type_error e.pos) end
  |Expr_unop (o,ee) -> begin match o,(type_expr ee ml) with
      (* Typage des opérateurs unaires *)
		       |UnOp_not,(Prim(Boolean),_) -> (Prim(Boolean),false)
		       |UnOp_negative,(Prim(Integer),_) -> (Prim(Integer),false)
		       |_ -> raise (Type_error e.pos) end
  |Expr_new id -> begin match search id ml with
      (* Instanciation d'un record, en donnant un pointeur vers ce dernier *)
			|Some(Type(Some t), b) -> (Access t,false)
			|Some(Type(None),b) -> (Access(Prim(Nulltype)),false)
			|_ -> raise (Type_error e.pos) end
  |Expr_call (idf,l) ->
    (* f(x1,x2...,xn) *)
    let ll = List.map (fun x -> type_expr x ml) l in (*List eff. arg. types *)
    begin match search idf ml with
	  |Some(Val(Function (arg,ret)),b)
	       when (List.for_all2 (fun (t1,b1) (t2,b2)
				    -> (equiv t1 t2)&&(b1|| (not b2)))
				   ll (List.map (fun (x,y,z) -> (y,z)) arg)) -> (ret,false)
	  |_ -> raise (Type_error e.pos) end
  |Expr_ascii exp -> match type_expr exp ml with
		     |Prim(Integer),_ -> (Prim(Character),false)
		     |_ -> raise (Type_error e.pos)

and type_acc ac ml p = match ac with
  (* Typage d'un accès à une variable ou à un champs d'un Record *)
  |Acc_var id -> begin match search id ml with (* Cas variable *)
		 |Some(Val(t),b) -> (t,b)
		 |_ -> raise (Type_error p) end
  |Acc_field (e,id) -> match type_expr e ml with (* Cas champs Record *)
		       |(Record l,b) -> begin try (List.assoc id l,b) with
					  |Not_found -> raise (Type_error e.pos) end
		       |(Access (Record l),b) -> begin try (List.assoc id l,true) with
						   |Not_found -> raise (Type_error e.pos) end
		       |_ -> raise (Type_error e.pos);;

(* Des bisous historiques <3 *)

let type_ty t ml p= match t with (* À un ty associe son type *)
  |Ty_var id -> begin match search id ml with
		      |None -> raise (Type_error p)
		      |Some (dt,b) -> match dt with
				      |Val _ |Type None -> raise (Type_error p)
				      |Type Some(x) -> x end
  |Ty_access id -> match search id ml with
		   |None -> raise (Type_error p)
		   |Some (dt,b) -> match dt with
				   |Val _ -> raise (Type_error p)
				   |Type None -> Access(Prim(Nulltype))
				   |Type Some(x) -> Access(x)

let rec type_decl (d:declaration) ml cm=
  (* Typage de l'identifiant déclaré. La fonction renvoie la liste de map
     incrémentée. Si une fonction est déclarée, on ajoute à cm le binding entre
     l'identifiant de cette fonction, et son contexte *)

  let type_record r p = (* À la définition d'un record associe son type *)
    let rec aux r l t acc p = match l with
      |hd::tl -> aux r tl t ((hd,t)::acc) p
      |[] -> match r with
	     |[] -> acc
	     |hd::tl -> aux tl (fst hd) (type_ty (snd hd) ml p) acc p in
    match r with
    |[] -> raise (Type_error p) (* Should Not Happen *)
    |hd::tl -> List.rev  (aux tl (fst hd) (type_ty (snd hd) ml p) [] p) in

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
	      |_ -> raise (Type_error p)
	    end
    |false -> raise (Type_error d.pos) in
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
       type. Les vérifications supplémentaires sont faites dans une fonction
       mutuellement récursive : context_fun *)
    match is_available ml id with
    |false -> raise (Type_error p)
    |true -> (id,Function ((type_params params p), rettype)) in

  let val_and_context_bindings = (* Listes des bindings à ajouter respectivement
                                    à ml, et à cm *)
    match d.value with
    |Decl_type id -> begin match is_available ml id with
        (* Cas d'un type déclaré non défini, on ne fait que vérifier la
           disponibilité de l'identifiant *)
			   |true -> [(id,(Type(None),true))],[]
			   |_ -> raise (Type_error d.pos) end
    |Decl_access (id1,id2) -> begin match (search id1 ml),(search id2 ml) with
        (* Cas d'un type défini comme pointeur vers un autre type. On vérifie
           que l'identifiant du premier est libre, et celui du deuxième utilisé
           par un type. *)
				    |None,Some(Type(Some(t)),b)
				     -> [(id1,(Type(Some(Access(t))),true))],[]
				    |None,(Some(Type(None),b)) ->
				      [(id1,(Type(Some(Access(Prim(Nulltype)))),true))],[]
				    |_ -> raise (Type_error d.pos) end
    |Decl_record (id,l) -> begin match is_available ml id with
        (* Cas d'un type défini comme Record. On s'assure que son identifiant
           est disponible, et que les types le composant sont bien définis *)
				 |true -> [(id,(Type(Some(Record(type_record l d.pos))),true))],[]
				 |false -> raise (Type_error d.pos) end
    |Decl_vars (idl,t,eo) -> type_vars idl t eo d.pos,[]
    |Decl_procedure (id,ps,ldec,lins) ->
      (* Une procédure est considérée comme une fonction renvoyant un élément de
         type Nulltype.*)
      (* On type la fonction préalablement *)
      let (k,v as bind) = type_fun id ps (Prim(Nulltype)) (d.pos) in
      (* On construit son contexte (et on vérifie qu'elle est bien formée) *)
      let cont = context_fun id ps (Prim(Nulltype)) ldec lins ((Tmap.add k (Val(v),true)
									 (List.hd ml))
								 ::(List.tl ml)) (d.pos) in
      (* Si tout s'est bien passé, on renvoie les résultats *)
      ([(k,(Val(v),true))],[cont])
    |Decl_function (id,ps,rty,ldec,lins) ->
      (* On type la fonction préalablement *)
      let (k,v as bind) = type_fun id ps (type_ty rty ml d.pos) (d.pos) in
      (* On construit son contexte (et on vérifie qu'elle est bien formée) *)
      let cont = context_fun id ps (type_ty rty ml d.pos) ldec lins ((Tmap.add k (Val(v),true) (List.hd ml))
						  ::(List.tl ml)) (d.pos) in
      (* Si tout s'est bien passé, on renvoie les résultats *)
      ([(k,(Val(v),true))],[cont]) in

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
					 |_ -> raise (Type_error i.pos) end
			|_ -> raise (Type_error i.pos) end

  (* Appel d'une fonction. On vérifie que les arguments sont corrects :
     les types coïncident, et si un paramètre formel est inOut, l'arg
     lui correspondant est valeur gauche *)
  |Instr_call (idf,l) ->
    let ll = List.map (fun x -> type_expr x ml) l in (*List eff. arg. types *)
    begin match search idf ml with
	  |Some(Val(Function (arg,ret)),b)
	       when (List.for_all2
		       (fun (t1,b1) (t2,b2) -> (equiv t1 t2)&&(b1||(not b2)))
		       ll (List.map (fun (x,y,z) -> (y,z)) arg) ) -> false
	  |_-> raise (Type_error i.pos) end

  (* Renvoie d'une valeur (eo), on vérifie que le type de eo est le même que
     celui spécifié au début de la déclaration de fonction. Si eo = None le type
     doit être NullType *)
  |Instr_return eo -> begin match eo with
		      |None when (equiv typ (Prim(Nulltype))) -> true
		      |Some e -> begin match type_expr e ml with
				       |t,_ when (equiv t typ) -> true
				       |_ -> raise (Type_error i.pos) end
		      |_ -> raise (Type_error i.pos)
		      end

  (* Bloc d'instructions, on renvoie true ssi le bloc fini par un return correct
  *)
  |Instr_block l -> (check_block l ml typ)

  (* If...Then...Else if...Then...Else...
     On vérifie que toutes les conditions sont des expressions de type booléen.
     On renvoie true ssi chacun des blocs fini par un return correct *)
  |Instr_if (ll,els) -> let check (e,l) = match type_expr e ml with
			  |t,_ when (equiv t (Prim(Boolean))) -> check_block l ml typ
			  |_ -> raise (Type_error i.pos) in
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
      |_ -> raise (Type_error i.pos) end
  (* While(cond), do(bloc) : on vérifie que cond est une expression de type
     booléen, on renvoie true ssi le bloc fini par un return
     (ça n'a pas de sens) *)
  |Instr_while (cond,bloc) -> match type_expr cond ml with
			      |t,_ when (equiv t (Prim(Boolean))) -> check_block bloc ml typ
			      |_ -> raise (Type_error i.pos)

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
    |false -> raise (Type_error p)
    |true -> {node = List.hd ml ; subtree = mc} in
  (* Renvoyer une association (id,context_tree) *)
  let mapl = handle_decl ldec (new_map params ml) (Tmap.empty) in
  (id,handle_inst lins (fst mapl) (snd mapl));;

let context_program ((id,decll,insl):program) =
  (* Init_map est ainsi la map contenant les types définis de base dans le
     langage. Ainsi, ils peuvent être shadowés par des déclarations nouvelles *)
  let init_list = [("Integer",(Type(Some(Prim(Integer))),true));
		   ("Boolean",(Type(Some(Prim(Boolean))),true));
		   ("Character",(Type(Some(Prim(Character))),true));
		   ("Nulltype",(Type(Some(Prim(Nulltype))),true))] in
  let init_map = List.fold_left (fun m (k,v) -> Tmap.add k v m) (Tmap.empty) init_list in
  context_fun id [] (Prim(Nulltype)) decll insl [init_map] (Lexing.dummy_pos,Lexing.dummy_pos);;
