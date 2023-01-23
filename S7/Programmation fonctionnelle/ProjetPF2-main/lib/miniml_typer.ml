open Miniml_types
open Miniml_parser
(* signature minimale pour définir des variables *)
module type VariableSpec =
  sig
    (* type abstrait des variables      *)
    type t

    (* création d'une variable fraîche  *)
    val fraiche : unit -> t

    (* fonctions de comparaison         *)
    (* permet de définir des conteneurs *)
    (* (hash-table, etc) de variables   *)
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val hash : t -> int

    (* fonction d'affichage             *)
    (* on utilise Format.std_formatter  *)
    (* comme premier paramètre          *)
    (* pour la sortie standard          *) 
    val fprintf : Format.formatter -> t -> unit
  end

(* implantation de la spécification     *)
module TypeVariable : VariableSpec =
  struct
    type t = int

    let fraiche =
      let cpt = ref 0 in
      (fun () -> incr cpt; !cpt)

    let compare a b = a - b
    let equal a b = a = b
    let hash a = Hashtbl.hash a

    let fprintf fmt a = Format.fprintf fmt "t{%d}" a
  end

(* signature minimale pour définir des termes *)

(* Fonction auxilière pour construire le type et les equations
   pour chaque sous expression
 Paramètre : expr : l'expression à typer
 Retour : type de l'expression et equations accumulées *)
let type_expr expr =

  (* Fonction auxilière pour construire le types et les equations
   pour chaque sous expression 
   Paramètre : expr : l'expression à typer
               env : l'environnement initial 
               eq : liste vide pour stocker les equations accumulées
   Retour : type de l'expression et equations accumulées *)
   let rec aux expr env eq =
      match expr with 
      |EConstant c -> ( match c with
                        |CBooleen _ -> TBool, eq
                        |CEntier _ -> TInt, eq
                        |CNil -> TList(TUnit), eq
                        |CUnit -> TUnit, eq )

      |EIdent ident -> let alpha = TVar(TypeVariable.fraiche()) in
                        let rec recherche ident env =
                         ( match env with     (* recherche de la variable dans l'environnement pour trouver son type *)
                          |[] -> false, alpha, (ident, alpha)::env (* ajout de la variable dans l'environnement à verifier *)
                          |(var, typ)::q -> if (String.equal ident var) then true, typ, env
                                            else recherche ident q )                    
                        in let _, typ, _ = recherche ident env 
                        in typ, eq  
                       
      |EProd (e1, e2) -> let t1, eq1 = aux e1 env eq in 
                         let t2, eq2 = aux e2 env eq in 
                         TProd(t1, t2), eq1@eq2

      |ECons (e1, e2) -> let t1, eq1 = aux e1 env eq in 
                         let t2, eq2 = aux e2 env eq in 
                         t2, (t2, TList(t1))::(eq1@eq2) (* ajout de l'equation tau2 =- tau1 list *)

      |EFun (ident, e) -> let alpha = TVar(TypeVariable.fraiche()) in 
                          let t2, eq2 = aux e ((ident, alpha)::env) eq in (* ajout de la variable dans l'environnement *)
                          TFun(alpha, t2), eq2

      |EIf (b, e1, e2) -> let t, eqb = aux b env eq in 
                           let t1, eq1 = aux e1 env eq in 
                           let t2, eq2 = aux e2 env eq in 
                           t1, (t, TBool)::(t1, t2)::(eqb@eq1@eq2) (* ajout de l'equation tau =- bool, tau1 =- tau2 *)

      |EApply (e1, e2) -> let beta = TVar(TypeVariable.fraiche()) in
                          let t1, eq1 = match e1 with 
                                      | EBinop(PLUS) | EBinop(MOINS) | EBinop(DIV) | EBinop(MULT)-> (TFun(TInt, TInt), eq) 
                                      | EBinop(AND) | EBinop(OR) -> (TFun(TBool, TBool), eq)
                                      | EBinop(EQU) | EBinop(NOTEQ) | EBinop(INF) | EBinop(SUP) | EBinop(INFEQ) | EBinop(SUPEQ) -> (TFun(TInt, TBool), eq)
                                      | EBinop(CONCAT) -> (TFun(TList(beta), TList(beta)), eq)
                                      | EBinop(CONS) -> (TFun(beta, TList(beta)), eq)
                                      | _ -> aux e1 env eq in 
                          let t2, eq2 = aux e2 env eq in 
                          let alpha = TVar(TypeVariable.fraiche()) in 
                          alpha, (t1, TFun(t2, alpha))::(eq1@eq2) (* ajout de l'equation tau1 =- tau2 -> tau3 *)
      
      |ELet (ident, e1, e2) -> let t1, eq1 = aux e1 env eq in 
                               let t2, eq2 = aux e2 ((ident, t1)::env) eq in (* ajout de la variable dans l'environnement *)
                               t2, (eq1@eq2) 

      |ELetrec (ident, e1, e2) -> let alpha = TVar(TypeVariable.fraiche()) in 
                                  let t1, eq1 = aux e1 ((ident, alpha)::env) eq in (* ajout de la variable dans l'environnement *)
                                  let t2, eq2 = aux e2 ((ident, alpha)::env) eq in (* ajout de la variable dans l'environnement *)
                                  t2, (t1, alpha)::(eq1@eq2) (* ajout de l'equation alpha =- tau *)

      |_ -> failwith "Erreur"

   in aux expr [] [] 

(* Partie 3 : Résolution des équations *)

(* Fonction de résolution des équations de type
 Paramètre : eq : liste des équations à résoudre
 Retour : liste des équations résolues *)
let rec resolution eq =

  (* Fonction auxiliaire pour remplacer une variable
   Paramètre : var : la variable à remplacer
               typ : le type par lequel remplacer
               eq : liste des équations à traiter
   Retour : liste des équations traitées *)
  let rec remplaceVar var typ eq =
    match eq with
    |[] -> []
    |(t1, t2)::q -> if (t1 = var) then (typ, t2)::(remplaceVar var typ q)
                    else if (t2 = var) then (t1, typ)::(remplaceVar var typ q)
                    else if (t1, t2) = (var, var) then (typ, typ)::(remplaceVar var typ q)
                    else (t1, t2)::(remplaceVar var typ q)
  in

  match eq with
  |[] -> true
  |(t1, t2)::q -> (
    match (t1, t2) with
    | (TInt, TInt) -> resolution q
    | (TBool, TBool) -> resolution q
    | (TUnit, TUnit) -> resolution q
    | (TList(t1), TList(t2)) -> resolution ((t1, t2)::q)
    | (TFun(t1, t2), TFun(sigma1, sigma2)) -> resolution ((t1, sigma1)::(t2, sigma2)::q)
    | (TProd(t1, t2), TProd(sigma1, sigma2)) -> resolution ((t1, sigma1)::(t2, sigma2)::q)
    | (TVar(var), t) -> resolution (remplaceVar (TVar(var)) t q)
    | (t, TVar(var)) -> resolution (remplaceVar (TVar(var)) t q)
    | _ -> false
  )

(* 
  Tests unitaires de la fonction resolution (partie 3)
  Les autres équations sont testées dans les tests d'intégration
*)

let exprInt = (EApply(EApply(EBinop(PLUS), EConstant(CEntier(2))), EConstant(CEntier(3)))) (* OK - réussi *)
let exprBool = (EApply(EApply(EBinop(AND), EConstant(CBooleen(true))), EConstant(CBooleen(false)))) (* OK - réussi *)
let exprList = (EApply(EApply(EBinop(CONS), EConstant(CEntier(2))), EConstant(CNil))) (* OK - réussi *)
let exprIf = (EIf(EConstant(CBooleen(true)), EConstant(CBooleen(true)), EConstant(CBooleen(false)))) (* OK - réussi *)

let exprIntKO = (EApply(EApply(EBinop(PLUS), EConstant(CBooleen(true))), EConstant(CEntier(3)))) (* KO - échoue *)
let exprBoolKO = (EApply(EApply(EBinop(AND), EConstant(CEntier(2))), EConstant(CBooleen(false)))) (* KO - échoue *)
let exprListKO = (EApply(EApply(EBinop(PLUS), EConstant(CBooleen(true))), EConstant(CNil))) (* KO - échoue *)
let exprIfKO = (EIf(EConstant(CBooleen(true)), EConstant(CEntier(2)), EConstant(CBooleen(false)))) (* KO - échoue *)


let %test _ = let (_, l) = type_expr exprInt in (resolution l)
let %test _ = let (_, l) = type_expr exprBool in (resolution l)
let %test _ = let (_, l) = type_expr exprList in (resolution l)
let %test _ = let (_, l) = type_expr exprIf in (resolution l)

let %test _ = let (_, l) = type_expr exprIntKO in not (resolution l)
let %test _ = let (_, l) = type_expr exprBoolKO in not (resolution l)
let %test _ = let (_, l) = type_expr exprListKO in not (resolution l)
let %test _ = let (_, l) = type_expr exprIfKO in not (resolution l)


(* Tests d'intégration, pour le typer et l'existence de solutions *)

let typer_validation repertoire file = 
  let f = read_miniml_tokens_from_file (repertoire ^ file) in
  let progs = p_Expr f in
  print_string ("\n"^file^"\n");
  match Solution.uncons progs with
  | None        -> (Format.printf "\n** typer failed ! **@.";)
  | Some (expression, _) -> 
    begin
      (* Avoir le type d'une expression et ses equations *)
      let (t, l) = type_expr (fst expression)
      in
      (* Affichage du type obtenu *)
      Miniml_printer.print_typ TypeVariable.fprintf Format.std_formatter t;

      (* Affichage de l'existence d'une solution *)
      let existence = resolution l in
      if existence then
          Format.printf "\n** Solution exists **@."
      else
          Format.printf "\n** Solution does not exist **@."
  end

(* Commenter la fonction suivante pour lancer dune runtest sans erreur*)
let _ =
  let repertoire = "lib/tests/" in
  let files = Sys.readdir repertoire in
  Array.iter (fun file -> typer_validation repertoire file) files
