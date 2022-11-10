(* Exercice 3 *)
module type Expression = sig
  (* Type pour représenter les expressions *)
  type exp


  (* eval : 'a arbre -> int *)
  (* Fonction qui évalue la valeur d’une expression *)
  (* Paramètre : l'expression sous forme d'un arbre où l'opérateur est dans le noeud et les nombres dans les feuilles *)
  (* Retour : la valeur de l'expression, qui est un entier *)
  val eval : exp -> int
end

(* Exercice 4 *)

(* TO DO avec l'aide du fichier  expressionArbreBinaire.txt *)
module ExpAvecArbreBinaire : Expression =
struct
    (* Type pour représenter les expressions binaires *)
    type op = Moins | Plus | Mult | Div
    type exp = Binaire of exp * op * exp | Entier of int
  
    (* eval *)
    let rec eval =  fun arbre ->
      match arbre with
      |Entier n -> n
      |Binaire (e1, ope, e2) -> match ope with
                |Plus -> (eval e1) + (eval e2)
                |Moins -> (eval e1) - (eval e2)
                |Div -> (eval e1) / (eval e2)
                |Mult -> (eval e1) * (eval e2);;
  (* Tests : TO DO *)

    (*let%test _ = eval (Binaire ((3, Plus, 4), Moins, 12)) = (-5)*)
end

(* Exercice 5 *)

(* TO DO avec l'aide du fichier  expressionArbreNaire.txt *)