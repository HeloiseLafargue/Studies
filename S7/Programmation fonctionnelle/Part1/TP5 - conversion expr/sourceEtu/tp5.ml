(* Evaluation des expressions simples *)


(* (Interface) Module abstrayant les expressions *)
module type ExprSimple =
sig
  type t
  val const : int -> t
  val plus : t -> t -> t
  val mult : t-> t -> t
end

(* Module (fermé) réalisant l'évaluation d'une expression *)
module EvalSimple : ExprSimple with type t = int = 
struct
  type t = int
  let const c = c
  let plus e1 e2 = e1 + e2
  let mult e1 e2 = e1 * e2
end


(* Solution 1 pour tester *)
(* A l'aide de foncteur *)

(* Définition des expressions *)
module ExemplesSimples (E:ExprSimple) =
struct
  (* 1+(2*3) *)
  let exemple1  = E.(plus (const 1) (mult (const 2) (const 3)) )
  (* (5+2)*(2*3) *)
  let exemple2 =  E.(mult (plus (const 5) (const 2)) (mult (const 2) (const 3)) )
end

(* Module d'évaluation des exemples *)
module EvalExemples =  ExemplesSimples (EvalSimple)

let%test _ = (EvalExemples.exemple1 = 7)
let%test _ = (EvalExemples.exemple2 = 42)


(* Module de conversion des expressions en chaîne de caractères *)
module PrintSimple : ExprSimple with type t = string =
struct
  type t = string
  let const c = string_of_int(c)
  let plus e1 e2 = "("^e1^"+"^e2^")"
  let mult e1 e2 = "("^e1^"*"^e2^")"
end

(* Module d'évaluation des conversions *)
module EvalPrintSimple =  ExemplesSimples (PrintSimple)

let%test _ = (EvalPrintSimple.exemple1 = "(1+(2*3))")
let%test _ = (EvalPrintSimple.exemple2 = "((5+2)*(2*3))")



(* Module de décompte des opérations d’une expression *)
module CompteSimple : ExprSimple with type t = int =
struct
  type t = int
  let const c = 0
  let plus e1 e2 = 1 + e1 + e2
  let mult e1 e2 = 1 + e1 + e2
end

(* Module d'évaluation du décompte d'opérations *)
module EvalCompte =  ExemplesSimples (CompteSimple)

let%test _ = (EvalCompte.exemple1 = 2)
let%test _ = (EvalCompte.exemple2 = 3)


(* Interface abstrayant la présence de variable dans les expressions *)
module type ExprVar =
sig
  type t
  val var : string -> t
  val def : string -> t -> t -> t
end

(* Interface abstrayant les expressions dans leur intégralité *)
module type Expr =
sig
  include ExprSimple
  include (ExprVar with type t := t)
end



(********************************** Printer *******************************)


(* Module de conversion des expressions avec variables en chaîne de caractères*)
module PrintVar : ExprVar with type t = string =
struct
  type t = string
  let var x = x 
  let def str var1 var2 = "let " ^ str ^ " = " ^ var1 ^ " in " ^ var2
end


(* Définition des expressions avec variables *)
module ExemplesVar (V:ExprVar) =
struct
  let exemple3  = V.(var "x")
  let exemple4 =  V.(def "Test" (var "x") (var "y"))
end


(* Module d'évaluation des conversions avec variables *)
module EvalPrintVar =  ExemplesVar(PrintVar)

let%test _ = (EvalPrintVar.exemple3 = "x")
let%test _ = (EvalPrintVar.exemple4= "let Test = x in y")



(**************************** Evaluateur ******************************)

(* Type définissant le type de l’environnement d’évaluation *)
type env = (string*int) list

(* Module d'évaluation des expressions avec variables *)
module EvalVar : ExprVar with type t = env -> int =
struct 
  type t = env -> int
  let def x e1 e2 = fun e -> e2 ((x,e1 e)::e)
  let var x = fun e -> List.assoc x e
end