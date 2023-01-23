open Miniml_types
open Miniml_lexer
open Lazyflux
open Miniml_printer

(********************************  Fonction de LECTURE d'un fichier  *******************************)

(* Produit le flux des lexèmes reconnus *)
let read_miniml_tokens_from_file filename : token Flux.t =
  try
    let chan = open_in filename in
    let buf = Lexing.from_channel chan in
    line_g := 1;
    let next_token () =
      try
        let next = token buf in
        if next = EOF
        then
          begin
            close_in chan;
            None
          end
        else
          Some (next, ())
   with
   | ErreurLex msg ->
      begin
        close_in chan;
        raise (ErreurLecture (Format.sprintf "ERREUR : ligne %d, lexème '%s' : %s" !line_g (Lexing.lexeme buf) msg))
      end in
    Flux.unfold next_token ()
 with
    | Sys_error _ -> raise (ErreurLecture (Format.sprintf "ERREUR : Impossible d'ouvrir le fichier '%s' !" filename))
;;

(* Fonction de lecture d'un buffer.   *)
(* Similaire à la fonction précédente *)
let read_miniml_tokens_from_lexbuf buf : token Flux.t =
  line_g := 1;
  let next_token () =
    try
      let next = token buf in
      if next = EOF
      then
        begin
          None
        end
      else
        Some (next, ())
    with
    | ErreurLex msg ->
       begin
         raise (ErreurLecture (Format.sprintf "ERREUR : ligne %d, lexème '%s' : %s" !line_g (Lexing.lexeme buf) msg))
       end in
  Flux.unfold next_token ()
;;

(* Fonction de lecture d'une chaîne.  *)
(* Similaire à la fonction précédente *)
let read_miniml_tokens_from_string chaine : token Flux.t =
  read_miniml_tokens_from_lexbuf (Lexing.from_string chaine)
;;

(* Fonctions auxiliaires de traitement des lexèmes *)
(* contenant une information: IDENT, BOOL et INT   *)
let isident =
  function IDENT _     -> true
         | _           -> false
let isbool =
  function BOOL _      -> true
         | _           -> false
let isint =
  function INT _       -> true
         | _           -> false

let unident =
  function
  | IDENT id    -> id
  | _           -> assert false
let unbool =
  function
  | BOOL b      -> b
  | _           -> assert false   
let unint =
  function
  | INT i       -> i
  | _           -> assert false


(********************************  Fonction de PARSING d'un fichier  *******************************)

(* Cette section est très similaire à celle du TP10, il faut juste adapter au projet les variables 
   et les types de flux, par exemple *)

(* le seul type de flux utilisé : le flux des solutions, le langage n’étant pas ambigu, il existe au ´
    plus une solution de parsing a récupérer. De plus, à la différence du TP10, on utilise le flux 
    définit dans les sources du sujet : lazyflux *)
module Solution = Flux;;


(* les 60 prochaines lignes, environ, sont un copier-coller des sources du TP10*)

(* types des parsers généraux *)
type ('a, 'b) result = ('b * 'a Flux.t) Solution.t;;
type ('a, 'b) parser = 'a Flux.t -> ('a, 'b) result;;

(* interface des parsers: combinateurs de parsers et parsers simples *)
module type Parsing =
  sig
    val map : ('b -> 'c) -> ('a, 'b) parser -> ('a, 'c) parser

    val return : 'b -> ('a, 'b) parser

    val ( >>= ) : ('a, 'b) parser -> ('b -> ('a, 'c) parser) -> ('a, 'c) parser

    val zero : ('a, 'b) parser

    val ( ++ ) : ('a, 'b) parser -> ('a, 'b) parser -> ('a, 'b) parser

    val run : ('a, 'b) parser -> 'a Flux.t -> 'b Solution.t

    val pvide : ('a, unit) parser

    val ptest : ('a -> bool) -> ('a, 'a) parser

    val ( *> ) : ('a, 'b) parser -> ('a, 'c) parser -> ('a, 'b * 'c) parser
  end

(* implantation des parsers, comme vu en TD. On utilise les opérations *)
(* du module Flux et du module Solution                                *)
module Parser : Parsing =
  struct
    let map fmap parse f = Solution.map (fun (b, f') -> (fmap b, f')) (parse f);; 

    let return b f = Solution.return (b, f);;

    let (>>=) parse dep_parse = fun f -> Solution.(parse f >>= fun (b, f') -> dep_parse b f');;

    let zero _ = Solution.zero;;

    let (++) parse1 parse2 = fun f -> Solution.(parse1 f ++ parse2 f);;

    let run parse f = Solution.(map fst (filter (fun (_, f') -> Flux.uncons f' = None) (parse f)));;

    let pvide f =
      match Flux.uncons f with
      | None   -> Solution.return ((), f)
      | Some _ -> Solution.zero;;

    let ptest p f =
      match Flux.uncons f with
      | None        -> Solution.zero
      | Some (t, q) -> if p t then Solution.return (t, q) else Solution.zero;;

    let ( *> ) parse1 parse2 = fun f ->
      Solution.(parse1 f >>= fun (b, f') -> parse2 f' >>= fun (c, f'') -> return ((b, c), f''));;
  end

  open Parser

(* 'droppe' le resultat d'un parser et le remplace par () *)
let drop p = map (fun _ -> ()) p;;

(* Parser qui droppe le token s'il existe bien *)
let p_TOKEN p = drop ( ptest ((=) p) );;

(* On va créer les parsers de chaque token simple existant *)

let p_TO = p_TOKEN TO;;
let p_DOUBLEPTVIRG = p_TOKEN DOUBLEPTVIRG;;
let p_CONS = p_TOKEN CONS;;
let p_PLUS = p_TOKEN PLUS;;
let p_MOINS = p_TOKEN MOINS;;
let p_MULT = p_TOKEN MULT;;
let p_DIV = p_TOKEN DIV;;
let p_FPLUS = p_TOKEN FPLUS;;
let p_FMOINS = p_TOKEN FMOINS;;
let p_FMULT = p_TOKEN FMULT;;
let p_FDIV = p_TOKEN FDIV;;
let p_PARO = p_TOKEN PARO;;
let p_PARF = p_TOKEN PARF;;
let p_CROO = p_TOKEN CROO;;
let p_CROF = p_TOKEN CROF;;
let p_EQU = p_TOKEN EQU;;
let p_NOTEQ = p_TOKEN NOTEQ;;
let p_INF = p_TOKEN INF;;
let p_SUP = p_TOKEN SUP;;
let p_INFEQ = p_TOKEN INFEQ;;
let p_SUPEQ = p_TOKEN SUPEQ;;
let p_CONCAT = p_TOKEN CONCAT;;
let p_AND = p_TOKEN AND;;
let p_OR = p_TOKEN OR;;
let p_VIRG = p_TOKEN VIRG;;
let p_PTVIRG = p_TOKEN PTVIRG;;
let p_MOD = p_TOKEN MOD;;
let p_LET = p_TOKEN LET;;
let p_IN = p_TOKEN IN;;
let p_REC = p_TOKEN REC;;
let p_FUN = p_TOKEN FUN;;
let p_MATCH = p_TOKEN MATCH;;
let p_WITH = p_TOKEN WITH;;
let p_BAR = p_TOKEN BAR;;
let p_IF = p_TOKEN IF;;
let p_THEN = p_TOKEN THEN;;
let p_ELSE = p_TOKEN ELSE;;
let p_TYPE = p_TOKEN TYPE;;
let p_QUOTE = p_TOKEN QUOTE;;
let p_EOF = p_TOKEN EOF;; (*pvide ?*)

(* On va, maintenant, créer les parsers de chaque token composé existant *)

let p_IDENT = (ptest isident) >>= fun token -> return (unident token);;
let p_BOOL = (ptest isbool) >>= fun token -> return (unbool token);;
let p_ENTIER = (ptest isint) >>= fun token -> return (unint token);;

(* 
Expr → let Liaison in Expr 
| let rec Liaison in Expr 
| (Expr Binop Expr) 
| (Expr) 
| (Expr Expr) 
| if Expr then Expr else Expr 
| (fun ident −> Expr)
| ident
| Constant 

Liaison → ident = Expr
Binop → Arithop | Boolop | Relop | @ | ::
Arithop → + | − | * | /
Boolop → && | ||
Relop → = | <> | <= | < | >= | >
Constant → entier | booleen | [] | ()
*)

(* Ici, on créé la fonction qui parse récursivement une expression*)

let rec p_Expr : (token, expr) parser = fun flux ->
  (
    (p_LET 
    >>= fun () -> p_Liaison 
    >>= fun (id, e1) -> p_IN 
    >>= fun () -> p_Expr 
    >>= fun e2 -> return (ELet (id, e1, e2))    
    (* ELet (id, e1, e2) -> "let id = e1 in e2"*)) ++
    (p_LET 
    >>= fun () -> p_REC
    >>= fun () -> p_Liaison 
    >>= fun (id, e1) -> p_IN 
    >>= fun () -> p_Expr 
    >>= fun e2 -> return (ELetrec (id, e1, e2))    
    (* ELetrec (id, e1, e2) -> "let rec id = e1 in e2"*)) ++
    (p_PARO
    >>= fun () -> p_Expr
    >>= fun e1 -> p_Binop
    >>= fun token -> p_Expr
    >>= fun e2 -> p_PARF
    >>= fun () -> return (EApply(EApply(EBinop(token), e1), e2))
    (* (((+) 0) 1) <==> 1 + 0 <==> EApply(EApply(EBinop(PLUS), 0), 1) *)) ++
    (p_PARO
    >>= fun () -> p_Expr
    >>= fun e1 -> p_Expr
    >>= fun e2 -> p_PARF
    >>= fun () -> return (EApply(e1, e2))) ++ 
    (p_PARO
    >>= fun () -> p_Expr
    >>= fun e -> return e) (*pas sûr du return e*) ++
    (p_IF
    >>= fun () -> p_Expr
    >>= fun e1 -> p_THEN
    >>= fun () -> p_Expr
    >>= fun e2 -> p_ELSE
    >>= fun () -> p_Expr
    >>= fun e3 -> return (EIf(e1, e2, e3))) ++
    (p_PARO 
    >>= fun () -> p_FUN
    >>= fun () -> p_IDENT
    >>= fun id -> p_TO
    >>= fun () -> p_Expr
    >>= fun e -> p_PARF
    >>= fun () -> return (EFun(id, e))) ++
    (p_IDENT
    >>= fun id -> return (EIdent(id))) ++
    (p_Constant
    >>= fun c -> return (EConstant(c)))
  ) flux

(* Ici, on créé la fonction qui parse récursivement une liaison*)
and p_Liaison : (token, (ident * expr)) parser = fun flux ->
  (
    (p_IDENT 
    >>= fun ident -> p_EQU 
    >>= fun () -> p_Expr 
    >>= fun expr -> return (ident, expr))
  ) flux

and p_Binop : (token, token) parser = fun flux ->
  (
    (* Arithop Integers*)
    (p_PLUS 
    >>= fun () -> return PLUS) ++
    (p_MOINS 
    >>= fun () -> return MOINS) ++
    (p_MULT 
    >>= fun () -> return MULT) ++
    (p_DIV 
    >>= fun () -> return DIV) ++
    (* Arithop Floats *)
    (p_FPLUS 
    >>= fun () -> return FPLUS) ++
    (p_FMOINS 
    >>= fun () -> return FMOINS) ++
    (p_FMULT 
    >>= fun () -> return FMULT) ++
    (p_FDIV 
    >>= fun () -> return FDIV) ++
    (* Concat *)
    (p_CONCAT 
    >>= fun () -> return CONCAT) ++
    (* Boolop *)
    (p_AND 
    >>= fun () -> return AND) ++
    (p_OR 
    >>= fun () -> return OR) ++
    (* Relop *)
    (p_EQU 
    >>= fun () -> return EQU) ++
    (p_NOTEQ 
    >>= fun () -> return NOTEQ) ++
    (p_INF 
    >>= fun () -> return INF) ++
    (p_SUP 
    >>= fun () -> return SUP) ++
    (p_INFEQ 
    >>= fun () -> return INFEQ) ++
    (p_SUPEQ 
    >>= fun () -> return SUPEQ) ++
    (p_CONS 
    >>= fun () -> return CONS)
  ) flux
  and p_Constant : (token, constant) parser = fun flux ->
    (
      (p_ENTIER >>= fun e -> return (CEntier(e)))
      ++ 
      (p_BOOL >>= fun b -> return (CBooleen(b))) 
      ++ 
      (p_CROO >>= fun () -> p_CROF >>= fun () -> return (CNil)) 
      ++
      (p_PARF >>= fun () -> p_PARF >>= fun () -> return (CUnit))
    ) flux;;

    let parsing_validation repertoire file = 
      let f = read_miniml_tokens_from_file (repertoire ^ file) in
      let progs = p_Expr f in
      print_string ("\n"^file^"\n");
      match Solution.uncons progs with
      | None        -> (Format.printf "\n** parsing failed ! **@.";)
      | Some (expression, _) -> 
        begin
          print_expr Format.std_formatter (fst expression);
          Format.printf "\n** parsing succesfull ! **@.";
        end

    (* Commenter la fonction suivante pour lancer dune runtest sans erreur*)
    let _ =
      let repertoire = "lib/tests/" in
      let files = Sys.readdir repertoire in
      Array.iter (fun file -> parsing_validation repertoire file) files