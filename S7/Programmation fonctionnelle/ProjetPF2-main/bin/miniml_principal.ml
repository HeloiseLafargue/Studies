(* ouverture de la "library" definie dans lib/dune *)
open Miniml

(* ouverture de modules de la library Miniml *)
open Miniml_parser
open Miniml_typer 

(* Programme principal *)

(* Lecture d'un programme MiniML dans un fichier*)
let lexbuf = Miniml_parser.read_miniml_tokens_from_file Sys.argv.(1);;

let progs = Miniml_parser.p_Expr lexbuf;;


let sols = match Solution.uncons progs with
| None        -> (Format.printf "\n** parsing failed ! **@.";)
| Some (expression, _) -> 
    begin
        (* Avoir le type d'une expression et ses equations *)
        let (t, l) = type_expr (fst expression)
        in
        (* Affichage du type obtenu *)
        Miniml_printer.print_typ TypeVariable.fprintf Format.std_formatter t;

        (* Affichage de l'existence d'une solution *)
        let existence = Miniml_typer.resolution l in
        if existence then
            Format.printf "\n** Solution exists **@."
        else
            Format.printf "\n** Solution does not exist **@."
    end
;;
