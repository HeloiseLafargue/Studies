%{

(* Partie recopiee dans le fichier CaML genere. *)
(* Ouverture de modules exploites dans les actions *)
(* Declarations de types, de constantes, de fonctions, d'exceptions exploites dans les actions *)

%}

/* Declaration des unites lexicales et de leur type si une valeur particuliere leur est associee */

%token UL_PAROUV UL_PARFER
%token UL_PT

/* Defini le type des donnees associees a l'unite lexicale */
%token <int> UL_ENTIER
%token <string> UL_IDENT

/* Unite lexicale particuliere qui represente la fin du fichier */

%token UL_FIN

/* Type renvoye pour le nom terminal document */
%type <unit> scheme


/* Le non terminal document est l'axiome */
%start scheme

%% /* Regles de productions */

scheme : expression UL_FIN { (print_endline "scheme : expression UL_FIN ") }

expression : expression2 { (print_endline "expression : expression2")}
|UL_IDENT { (print_endline "expression : identificateur")}
|UL_ENTIER { (print_endline "expression : entier")}

expression2 : UL_PAROUV expression3 UL_PARFER { (print_endline "expression2 : UL_PAROUV expression3 UL_PARFER")}

expression3 : scheme UL_PT scheme { (print_endline "expression3 : scheme UL_PT scheme")}
|expression4 { (print_endline "expression3 : expression4")}

expression4 : /*mot vide*/ { (print_endline "expression4 : mot")}
|scheme expression4 { (print_endline "expression4 : scheme expression4")}

%%
