
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | VOID
    | VIRG
    | VIDE
    | TYPEIDENT of (
# 16 "parserJava.mly"
       (string)
# 18 "parserJava.ml"
  )
    | TANTQUE
    | STRING
    | SINON
    | SI
    | RETOUR
    | PTVIRG
    | PAROUV
    | PARFER
    | OPSUPEG
    | OPSUP
    | OPPT
    | OPPLUS
    | OPOU
    | OPNONEG
    | OPNON
    | OPMULT
    | OPMOINS
    | OPMOD
    | OPINFEG
    | OPINF
    | OPET
    | OPEG
    | OPDIV
    | NOUVEAU
    | INT
    | IMPORT
    | IDENT of (
# 16 "parserJava.mly"
       (string)
# 49 "parserJava.ml"
  )
    | FLOTTANT of (
# 23 "parserJava.mly"
       (float)
# 54 "parserJava.ml"
  )
    | FLOAT
    | FIN
    | ENTIER of (
# 22 "parserJava.mly"
       (int)
# 61 "parserJava.ml"
  )
    | CROOUV
    | CROFER
    | CHAR
    | CHAINE of (
# 26 "parserJava.mly"
       (string)
# 69 "parserJava.ml"
  )
    | CARACTERE of (
# 25 "parserJava.mly"
       (char)
# 74 "parserJava.ml"
  )
    | BOOLEEN of (
# 24 "parserJava.mly"
       (bool)
# 79 "parserJava.ml"
  )
    | BOOL
    | ASSIGN
    | ACCOUV
    | ACCFER
  
end

include MenhirBasics

# 1 "parserJava.mly"
  

(* Partie recopiee dans le fichier CaML genere. *)
(* Ouverture de modules exploites dans les actions *)
(* Declarations de types, de constantes, de fonctions, d'exceptions exploites dans les actions *)

(* let nbrVariables = ref 0;; *)

let nbrFonctions = ref 0;;


# 102 "parserJava.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState000 : ('s, _menhir_box_fichier) _menhir_state
    (** State 000.
        Stack shape : .
        Start symbol: fichier. *)

  | MenhirState003 : (('s, _menhir_box_fichier) _menhir_cell1_VOID _menhir_cell0_IDENT, _menhir_box_fichier) _menhir_state
    (** State 003.
        Stack shape : VOID IDENT.
        Start symbol: fichier. *)

  | MenhirState011 : (('s _menhir_cell0_IDENT, _menhir_box_fichier) _menhir_cell1_typeStruct _menhir_cell0_IDENT, _menhir_box_fichier) _menhir_state
    (** State 011.
        Stack shape : IDENT typeStruct IDENT.
        Start symbol: fichier. *)

  | MenhirState012 : ((('s, _menhir_box_fichier) _menhir_cell1_typeStruct _menhir_cell0_IDENT, _menhir_box_fichier) _menhir_cell1_VIRG, _menhir_box_fichier) _menhir_state
    (** State 012.
        Stack shape : typeStruct IDENT VIRG.
        Start symbol: fichier. *)

  | MenhirState014 : (((('s, _menhir_box_fichier) _menhir_cell1_typeStruct _menhir_cell0_IDENT, _menhir_box_fichier) _menhir_cell1_VIRG, _menhir_box_fichier) _menhir_cell1_typeStruct _menhir_cell0_IDENT, _menhir_box_fichier) _menhir_state
    (** State 014.
        Stack shape : typeStruct IDENT VIRG typeStruct IDENT.
        Start symbol: fichier. *)

  | MenhirState025 : (('s, _menhir_box_fichier) _menhir_cell1_typeStruct _menhir_cell0_IDENT, _menhir_box_fichier) _menhir_state
    (** State 025.
        Stack shape : typeStruct IDENT.
        Start symbol: fichier. *)

  | MenhirState030 : (('s, _menhir_box_fichier) _menhir_cell1_fonction, _menhir_box_fichier) _menhir_state
    (** State 030.
        Stack shape : fonction.
        Start symbol: fichier. *)

  | MenhirState032 : (('s, _menhir_box_fichier) _menhir_cell1_entete, _menhir_box_fichier) _menhir_state
    (** State 032.
        Stack shape : entete.
        Start symbol: fichier. *)

  | MenhirState033 : (('s, _menhir_box_fichier) _menhir_cell1_ACCOUV, _menhir_box_fichier) _menhir_state
    (** State 033.
        Stack shape : ACCOUV.
        Start symbol: fichier. *)

  | MenhirState034 : ((('s, _menhir_box_fichier) _menhir_cell1_ACCOUV, _menhir_box_fichier) _menhir_cell1_variables, _menhir_box_fichier) _menhir_state
    (** State 034.
        Stack shape : ACCOUV variables.
        Start symbol: fichier. *)

  | MenhirState036 : (('s, _menhir_box_fichier) _menhir_cell1_TANTQUE, _menhir_box_fichier) _menhir_state
    (** State 036.
        Stack shape : TANTQUE.
        Start symbol: fichier. *)

  | MenhirState062 : (('s, _menhir_box_fichier) _menhir_cell1_unaires, _menhir_box_fichier) _menhir_state
    (** State 062.
        Stack shape : unaires.
        Start symbol: fichier. *)

  | MenhirState063 : (('s, _menhir_box_fichier) _menhir_cell1_unaire, _menhir_box_fichier) _menhir_state
    (** State 063.
        Stack shape : unaire.
        Start symbol: fichier. *)

  | MenhirState071 : (('s, _menhir_box_fichier) _menhir_cell1_binaire, _menhir_box_fichier) _menhir_state
    (** State 071.
        Stack shape : binaire.
        Start symbol: fichier. *)

  | MenhirState080 : (('s, _menhir_box_fichier) _menhir_cell1_unaires _menhir_cell0_IDENT, _menhir_box_fichier) _menhir_state
    (** State 080.
        Stack shape : unaires IDENT.
        Start symbol: fichier. *)

  | MenhirState089 : (('s, _menhir_box_fichier) _menhir_cell1_unaires _menhir_cell0_bloc4, _menhir_box_fichier) _menhir_state
    (** State 089.
        Stack shape : unaires bloc4.
        Start symbol: fichier. *)

  | MenhirState090 : (('s, _menhir_box_fichier) _menhir_cell1_PAROUV, _menhir_box_fichier) _menhir_state
    (** State 090.
        Stack shape : PAROUV.
        Start symbol: fichier. *)

  | MenhirState091 : ((('s, _menhir_box_fichier) _menhir_cell1_PAROUV, _menhir_box_fichier) _menhir_cell1_expression, _menhir_box_fichier) _menhir_state
    (** State 091.
        Stack shape : PAROUV expression.
        Start symbol: fichier. *)

  | MenhirState092 : ((('s, _menhir_box_fichier) _menhir_cell1_expression, _menhir_box_fichier) _menhir_cell1_VIRG, _menhir_box_fichier) _menhir_state
    (** State 092.
        Stack shape : expression VIRG.
        Start symbol: fichier. *)

  | MenhirState093 : (((('s, _menhir_box_fichier) _menhir_cell1_expression, _menhir_box_fichier) _menhir_cell1_VIRG, _menhir_box_fichier) _menhir_cell1_expression, _menhir_box_fichier) _menhir_state
    (** State 093.
        Stack shape : expression VIRG expression.
        Start symbol: fichier. *)

  | MenhirState098 : (('s, _menhir_box_fichier) _menhir_cell1_CROOUV, _menhir_box_fichier) _menhir_state
    (** State 098.
        Stack shape : CROOUV.
        Start symbol: fichier. *)

  | MenhirState102 : (('s, _menhir_box_fichier) _menhir_cell1_suffixe, _menhir_box_fichier) _menhir_state
    (** State 102.
        Stack shape : suffixe.
        Start symbol: fichier. *)

  | MenhirState106 : ((('s, _menhir_box_fichier) _menhir_cell1_TANTQUE, _menhir_box_fichier) _menhir_cell1_expression, _menhir_box_fichier) _menhir_state
    (** State 106.
        Stack shape : TANTQUE expression.
        Start symbol: fichier. *)

  | MenhirState109 : (('s, _menhir_box_fichier) _menhir_cell1_SI, _menhir_box_fichier) _menhir_state
    (** State 109.
        Stack shape : SI.
        Start symbol: fichier. *)

  | MenhirState111 : ((('s, _menhir_box_fichier) _menhir_cell1_SI, _menhir_box_fichier) _menhir_cell1_expression, _menhir_box_fichier) _menhir_state
    (** State 111.
        Stack shape : SI expression.
        Start symbol: fichier. *)

  | MenhirState113 : (((('s, _menhir_box_fichier) _menhir_cell1_SI, _menhir_box_fichier) _menhir_cell1_expression, _menhir_box_fichier) _menhir_cell1_bloc, _menhir_box_fichier) _menhir_state
    (** State 113.
        Stack shape : SI expression bloc.
        Start symbol: fichier. *)

  | MenhirState116 : (('s, _menhir_box_fichier) _menhir_cell1_RETOUR, _menhir_box_fichier) _menhir_state
    (** State 116.
        Stack shape : RETOUR.
        Start symbol: fichier. *)

  | MenhirState121 : (('s, _menhir_box_fichier) _menhir_cell1_instruction, _menhir_box_fichier) _menhir_state
    (** State 121.
        Stack shape : instruction.
        Start symbol: fichier. *)

  | MenhirState125 : (('s, _menhir_box_fichier) _menhir_cell1_variable, _menhir_box_fichier) _menhir_state
    (** State 125.
        Stack shape : variable.
        Start symbol: fichier. *)


and ('s, 'r) _menhir_cell1_binaire = 
  | MenhirCell1_binaire of 's * ('s, 'r) _menhir_state * (unit)

and ('s, 'r) _menhir_cell1_bloc = 
  | MenhirCell1_bloc of 's * ('s, 'r) _menhir_state * (unit)

and 's _menhir_cell0_bloc4 = 
  | MenhirCell0_bloc4 of 's * (unit)

and 's _menhir_cell0_blocType1 = 
  | MenhirCell0_blocType1 of 's * (unit)

and ('s, 'r) _menhir_cell1_entete = 
  | MenhirCell1_entete of 's * ('s, 'r) _menhir_state * (unit)

and ('s, 'r) _menhir_cell1_expression = 
  | MenhirCell1_expression of 's * ('s, 'r) _menhir_state * (unit)

and ('s, 'r) _menhir_cell1_fonction = 
  | MenhirCell1_fonction of 's * ('s, 'r) _menhir_state * (unit)

and ('s, 'r) _menhir_cell1_instruction = 
  | MenhirCell1_instruction of 's * ('s, 'r) _menhir_state * (unit)

and ('s, 'r) _menhir_cell1_suffixe = 
  | MenhirCell1_suffixe of 's * ('s, 'r) _menhir_state * (unit)

and ('s, 'r) _menhir_cell1_typeBase = 
  | MenhirCell1_typeBase of 's * ('s, 'r) _menhir_state * (unit)

and ('s, 'r) _menhir_cell1_typeStruct = 
  | MenhirCell1_typeStruct of 's * ('s, 'r) _menhir_state * (unit)

and ('s, 'r) _menhir_cell1_unaire = 
  | MenhirCell1_unaire of 's * ('s, 'r) _menhir_state * (unit)

and ('s, 'r) _menhir_cell1_unaires = 
  | MenhirCell1_unaires of 's * ('s, 'r) _menhir_state * (unit)

and ('s, 'r) _menhir_cell1_variable = 
  | MenhirCell1_variable of 's * ('s, 'r) _menhir_state * (unit)

and ('s, 'r) _menhir_cell1_variables = 
  | MenhirCell1_variables of 's * ('s, 'r) _menhir_state * (int)

and ('s, 'r) _menhir_cell1_ACCOUV = 
  | MenhirCell1_ACCOUV of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_CROOUV = 
  | MenhirCell1_CROOUV of 's * ('s, 'r) _menhir_state

and 's _menhir_cell0_IDENT = 
  | MenhirCell0_IDENT of 's * (
# 16 "parserJava.mly"
       (string)
# 306 "parserJava.ml"
)

and ('s, 'r) _menhir_cell1_PAROUV = 
  | MenhirCell1_PAROUV of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_RETOUR = 
  | MenhirCell1_RETOUR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_SI = 
  | MenhirCell1_SI of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_TANTQUE = 
  | MenhirCell1_TANTQUE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_VIRG = 
  | MenhirCell1_VIRG of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_VOID = 
  | MenhirCell1_VOID of 's * ('s, 'r) _menhir_state

and _menhir_box_fichier = 
  | MenhirBox_fichier of (unit) [@@unboxed]

let _menhir_action_01 =
  fun () ->
    (
# 130 "parserJava.mly"
                                                                                            ()
# 335 "parserJava.ml"
     : (unit))

let _menhir_action_02 =
  fun () ->
    (
# 130 "parserJava.mly"
                                                                                            ()
# 343 "parserJava.ml"
     : (unit))

let _menhir_action_03 =
  fun () ->
    (
# 130 "parserJava.mly"
                                                                                            ()
# 351 "parserJava.ml"
     : (unit))

let _menhir_action_04 =
  fun () ->
    (
# 130 "parserJava.mly"
                                                                                            ()
# 359 "parserJava.ml"
     : (unit))

let _menhir_action_05 =
  fun () ->
    (
# 130 "parserJava.mly"
                                                                                            ()
# 367 "parserJava.ml"
     : (unit))

let _menhir_action_06 =
  fun () ->
    (
# 130 "parserJava.mly"
                                                                                            ()
# 375 "parserJava.ml"
     : (unit))

let _menhir_action_07 =
  fun () ->
    (
# 130 "parserJava.mly"
                                                                                            ()
# 383 "parserJava.ml"
     : (unit))

let _menhir_action_08 =
  fun () ->
    (
# 130 "parserJava.mly"
                                                                                            ()
# 391 "parserJava.ml"
     : (unit))

let _menhir_action_09 =
  fun () ->
    (
# 130 "parserJava.mly"
                                                                                            ()
# 399 "parserJava.ml"
     : (unit))

let _menhir_action_10 =
  fun () ->
    (
# 130 "parserJava.mly"
                                                                                            ()
# 407 "parserJava.ml"
     : (unit))

let _menhir_action_11 =
  fun () ->
    (
# 129 "parserJava.mly"
                              ( (print_endline "mot vide") )
# 415 "parserJava.ml"
     : (unit))

let _menhir_action_12 =
  fun () ->
    (
# 129 "parserJava.mly"
                              ( (print_endline "mot vide") )
# 423 "parserJava.ml"
     : (unit))

let _menhir_action_13 =
  fun _2 ->
    (
# 88 "parserJava.mly"
     (
	(print_endline "bloc : ACCOUV variables instructions ACCFER");
	(print_string "Nombre de variables = ");
	(print_int _2);
	(print_newline ())
	)
# 436 "parserJava.ml"
     : (unit))

let _menhir_action_14 =
  fun () ->
    (
# 132 "parserJava.mly"
                                                                                              ()
# 444 "parserJava.ml"
     : (unit))

let _menhir_action_15 =
  fun () ->
    (
# 132 "parserJava.mly"
                                                                                              ()
# 452 "parserJava.ml"
     : (unit))

let _menhir_action_16 =
  fun () ->
    (
# 132 "parserJava.mly"
                                                                                              ()
# 460 "parserJava.ml"
     : (unit))

let _menhir_action_17 =
  fun () ->
    (
# 132 "parserJava.mly"
                                                                                              ()
# 468 "parserJava.ml"
     : (unit))

let _menhir_action_18 =
  fun () ->
    (
# 132 "parserJava.mly"
                                                                                              ()
# 476 "parserJava.ml"
     : (unit))

let _menhir_action_19 =
  fun () ->
    (
# 132 "parserJava.mly"
                                                                                              ()
# 484 "parserJava.ml"
     : (unit))

let _menhir_action_20 =
  fun () ->
    (
# 132 "parserJava.mly"
                                                                                              ()
# 492 "parserJava.ml"
     : (unit))

let _menhir_action_21 =
  fun () ->
    (
# 133 "parserJava.mly"
                                                 ()
# 500 "parserJava.ml"
     : (unit))

let _menhir_action_22 =
  fun () ->
    (
# 133 "parserJava.mly"
                                                 ()
# 508 "parserJava.ml"
     : (unit))

let _menhir_action_23 =
  fun () ->
    (
# 134 "parserJava.mly"
                                         ()
# 516 "parserJava.ml"
     : (unit))

let _menhir_action_24 =
  fun () ->
    (
# 134 "parserJava.mly"
                                         ()
# 524 "parserJava.ml"
     : (unit))

let _menhir_action_25 =
  fun () ->
    (
# 138 "parserJava.mly"
                                        ( (print_endline "mot vide") )
# 532 "parserJava.ml"
     : (unit))

let _menhir_action_26 =
  fun () ->
    (
# 138 "parserJava.mly"
                                        ( (print_endline "mot vide") )
# 540 "parserJava.ml"
     : (unit))

let _menhir_action_27 =
  fun () ->
    (
# 139 "parserJava.mly"
                                            ()
# 548 "parserJava.ml"
     : (unit))

let _menhir_action_28 =
  fun () ->
    (
# 143 "parserJava.mly"
                                                       ()
# 556 "parserJava.ml"
     : (unit))

let _menhir_action_29 =
  fun () ->
    (
# 143 "parserJava.mly"
                                                       ()
# 564 "parserJava.ml"
     : (unit))

let _menhir_action_30 =
  fun () ->
    (
# 143 "parserJava.mly"
                                                       ()
# 572 "parserJava.ml"
     : (unit))

let _menhir_action_31 =
  fun () ->
    (
# 143 "parserJava.mly"
                                                       ()
# 580 "parserJava.ml"
     : (unit))

let _menhir_action_32 =
  fun () ->
    (
# 143 "parserJava.mly"
                                                       ()
# 588 "parserJava.ml"
     : (unit))

let _menhir_action_33 =
  fun () ->
    (
# 143 "parserJava.mly"
                                                       ()
# 596 "parserJava.ml"
     : (unit))

let _menhir_action_34 =
  fun () ->
    (
# 144 "parserJava.mly"
                            ( (print_endline "mot vide") )
# 604 "parserJava.ml"
     : (unit))

let _menhir_action_35 =
  fun () ->
    (
# 144 "parserJava.mly"
                            ( (print_endline "mot vide") )
# 612 "parserJava.ml"
     : (unit))

let _menhir_action_36 =
  fun () ->
    (
# 73 "parserJava.mly"
                                 ( (print_endline "declTab : /* Lambda, mot vide */") )
# 620 "parserJava.ml"
     : (unit))

let _menhir_action_37 =
  fun () ->
    (
# 74 "parserJava.mly"
                        ( (print_endline "declTab : CROOUV CROFER") )
# 628 "parserJava.ml"
     : (unit))

let _menhir_action_38 =
  fun () ->
    (
# 118 "parserJava.mly"
                            ( (print_endline "instruction : rien") )
# 636 "parserJava.ml"
     : (unit))

let _menhir_action_39 =
  fun () ->
    (
# 118 "parserJava.mly"
                            ( (print_endline "instruction : rien") )
# 644 "parserJava.ml"
     : (unit))

let _menhir_action_40 =
  fun () ->
    (
# 78 "parserJava.mly"
                                                    ( (print_endline "entete : typeStruct IDENT PAROUV parsFormels PARFER") )
# 652 "parserJava.ml"
     : (unit))

let _menhir_action_41 =
  fun () ->
    (
# 79 "parserJava.mly"
                                              ( (print_endline "entete : VOID IDENT PAROUV parsFormels PARFER") )
# 660 "parserJava.ml"
     : (unit))

let _menhir_action_42 =
  fun () ->
    (
# 121 "parserJava.mly"
                                      ()
# 668 "parserJava.ml"
     : (unit))

let _menhir_action_43 =
  fun () ->
    (
# 121 "parserJava.mly"
                                      ()
# 676 "parserJava.ml"
     : (unit))

let _menhir_action_44 =
  fun () ->
    (
# 59 "parserJava.mly"
                        ( (print_endline "fichier : programme FIN");(print_string "Nombre de fonctions : ");(print_int !nbrFonctions);(print_newline()) )
# 684 "parserJava.ml"
     : (unit))

let _menhir_action_45 =
  fun () ->
    (
# 76 "parserJava.mly"
                        ( (print_endline "fonction : entete bloc") )
# 692 "parserJava.ml"
     : (unit))

let _menhir_action_46 =
  fun () ->
    (
# 113 "parserJava.mly"
                                               ( (print_endline "instruction : expression PTVIRG") )
# 700 "parserJava.ml"
     : (unit))

let _menhir_action_47 =
  fun () ->
    (
# 116 "parserJava.mly"
                                                         ( (print_endline "instruction : RETURN expression PTVIRG") )
# 708 "parserJava.ml"
     : (unit))

let _menhir_action_48 =
  fun () ->
    (
# 116 "parserJava.mly"
                                                         ( (print_endline "instruction : RETURN expression PTVIRG") )
# 716 "parserJava.ml"
     : (unit))

let _menhir_action_49 =
  fun () ->
    (
# 116 "parserJava.mly"
                                                         ( (print_endline "instruction : RETURN expression PTVIRG") )
# 724 "parserJava.ml"
     : (unit))

let _menhir_action_50 =
  fun () ->
    (
# 109 "parserJava.mly"
                                        ( (print_endline "instructions : instruction") )
# 732 "parserJava.ml"
     : (unit))

let _menhir_action_51 =
  fun () ->
    (
# 110 "parserJava.mly"
       ((print_endline "instructions : /* Lambda, mot vide */"))
# 740 "parserJava.ml"
     : (unit))

let _menhir_action_52 =
  fun () ->
    (
# 81 "parserJava.mly"
                                     ( (print_endline "parsFormels : /* Lambda, mot vide */") )
# 748 "parserJava.ml"
     : (unit))

let _menhir_action_53 =
  fun () ->
    (
# 82 "parserJava.mly"
                                                ( (print_endline "parsFormels : typeStruct IDENT suiteParsFormels") )
# 756 "parserJava.ml"
     : (unit))

let _menhir_action_54 =
  fun () ->
    (
# 61 "parserJava.mly"
                                   ( (nbrFonctions := 0); (print_endline "programme : /* Lambda, mot vide */") )
# 764 "parserJava.ml"
     : (unit))

let _menhir_action_55 =
  fun () ->
    (
# 62 "parserJava.mly"
                               ( (nbrFonctions := !nbrFonctions + 1);(print_endline "programme : fonction programme") )
# 772 "parserJava.ml"
     : (unit))

let _menhir_action_56 =
  fun () ->
    (
# 137 "parserJava.mly"
                                                               ()
# 780 "parserJava.ml"
     : (unit))

let _menhir_action_57 =
  fun () ->
    (
# 137 "parserJava.mly"
                                                               ()
# 788 "parserJava.ml"
     : (unit))

let _menhir_action_58 =
  fun () ->
    (
# 136 "parserJava.mly"
                              ( (print_endline "mot vide") )
# 796 "parserJava.ml"
     : (unit))

let _menhir_action_59 =
  fun () ->
    (
# 136 "parserJava.mly"
                              ( (print_endline "mot vide") )
# 804 "parserJava.ml"
     : (unit))

let _menhir_action_60 =
  fun () ->
    (
# 84 "parserJava.mly"
                                          ( (print_endline "suiteParsFormels : /* Lambda, mot vide */") )
# 812 "parserJava.ml"
     : (unit))

let _menhir_action_61 =
  fun () ->
    (
# 85 "parserJava.mly"
                                                          ( (print_endline "suiteParsFormels : VIRG typeStruct IDENT suiteParsFormels") )
# 820 "parserJava.ml"
     : (unit))

let _menhir_action_62 =
  fun () ->
    (
# 66 "parserJava.mly"
               ( (print_endline "typeBase : INT") )
# 828 "parserJava.ml"
     : (unit))

let _menhir_action_63 =
  fun () ->
    (
# 67 "parserJava.mly"
                 ( (print_endline "typeBase : FLOAT") )
# 836 "parserJava.ml"
     : (unit))

let _menhir_action_64 =
  fun () ->
    (
# 68 "parserJava.mly"
                ( (print_endline "typeBase : BOOL") )
# 844 "parserJava.ml"
     : (unit))

let _menhir_action_65 =
  fun () ->
    (
# 69 "parserJava.mly"
                ( (print_endline "typeBase : CHAR") )
# 852 "parserJava.ml"
     : (unit))

let _menhir_action_66 =
  fun () ->
    (
# 70 "parserJava.mly"
                  ( (print_endline "typeBase : STRING") )
# 860 "parserJava.ml"
     : (unit))

let _menhir_action_67 =
  fun () ->
    (
# 71 "parserJava.mly"
                     ( (print_endline "typeBase : TYPEIDENT") )
# 868 "parserJava.ml"
     : (unit))

let _menhir_action_68 =
  fun () ->
    (
# 64 "parserJava.mly"
                              ( (print_endline "typeStruct : typeBase declTab") )
# 876 "parserJava.ml"
     : (unit))

let _menhir_action_69 =
  fun () ->
    (
# 142 "parserJava.mly"
                            ()
# 884 "parserJava.ml"
     : (unit))

let _menhir_action_70 =
  fun () ->
    (
# 127 "parserJava.mly"
             ()
# 892 "parserJava.ml"
     : (unit))

let _menhir_action_71 =
  fun () ->
    (
# 127 "parserJava.mly"
             ()
# 900 "parserJava.ml"
     : (unit))

let _menhir_action_72 =
  fun () ->
    (
# 127 "parserJava.mly"
             ()
# 908 "parserJava.ml"
     : (unit))

let _menhir_action_73 =
  fun () ->
    (
# 127 "parserJava.mly"
             ()
# 916 "parserJava.ml"
     : (unit))

let _menhir_action_74 =
  fun () ->
    (
# 123 "parserJava.mly"
                           ( (print_endline "mot vide") )
# 924 "parserJava.ml"
     : (unit))

let _menhir_action_75 =
  fun () ->
    (
# 123 "parserJava.mly"
                           ( (print_endline "mot vide") )
# 932 "parserJava.ml"
     : (unit))

let _menhir_action_76 =
  fun () ->
    (
# 106 "parserJava.mly"
                                   ( (print_endline "variable : typeStruct IDENT PTVIRG") )
# 940 "parserJava.ml"
     : (unit))

let _menhir_action_77 =
  fun () ->
    (
# 96 "parserJava.mly"
   (
		(print_endline "variables : /* Lambda, mot vide */");
		0
		)
# 951 "parserJava.ml"
     : (int))

let _menhir_action_78 =
  fun _2 ->
    (
# 101 "parserJava.mly"
   (
		(print_endline "variables : variable variables");
		(_2 + 1)
		)
# 962 "parserJava.ml"
     : (int))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ACCFER ->
        "ACCFER"
    | ACCOUV ->
        "ACCOUV"
    | ASSIGN ->
        "ASSIGN"
    | BOOL ->
        "BOOL"
    | BOOLEEN _ ->
        "BOOLEEN"
    | CARACTERE _ ->
        "CARACTERE"
    | CHAINE _ ->
        "CHAINE"
    | CHAR ->
        "CHAR"
    | CROFER ->
        "CROFER"
    | CROOUV ->
        "CROOUV"
    | ENTIER _ ->
        "ENTIER"
    | FIN ->
        "FIN"
    | FLOAT ->
        "FLOAT"
    | FLOTTANT _ ->
        "FLOTTANT"
    | IDENT _ ->
        "IDENT"
    | IMPORT ->
        "IMPORT"
    | INT ->
        "INT"
    | NOUVEAU ->
        "NOUVEAU"
    | OPDIV ->
        "OPDIV"
    | OPEG ->
        "OPEG"
    | OPET ->
        "OPET"
    | OPINF ->
        "OPINF"
    | OPINFEG ->
        "OPINFEG"
    | OPMOD ->
        "OPMOD"
    | OPMOINS ->
        "OPMOINS"
    | OPMULT ->
        "OPMULT"
    | OPNON ->
        "OPNON"
    | OPNONEG ->
        "OPNONEG"
    | OPOU ->
        "OPOU"
    | OPPLUS ->
        "OPPLUS"
    | OPPT ->
        "OPPT"
    | OPSUP ->
        "OPSUP"
    | OPSUPEG ->
        "OPSUPEG"
    | PARFER ->
        "PARFER"
    | PAROUV ->
        "PAROUV"
    | PTVIRG ->
        "PTVIRG"
    | RETOUR ->
        "RETOUR"
    | SI ->
        "SI"
    | SINON ->
        "SINON"
    | STRING ->
        "STRING"
    | TANTQUE ->
        "TANTQUE"
    | TYPEIDENT _ ->
        "TYPEIDENT"
    | VIDE ->
        "VIDE"
    | VIRG ->
        "VIRG"
    | VOID ->
        "VOID"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37-39"]
  
  let rec _menhir_run_028 : type  ttv_stack. ttv_stack -> _menhir_box_fichier =
    fun _menhir_stack ->
      let _v = _menhir_action_44 () in
      MenhirBox_fichier _v
  
  let rec _menhir_run_031 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_fonction -> _menhir_box_fichier =
    fun _menhir_stack ->
      let MenhirCell1_fonction (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _ = _menhir_action_55 () in
      _menhir_goto_programme _menhir_stack _menhir_s
  
  and _menhir_goto_programme : type  ttv_stack. ttv_stack -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _menhir_box_fichier =
    fun _menhir_stack _menhir_s ->
      match _menhir_s with
      | MenhirState030 ->
          _menhir_run_031 _menhir_stack
      | MenhirState000 ->
          _menhir_run_028 _menhir_stack
      | _ ->
          _menhir_fail ()
  
  let rec _menhir_run_001 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_VOID (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | PAROUV ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TYPEIDENT _ ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v = _menhir_action_67 () in
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState003 _tok
              | STRING ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v = _menhir_action_66 () in
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState003 _tok
              | INT ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v = _menhir_action_62 () in
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState003 _tok
              | FLOAT ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v = _menhir_action_63 () in
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState003 _tok
              | CHAR ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v = _menhir_action_65 () in
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState003 _tok
              | BOOL ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v = _menhir_action_64 () in
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState003 _tok
              | PARFER ->
                  let _ = _menhir_action_52 () in
                  _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_016 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_typeBase (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | CROOUV ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | CROFER ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _ = _menhir_action_37 () in
              _menhir_goto_declTab _menhir_stack _menhir_lexbuf _menhir_lexer _tok
          | _ ->
              _eRR ())
      | IDENT _ ->
          let _ = _menhir_action_36 () in
          _menhir_goto_declTab _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_declTab : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_typeBase -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let MenhirCell1_typeBase (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _v = _menhir_action_68 () in
      _menhir_goto_typeStruct _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_typeStruct : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState033 ->
          _menhir_run_127 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
      | MenhirState125 ->
          _menhir_run_127 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
      | MenhirState030 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState012 ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState003 ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_127 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | IDENT _ ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | PTVIRG ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_76 () in
              let _menhir_stack = MenhirCell1_variable (_menhir_stack, _menhir_s, _v) in
              (match (_tok : MenhirBasics.token) with
              | TYPEIDENT _ ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v = _menhir_action_67 () in
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState125 _tok
              | STRING ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v = _menhir_action_66 () in
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState125 _tok
              | INT ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v = _menhir_action_62 () in
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState125 _tok
              | FLOAT ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v = _menhir_action_63 () in
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState125 _tok
              | CHAR ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v = _menhir_action_65 () in
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState125 _tok
              | BOOL ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v = _menhir_action_64 () in
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState125 _tok
              | ACCFER | ASSIGN | BOOLEEN _ | CARACTERE _ | ENTIER _ | FLOTTANT _ | IDENT _ | NOUVEAU | OPDIV | OPET | OPMOD | OPMOINS | OPMULT | OPNONEG | OPOU | OPPLUS | OPPT | PAROUV | PTVIRG | RETOUR | SI | TANTQUE | VIDE ->
                  let _v = _menhir_action_77 () in
                  _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_126 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_variable -> _ -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_variable (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_78 _2 in
      _menhir_goto_variables _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_variables : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState125 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState033 ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_034 : type  ttv_stack. ((ttv_stack, _menhir_box_fichier) _menhir_cell1_ACCOUV as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_variables (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TANTQUE ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | SI ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | RETOUR ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | PAROUV ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | OPPT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_02 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034 _tok
      | OPPLUS ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | OPOU ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034 _tok
      | OPNONEG ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | OPMULT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_05 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034 _tok
      | OPMOINS ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | OPMOD ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034 _tok
      | OPET ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_09 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034 _tok
      | OPDIV ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_06 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034 _tok
      | ASSIGN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_01 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034 _tok
      | PTVIRG ->
          let _ = _menhir_action_12 () in
          _menhir_run_070_spec_034 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | ACCFER ->
          let _ = _menhir_action_51 () in
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BOOLEEN _ | CARACTERE _ | ENTIER _ | FLOTTANT _ | IDENT _ | NOUVEAU | VIDE ->
          let _v = _menhir_action_75 () in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034 _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_035 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_TANTQUE (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PAROUV ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | PAROUV ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState036
          | OPPT ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_02 () in
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState036 _tok
          | OPPLUS ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState036
          | OPOU ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_08 () in
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState036 _tok
          | OPNONEG ->
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState036
          | OPMULT ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_05 () in
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState036 _tok
          | OPMOINS ->
              _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState036
          | OPMOD ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_07 () in
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState036 _tok
          | OPET ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_09 () in
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState036 _tok
          | OPDIV ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_06 () in
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState036 _tok
          | ASSIGN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_01 () in
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState036 _tok
          | PARFER ->
              let _ = _menhir_action_12 () in
              _menhir_run_070_spec_036 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
          | BOOLEEN _ | CARACTERE _ | ENTIER _ | FLOTTANT _ | IDENT _ | NOUVEAU | VIDE ->
              let _v = _menhir_action_75 () in
              _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState036 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_037 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PAROUV (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STRING ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_32 () in
          _menhir_goto_blocType1 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | INT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_28 () in
          _menhir_goto_blocType1 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | IDENT _ ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_33 () in
          _menhir_goto_blocType1 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | FLOAT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_29 () in
          _menhir_goto_blocType1 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | CHAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_31 () in
          _menhir_goto_blocType1 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BOOL ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_30 () in
          _menhir_goto_blocType1 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_blocType1 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_PAROUV -> _ -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_blocType1 (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | CROOUV ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | CROFER ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _ = _menhir_action_34 () in
              _menhir_goto_blocType2 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
          | _ ->
              _eRR ())
      | PARFER ->
          let _ = _menhir_action_35 () in
          _menhir_goto_blocType2 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_blocType2 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_PAROUV _menhir_cell0_blocType1 -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let MenhirCell0_blocType1 (_menhir_stack, _) = _menhir_stack in
      let _ = _menhir_action_69 () in
      match (_tok : MenhirBasics.token) with
      | PARFER ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_PAROUV (_menhir_stack, _menhir_s) = _menhir_stack in
          let _v = _menhir_action_70 () in
          _menhir_goto_unaire _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_unaire : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_063 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_unaire (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | PAROUV ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | OPPLUS ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_71 () in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState063 _tok
      | OPNONEG ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_73 () in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState063 _tok
      | OPMOINS ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_72 () in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState063 _tok
      | BOOLEEN _ | CARACTERE _ | ENTIER _ | FLOTTANT _ | IDENT _ | NOUVEAU | VIDE ->
          let _ = _menhir_action_75 () in
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_067 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_unaire -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let MenhirCell1_unaire (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _v = _menhir_action_74 () in
      _menhir_goto_unaires _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_unaires : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState063 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | MenhirState121 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState109 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState098 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState092 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState090 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState062 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState036 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_060 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_unaires (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VIDE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _ = _menhir_action_18 () in
          _menhir_goto_bloc2 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | NOUVEAU ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v_10 ->
              let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v_10) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | PAROUV ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | PARFER ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let _ = _menhir_action_21 () in
                      _menhir_goto_bloc3 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
                  | _ ->
                      _eRR ())
              | CROOUV ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  (match (_tok : MenhirBasics.token) with
                  | PAROUV ->
                      _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
                  | OPPT ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let _v = _menhir_action_02 () in
                      _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState080 _tok
                  | OPPLUS ->
                      _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
                  | OPOU ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let _v = _menhir_action_08 () in
                      _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState080 _tok
                  | OPNONEG ->
                      _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
                  | OPMULT ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let _v = _menhir_action_05 () in
                      _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState080 _tok
                  | OPMOINS ->
                      _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState080
                  | OPMOD ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let _v = _menhir_action_07 () in
                      _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState080 _tok
                  | OPET ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let _v = _menhir_action_09 () in
                      _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState080 _tok
                  | OPDIV ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let _v = _menhir_action_06 () in
                      _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState080 _tok
                  | ASSIGN ->
                      let _tok = _menhir_lexer _menhir_lexbuf in
                      let _v = _menhir_action_01 () in
                      _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState080 _tok
                  | CROFER ->
                      let _ = _menhir_action_12 () in
                      _menhir_run_070_spec_080 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
                  | BOOLEEN _ | CARACTERE _ | ENTIER _ | FLOTTANT _ | IDENT _ | NOUVEAU | VIDE ->
                      let _v = _menhir_action_75 () in
                      _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState080 _tok
                  | _ ->
                      _eRR ())
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | IDENT _ ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_23 () in
          _menhir_goto_bloc4 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | FLOTTANT _ ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _ = _menhir_action_15 () in
          _menhir_goto_bloc2 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | ENTIER _ ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _ = _menhir_action_14 () in
          _menhir_goto_bloc2 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | CARACTERE _ ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _ = _menhir_action_16 () in
          _menhir_goto_bloc2 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | BOOLEEN _ ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _ = _menhir_action_17 () in
          _menhir_goto_bloc2 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_bloc2 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_unaires -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let MenhirCell1_unaires (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _v = _menhir_action_42 () in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState034 ->
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
      | MenhirState121 ->
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | MenhirState109 ->
          _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState036 ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState098 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | MenhirState092 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState090 ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | MenhirState062 ->
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_123 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PTVIRG ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_46 () in
          _menhir_goto_instruction _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_instruction : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_instruction (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TANTQUE ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState121
      | SI ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState121
      | RETOUR ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState121
      | PAROUV ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState121
      | OPPT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_02 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState121 _tok
      | OPPLUS ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState121
      | OPOU ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState121 _tok
      | OPNONEG ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState121
      | OPMULT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_05 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState121 _tok
      | OPMOINS ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState121
      | OPMOD ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState121 _tok
      | OPET ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_09 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState121 _tok
      | OPDIV ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_06 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState121 _tok
      | ASSIGN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_01 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState121 _tok
      | PTVIRG ->
          let _ = _menhir_action_12 () in
          _menhir_run_070_spec_121 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | ACCFER ->
          let _ = _menhir_action_51 () in
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer
      | BOOLEEN _ | CARACTERE _ | ENTIER _ | FLOTTANT _ | IDENT _ | NOUVEAU | VIDE ->
          let _v = _menhir_action_75 () in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState121 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_108 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_SI (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PAROUV ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | PAROUV ->
              _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState109
          | OPPT ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_02 () in
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState109 _tok
          | OPPLUS ->
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState109
          | OPOU ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_08 () in
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState109 _tok
          | OPNONEG ->
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState109
          | OPMULT ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_05 () in
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState109 _tok
          | OPMOINS ->
              _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState109
          | OPMOD ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_07 () in
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState109 _tok
          | OPET ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_09 () in
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState109 _tok
          | OPDIV ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_06 () in
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState109 _tok
          | ASSIGN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_01 () in
              _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState109 _tok
          | PARFER ->
              let _ = _menhir_action_12 () in
              _menhir_run_070_spec_109 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
          | BOOLEEN _ | CARACTERE _ | ENTIER _ | FLOTTANT _ | IDENT _ | NOUVEAU | VIDE ->
              let _v = _menhir_action_75 () in
              _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState109 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_071 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_binaire (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | OPPT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_02 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071 _tok
      | OPPLUS ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_03 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071 _tok
      | OPOU ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071 _tok
      | OPNONEG ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_10 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071 _tok
      | OPMULT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_05 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071 _tok
      | OPMOINS ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_04 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071 _tok
      | OPMOD ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071 _tok
      | OPET ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_09 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071 _tok
      | OPDIV ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_06 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071 _tok
      | ASSIGN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_01 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071 _tok
      | CROFER | PARFER | PTVIRG | VIRG ->
          let _ = _menhir_action_12 () in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_075 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_binaire -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let MenhirCell1_binaire (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _ = _menhir_action_11 () in
      _menhir_goto_binaires _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
  
  and _menhir_goto_binaires : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok ->
      match _menhir_s with
      | MenhirState071 ->
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | MenhirState034 ->
          _menhir_run_070_spec_034 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | MenhirState121 ->
          _menhir_run_070_spec_121 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | MenhirState116 ->
          _menhir_run_070_spec_116 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | MenhirState109 ->
          _menhir_run_070_spec_109 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | MenhirState036 ->
          _menhir_run_070_spec_036 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | MenhirState098 ->
          _menhir_run_070_spec_098 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | MenhirState090 ->
          _menhir_run_070_spec_090 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | MenhirState092 ->
          _menhir_run_070_spec_092 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | MenhirState080 ->
          _menhir_run_070_spec_080 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | MenhirState062 ->
          _menhir_run_070_spec_062 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_070_spec_034 : type  ttv_stack. ((ttv_stack, _menhir_box_fichier) _menhir_cell1_ACCOUV, _menhir_box_fichier) _menhir_cell1_variables -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let _ = _menhir_action_43 () in
      _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034 _tok
  
  and _menhir_run_070_spec_121 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_instruction -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let _ = _menhir_action_43 () in
      _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState121 _tok
  
  and _menhir_run_070_spec_116 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_RETOUR -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let _ = _menhir_action_43 () in
      _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
  
  and _menhir_run_117 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_RETOUR -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      match (_tok : MenhirBasics.token) with
      | PTVIRG ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_RETOUR (_menhir_stack, _menhir_s) = _menhir_stack in
          let _v = _menhir_action_49 () in
          _menhir_goto_instruction _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_070_spec_109 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_SI -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let _v = _menhir_action_43 () in
      _menhir_run_110 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState109 _tok
  
  and _menhir_run_110 : type  ttv_stack. ((ttv_stack, _menhir_box_fichier) _menhir_cell1_SI as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | PARFER ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ACCOUV ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState111
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_033 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_ACCOUV (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYPEIDENT _ ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_67 () in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState033 _tok
      | STRING ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_66 () in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState033 _tok
      | INT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_62 () in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState033 _tok
      | FLOAT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_63 () in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState033 _tok
      | CHAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_65 () in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState033 _tok
      | BOOL ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_64 () in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState033 _tok
      | ACCFER | ASSIGN | BOOLEEN _ | CARACTERE _ | ENTIER _ | FLOTTANT _ | IDENT _ | NOUVEAU | OPDIV | OPET | OPMOD | OPMOINS | OPMULT | OPNONEG | OPOU | OPPLUS | OPPT | PAROUV | PTVIRG | RETOUR | SI | TANTQUE | VIDE ->
          let _v = _menhir_action_77 () in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState033 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_070_spec_036 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_TANTQUE -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let _v = _menhir_action_43 () in
      _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState036 _tok
  
  and _menhir_run_105 : type  ttv_stack. ((ttv_stack, _menhir_box_fichier) _menhir_cell1_TANTQUE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | PARFER ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ACCOUV ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState106
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_070_spec_098 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_CROOUV -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let _ = _menhir_action_43 () in
      _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
  
  and _menhir_run_099 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_CROOUV -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      match (_tok : MenhirBasics.token) with
      | CROFER ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_CROOUV (_menhir_stack, _menhir_s) = _menhir_stack in
          let _v = _menhir_action_57 () in
          _menhir_goto_suffixe _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_suffixe : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_suffixe (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | PAROUV ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | CROOUV ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState102
      | CROFER | PARFER | PTVIRG | VIRG ->
          let _ = _menhir_action_59 () in
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_090 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PAROUV ->
          let _menhir_stack = MenhirCell1_PAROUV (_menhir_stack, _menhir_s) in
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | OPPT ->
          let _menhir_stack = MenhirCell1_PAROUV (_menhir_stack, _menhir_s) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_02 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState090 _tok
      | OPPLUS ->
          let _menhir_stack = MenhirCell1_PAROUV (_menhir_stack, _menhir_s) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | OPOU ->
          let _menhir_stack = MenhirCell1_PAROUV (_menhir_stack, _menhir_s) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState090 _tok
      | OPNONEG ->
          let _menhir_stack = MenhirCell1_PAROUV (_menhir_stack, _menhir_s) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | OPMULT ->
          let _menhir_stack = MenhirCell1_PAROUV (_menhir_stack, _menhir_s) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_05 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState090 _tok
      | OPMOINS ->
          let _menhir_stack = MenhirCell1_PAROUV (_menhir_stack, _menhir_s) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | OPMOD ->
          let _menhir_stack = MenhirCell1_PAROUV (_menhir_stack, _menhir_s) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState090 _tok
      | OPET ->
          let _menhir_stack = MenhirCell1_PAROUV (_menhir_stack, _menhir_s) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_09 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState090 _tok
      | OPDIV ->
          let _menhir_stack = MenhirCell1_PAROUV (_menhir_stack, _menhir_s) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_06 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState090 _tok
      | ASSIGN ->
          let _menhir_stack = MenhirCell1_PAROUV (_menhir_stack, _menhir_s) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_01 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState090 _tok
      | VIRG ->
          let _menhir_stack = MenhirCell1_PAROUV (_menhir_stack, _menhir_s) in
          let _ = _menhir_action_12 () in
          _menhir_run_070_spec_090 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | PARFER ->
          let _ = _menhir_action_26 () in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_56 () in
          _menhir_goto_suffixe _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | BOOLEEN _ | CARACTERE _ | ENTIER _ | FLOTTANT _ | IDENT _ | NOUVEAU | VIDE ->
          let _menhir_stack = MenhirCell1_PAROUV (_menhir_stack, _menhir_s) in
          let _v = _menhir_action_75 () in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState090 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_051 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ASSIGN | CROFER | OPDIV | OPET | OPMOD | OPMULT | OPOU | OPPT | PARFER | PTVIRG | VIRG ->
          let _v = _menhir_action_03 () in
          _menhir_goto_binaire _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | BOOLEEN _ | CARACTERE _ | ENTIER _ | FLOTTANT _ | IDENT _ | NOUVEAU | OPMOINS | OPNONEG | OPPLUS | PAROUV | VIDE ->
          let _v = _menhir_action_71 () in
          _menhir_goto_unaire _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_binaire : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_053 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ASSIGN | CROFER | OPDIV | OPET | OPMOD | OPMULT | OPOU | OPPT | PARFER | PTVIRG | VIRG ->
          let _v = _menhir_action_10 () in
          _menhir_goto_binaire _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | BOOLEEN _ | CARACTERE _ | ENTIER _ | FLOTTANT _ | IDENT _ | NOUVEAU | OPMOINS | OPNONEG | OPPLUS | PAROUV | VIDE ->
          let _v = _menhir_action_73 () in
          _menhir_goto_unaire _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_055 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ASSIGN | CROFER | OPDIV | OPET | OPMOD | OPMULT | OPOU | OPPT | PARFER | PTVIRG | VIRG ->
          let _v = _menhir_action_04 () in
          _menhir_goto_binaire _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | BOOLEEN _ | CARACTERE _ | ENTIER _ | FLOTTANT _ | IDENT _ | NOUVEAU | OPMOINS | OPNONEG | OPPLUS | PAROUV | VIDE ->
          let _v = _menhir_action_72 () in
          _menhir_goto_unaire _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_070_spec_090 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_PAROUV -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let _v = _menhir_action_43 () in
      _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState090 _tok
  
  and _menhir_run_091 : type  ttv_stack. ((ttv_stack, _menhir_box_fichier) _menhir_cell1_PAROUV as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VIRG ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState091
      | _ ->
          _eRR ()
  
  and _menhir_run_092 : type  ttv_stack. ((ttv_stack, _menhir_box_fichier) _menhir_cell1_expression as 'stack) -> _ -> _ -> ('stack, _menhir_box_fichier) _menhir_state -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_VIRG (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PAROUV ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState092
      | OPPT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_02 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState092 _tok
      | OPPLUS ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState092
      | OPOU ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState092 _tok
      | OPNONEG ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState092
      | OPMULT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_05 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState092 _tok
      | OPMOINS ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState092
      | OPMOD ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState092 _tok
      | OPET ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_09 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState092 _tok
      | OPDIV ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_06 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState092 _tok
      | ASSIGN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_01 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState092 _tok
      | VIRG ->
          let _ = _menhir_action_12 () in
          _menhir_run_070_spec_092 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | BOOLEEN _ | CARACTERE _ | ENTIER _ | FLOTTANT _ | IDENT _ | NOUVEAU | VIDE ->
          let _v = _menhir_action_75 () in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState092 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_070_spec_092 : type  ttv_stack. ((ttv_stack, _menhir_box_fichier) _menhir_cell1_expression, _menhir_box_fichier) _menhir_cell1_VIRG -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let _v = _menhir_action_43 () in
      _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState092 _tok
  
  and _menhir_run_093 : type  ttv_stack. (((ttv_stack, _menhir_box_fichier) _menhir_cell1_expression, _menhir_box_fichier) _menhir_cell1_VIRG as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VIRG ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState093
      | _ ->
          _eRR ()
  
  and _menhir_run_098 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_CROOUV (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PAROUV ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState098
      | OPPT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_02 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState098 _tok
      | OPPLUS ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState098
      | OPOU ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState098 _tok
      | OPNONEG ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState098
      | OPMULT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_05 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState098 _tok
      | OPMOINS ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState098
      | OPMOD ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState098 _tok
      | OPET ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_09 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState098 _tok
      | OPDIV ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_06 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState098 _tok
      | ASSIGN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_01 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState098 _tok
      | CROFER ->
          let _ = _menhir_action_12 () in
          _menhir_run_070_spec_098 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | BOOLEEN _ | CARACTERE _ | ENTIER _ | FLOTTANT _ | IDENT _ | NOUVEAU | VIDE ->
          let _v = _menhir_action_75 () in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState098 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_103 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_suffixe -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let MenhirCell1_suffixe (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _ = _menhir_action_58 () in
      _menhir_goto_suffixes _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
  
  and _menhir_goto_suffixes : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok ->
      match _menhir_s with
      | MenhirState102 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | MenhirState089 ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_101 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_unaires _menhir_cell0_bloc4 -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let MenhirCell0_bloc4 (_menhir_stack, _) = _menhir_stack in
      let _ = _menhir_action_20 () in
      _menhir_goto_bloc2 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
  
  and _menhir_run_070_spec_080 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_unaires _menhir_cell0_IDENT -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let _ = _menhir_action_43 () in
      _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
  
  and _menhir_run_081 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_unaires _menhir_cell0_IDENT -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      match (_tok : MenhirBasics.token) with
      | CROFER ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _ = _menhir_action_22 () in
          _menhir_goto_bloc3 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_bloc3 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_unaires _menhir_cell0_IDENT -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let MenhirCell0_IDENT (_menhir_stack, _) = _menhir_stack in
      let _ = _menhir_action_19 () in
      _menhir_goto_bloc2 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
  
  and _menhir_run_070_spec_062 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_unaires -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let _ = _menhir_action_43 () in
      _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
  
  and _menhir_run_068 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_unaires -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      match (_tok : MenhirBasics.token) with
      | PARFER ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_24 () in
          _menhir_goto_bloc4 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_bloc4 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_unaires -> _ -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _menhir_stack = MenhirCell0_bloc4 (_menhir_stack, _v) in
      match (_tok : MenhirBasics.token) with
      | PAROUV ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | CROOUV ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState089
      | CROFER | PARFER | PTVIRG | VIRG ->
          let _ = _menhir_action_59 () in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_116 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_RETOUR (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PAROUV ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | OPPT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_02 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState116 _tok
      | OPPLUS ->
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | OPOU ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_08 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState116 _tok
      | OPNONEG ->
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | OPMULT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_05 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState116 _tok
      | OPMOINS ->
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState116
      | OPMOD ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState116 _tok
      | OPET ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_09 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState116 _tok
      | OPDIV ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_06 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState116 _tok
      | ASSIGN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_01 () in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState116 _tok
      | PTVIRG ->
          let _ = _menhir_action_12 () in
          _menhir_run_070_spec_116 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | BOOLEEN _ | CARACTERE _ | ENTIER _ | FLOTTANT _ | IDENT _ | NOUVEAU | VIDE ->
          let _v = _menhir_action_75 () in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState116 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_122 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_instruction -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let MenhirCell1_instruction (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _ = _menhir_action_50 () in
      _menhir_goto_instructions _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
  
  and _menhir_goto_instructions : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      match _menhir_s with
      | MenhirState121 ->
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MenhirState034 ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_119 : type  ttv_stack. ((ttv_stack, _menhir_box_fichier) _menhir_cell1_ACCOUV, _menhir_box_fichier) _menhir_cell1_variables -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_variables (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_ACCOUV (_menhir_stack, _menhir_s) = _menhir_stack in
      let _v = _menhir_action_13 _2 in
      _menhir_goto_bloc _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_bloc : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState032 ->
          _menhir_run_130 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | MenhirState113 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | MenhirState111 ->
          _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState106 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_130 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_entete -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let MenhirCell1_entete (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _v = _menhir_action_45 () in
      let _menhir_stack = MenhirCell1_fonction (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VOID ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState030
      | TYPEIDENT _ ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_67 () in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState030 _tok
      | STRING ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_66 () in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState030 _tok
      | INT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_62 () in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState030 _tok
      | FLOAT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_63 () in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState030 _tok
      | CHAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_65 () in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState030 _tok
      | BOOL ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_64 () in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState030 _tok
      | FIN ->
          let _ = _menhir_action_54 () in
          _menhir_run_031 _menhir_stack
      | _ ->
          _eRR ()
  
  and _menhir_run_114 : type  ttv_stack. (((ttv_stack, _menhir_box_fichier) _menhir_cell1_SI, _menhir_box_fichier) _menhir_cell1_expression, _menhir_box_fichier) _menhir_cell1_bloc -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let _ = _menhir_action_38 () in
      _menhir_goto_elsebloc _menhir_stack _menhir_lexbuf _menhir_lexer _tok
  
  and _menhir_goto_elsebloc : type  ttv_stack. (((ttv_stack, _menhir_box_fichier) _menhir_cell1_SI, _menhir_box_fichier) _menhir_cell1_expression, _menhir_box_fichier) _menhir_cell1_bloc -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let MenhirCell1_bloc (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_SI (_menhir_stack, _menhir_s) = _menhir_stack in
      let _v = _menhir_action_47 () in
      _menhir_goto_instruction _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_112 : type  ttv_stack. (((ttv_stack, _menhir_box_fichier) _menhir_cell1_SI, _menhir_box_fichier) _menhir_cell1_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_bloc (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SINON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | ACCOUV ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
          | _ ->
              _eRR ())
      | ACCFER | ASSIGN | BOOLEEN _ | CARACTERE _ | ENTIER _ | FLOTTANT _ | IDENT _ | NOUVEAU | OPDIV | OPET | OPMOD | OPMOINS | OPMULT | OPNONEG | OPOU | OPPLUS | OPPT | PAROUV | PTVIRG | RETOUR | SI | TANTQUE | VIDE ->
          let _ = _menhir_action_39 () in
          _menhir_goto_elsebloc _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_107 : type  ttv_stack. ((ttv_stack, _menhir_box_fichier) _menhir_cell1_TANTQUE, _menhir_box_fichier) _menhir_cell1_expression -> _ -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let MenhirCell1_expression (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_TANTQUE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _v = _menhir_action_48 () in
      _menhir_goto_instruction _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_023 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_typeStruct (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | IDENT _v_0 ->
          let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v_0) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | PAROUV ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | TYPEIDENT _ ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v = _menhir_action_67 () in
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
              | STRING ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v = _menhir_action_66 () in
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
              | INT ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v = _menhir_action_62 () in
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
              | FLOAT ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v = _menhir_action_63 () in
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
              | CHAR ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v = _menhir_action_65 () in
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
              | BOOL ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v = _menhir_action_64 () in
                  _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
              | PARFER ->
                  let _ = _menhir_action_52 () in
                  _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_026 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_typeStruct _menhir_cell0_IDENT -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell0_IDENT (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_typeStruct (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _v = _menhir_action_40 () in
      _menhir_goto_entete _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_entete : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_entete (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ACCOUV ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState032
      | _ ->
          _eRR ()
  
  and _menhir_run_013 : type  ttv_stack. (((ttv_stack, _menhir_box_fichier) _menhir_cell1_typeStruct _menhir_cell0_IDENT, _menhir_box_fichier) _menhir_cell1_VIRG as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_typeStruct (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | IDENT _v_0 ->
          let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v_0) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VIRG ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState014
          | PARFER ->
              let _ = _menhir_action_60 () in
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_012 : type  ttv_stack. ((ttv_stack, _menhir_box_fichier) _menhir_cell1_typeStruct _menhir_cell0_IDENT as 'stack) -> _ -> _ -> ('stack, _menhir_box_fichier) _menhir_state -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_VIRG (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYPEIDENT _ ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_67 () in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState012 _tok
      | STRING ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_66 () in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState012 _tok
      | INT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_62 () in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState012 _tok
      | FLOAT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_63 () in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState012 _tok
      | CHAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_65 () in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState012 _tok
      | BOOL ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_64 () in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState012 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_015 : type  ttv_stack. (((ttv_stack, _menhir_box_fichier) _menhir_cell1_typeStruct _menhir_cell0_IDENT, _menhir_box_fichier) _menhir_cell1_VIRG, _menhir_box_fichier) _menhir_cell1_typeStruct _menhir_cell0_IDENT -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let MenhirCell0_IDENT (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_typeStruct (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_VIRG (_menhir_stack, _menhir_s) = _menhir_stack in
      let _ = _menhir_action_61 () in
      _menhir_goto_suiteParsFormels _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
  
  and _menhir_goto_suiteParsFormels : type  ttv_stack. ((ttv_stack, _menhir_box_fichier) _menhir_cell1_typeStruct _menhir_cell0_IDENT as 'stack) -> _ -> _ -> ('stack, _menhir_box_fichier) _menhir_state -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      match _menhir_s with
      | MenhirState011 ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MenhirState014 ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_020 : type  ttv_stack. (ttv_stack _menhir_cell0_IDENT, _menhir_box_fichier) _menhir_cell1_typeStruct _menhir_cell0_IDENT -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let MenhirCell0_IDENT (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_typeStruct (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _ = _menhir_action_53 () in
      _menhir_goto_parsFormels _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
  
  and _menhir_goto_parsFormels : type  ttv_stack. (ttv_stack _menhir_cell0_IDENT as 'stack) -> _ -> _ -> ('stack, _menhir_box_fichier) _menhir_state -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      match _menhir_s with
      | MenhirState025 ->
          _menhir_run_026 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MenhirState003 ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_021 : type  ttv_stack. (ttv_stack, _menhir_box_fichier) _menhir_cell1_VOID _menhir_cell0_IDENT -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell0_IDENT (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_VOID (_menhir_stack, _menhir_s) = _menhir_stack in
      let _v = _menhir_action_41 () in
      _menhir_goto_entete _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_010 : type  ttv_stack. (ttv_stack _menhir_cell0_IDENT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_fichier) _menhir_state -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_typeStruct (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | IDENT _v_0 ->
          let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v_0) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VIRG ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState011
          | PARFER ->
              let _ = _menhir_action_60 () in
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  let rec _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_fichier =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VOID ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | TYPEIDENT _ ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_67 () in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState000 _tok
      | STRING ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_66 () in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState000 _tok
      | INT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_62 () in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState000 _tok
      | FLOAT ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_63 () in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState000 _tok
      | CHAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_65 () in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState000 _tok
      | BOOL ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_64 () in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState000 _tok
      | FIN ->
          let _ = _menhir_action_54 () in
          _menhir_run_028 _menhir_stack
      | _ ->
          _eRR ()
  
end

let fichier =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_fichier v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

# 145 "parserJava.mly"
  

# 2640 "parserJava.ml"
