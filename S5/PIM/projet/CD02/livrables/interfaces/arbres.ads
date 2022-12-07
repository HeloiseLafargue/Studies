with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
-- Spécification du module Arbres.

package Arbres is

	type T_Arbre is private;


	-- Initilaiser un arbre.  L'arbre est vide.
	procedure Initialiser (Huff : out T_Arbre) with
		Post => Est_Vide (Huff);

	-- Est-ce que l'arbre est vide ?
	function Est_Vide (Huff : in T_Arbre) return Boolean;


	-- Renvoie la racine d'un arbre
	function Frequence (Arbre : in T_Arbre) return Integer with
		Pre => not Est_Vide (Arbre);
		
	-- Renvoie la feuille d'un arbre
	function Valeur (Arbre : in T_Arbre) return Character with
		Pre => not Est_Vide (Arbre);

	-- Renvoie l'arbre suivant
	function Suivant (Arbre : in T_Arbre) return T_Arbre with
		Pre => not Est_Vide (Arbre);
	
	procedure Modifier_Frequence (Arbre : in out T_Arbre; Frequence : in Integer) with
	  Pre => not Est_Vide (Arbre);
	
	procedure Modifier_Valeur (Arbre : in out T_Arbre; Valeur : in Character) with
	  Pre => not Est_Vide (Arbre);

	procedure Modifier_FG (Arbre : in out T_Arbre; Fils : in T_Arbre) with
	  Pre => not Est_Vide (Arbre);

	function FG(Arbre : in T_Arbre) return T_Arbre with 
	  Pre => not Est_Vide (Arbre);
					 
	procedure Modifier_FD (Arbre : in out T_Arbre; Fils : in T_Arbre) with
	  Pre => not Est_Vide (Arbre);

	function FD(Arbre : in T_Arbre) return T_Arbre with 
	  Pre => not Est_Vide (Arbre);
			
	procedure Modifier_Suivant (Arbre : in out T_Arbre; Suivant : in T_Arbre) with
	  Pre => not Est_Vide (Arbre);					

	procedure Incr_Freq (Huff : in out T_Arbre; Caractere : in Character);
    
    -- Ajoute un arbre à la fin de la liste chaînée
    procedure Ajouter (Huff : in out T_Arbre; Arbre : in T_Arbre);
    
    -- Insère un arbre au bon endroit dans la liste chaînée (triée dans l'ordre croissant des fréquences)
    procedure Inserer (Huff : in out T_Arbre; Arbre : in T_Arbre);

    -- Tri par insertion de la liste chaînée
    function Tri (Huff : in T_Arbre) return T_Arbre;

	
	function estFeuille(Arbre : in T_Arbre) return boolean with
	  Pre => not Est_Vide(Arbre);

	function GenererCodages(Huff : in T_Arbre) return T_Arbre with
	  Pre => not Est_Vide(Huff);

	function Code (Codages : in T_Arbre; Caractere : in Character) return Unbounded_String with
	  Pre => not Est_Vide(Codages);
	
	

	procedure InitCellule (Cellule: out T_Arbre) ;

	procedure Del_Suivant (Huff : in T_Arbre) ;

	function CreerFeuille (Frequence : in Integer; Valeur : in Character) return T_Arbre;
	function Enregistrer (Arb1 : in T_Arbre; Arb2 : in T_Arbre) return T_Arbre;
	procedure Afficher_Codages (Codages : in T_Arbre);

	generic
		with procedure Traiter (Arbre : in T_Arbre);
	procedure Parcours_Infixe (Arbre : in T_Arbre);

	generic
		with procedure Traiter (Liste : in T_Arbre);
	procedure Pour_Chaque (Liste : in T_Arbre);

	function Indice (Liste : in T_Arbre; Valeur : in Character) return Integer;
	procedure AjouterDeb(Huff : in out T_Arbre; Arbre : in T_Arbre);

	function Supprimer(Liste : in out T_Arbre; Valeur : in Character) return T_Arbre;
	
	procedure DupliquerDernier (Liste : in T_Arbre);

	function CreerCodageInfixe (Huff : in T_Arbre) return Unbounded_String;
	procedure Inserer_A_Indice (Liste : in out T_Arbre; Arbre : in T_Arbre; i : in Integer);
    function Codage (Arbre : in T_Arbre) return Unbounded_String ;
    
    procedure Afficher_arbre(Huff : in T_Arbre);

private

	type T_Noeud;

	type T_Arbre is access T_Noeud;

	type T_Noeud is
		record
			Frequence: Integer;
			Valeur : Character;
			Suivant : T_Arbre;
 			Fils_Gauche: T_Arbre;
			Fils_Droit: T_Arbre;
			Codage : Unbounded_String;
		end record;

end Arbres;
