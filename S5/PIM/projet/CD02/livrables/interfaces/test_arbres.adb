with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Integer_Text_IO;           use Ada.Integer_Text_IO;
with Arbres;                    use Arbres;

procedure test_arbres is
	
	function ArbreComplet(n : Integer) return T_Arbre is 
		Cellule : T_Arbre;
	begin 
		
		InitCellule(Cellule);
		if n = 0 then 
			Modifier_Valeur(Cellule,'F');
			return Cellule;
		else
			Modifier_FG(Cellule, ArbreComplet(n-1));
			Modifier_FD(Cellule, ArbreComplet(n-1));
			Modifier_Valeur(Cellule,'*');
			return Cellule;
		end if;
	end ArbreComplet;
	
	function Peigne(n : Integer) return T_Arbre is 
		Cellule : T_Arbre;
	begin 
		
		InitCellule(Cellule);
		if n = 0 then 
			Modifier_Valeur(Cellule,'F');
			return Cellule;
		else
			Modifier_FG(Cellule, Peigne(n-1));
			Modifier_Valeur(Cellule,'*');
			return Cellule;
		end if;
	end Peigne;


	function TestList(n : Integer) return T_Arbre is	
		Cellule : T_Arbre;
	begin
		InitCellule(Cellule);
		Modifier_Frequence(Cellule, n) ;
		Modifier_Valeur(Cellule, Character'Val(n+80));
		if n>1 then 
			Modifier_Suivant(Cellule,TestList(n-1));
		end if;
		return Cellule;
	end TestList;

	List : T_Arbre;
	Arbre : T_Arbre;

	function Test return T_Arbre is
		Arbre : T_Arbre;
		Feuille1 : T_Arbre;
		Feuille2 : T_Arbre;
		Feuille3 : T_Arbre;
	begin
		Feuille1 := CreerFeuille(1,'a');
		Feuille2 := CreerFeuille(1,'b');
		Feuille3 := CreerFeuille(1,'c');
		
		Initialiser(Feuille2);
		Arbre := Enregistrer(Feuille2,Feuille3);
		
		Arbre := Enregistrer(Feuille1, Arbre);
		
		return Arbre;
	end Test;

	function CheckTree (Arbre : in T_Arbre) return Boolean is
	begin
		if Est_Vide(Arbre) then return True;
		elsif not ( Est_Vide(FG(Arbre)) xor Est_Vide(FD(Arbre))  ) then 
				return ( CheckTree(FG(Arbre)) and CheckTree(FD(Arbre)) );
		else return False;
		end if;
	end CheckTree;

begin 
	
	Afficher(Test);
	Put(Boolean'image(CheckTree(Test)));

end test_arbres;
