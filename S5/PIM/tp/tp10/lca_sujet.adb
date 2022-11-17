with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with lca;

procedure lca_sujet is

    	package LCA_String_Integer is
		new LCA (T_Cle => Unbounded_String, T_Donnee => Integer);
	use LCA_String_Integer;


    	-- Retourner une chaîne avec des guillemets autour de S
	function Avec_Guillemets (S: Unbounded_String) return String is
	begin
		return '"' & To_String (S) & '"';
	end;


	-- Convertir une String en Unbounded_String.
	function "+" (Item : in String) return Unbounded_String
		renames To_Unbounded_String;


	-- Afficher une Unbounded_String et un entier.
	procedure Afficher (S : in Unbounded_String; N: in Integer) is
	begin
		Put (Avec_Guillemets (S));
		Put (" : ");
		Put (N, 1);
		New_Line;
	end Afficher;

	-- Afficher la Sda.
	procedure Afficher is
		new Pour_Chaque (Afficher);


	Nb_Cles : constant Integer := 2;
	Cles : constant array (1..Nb_Cles) of Unbounded_String := (+"un", +"deux");
	Donnees : constant array (1..Nb_Cles) of Integer := (1, 2);

    Sda : T_LCA;


begin

    Initialiser(Sda);

    Enregistrer(Sda, Cles(1), Donnees(1));
    Enregistrer(Sda, Cles(2), Donnees(2));

    Afficher(Sda);

end lca_sujet;
