with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with SDA_Exceptions; 		use SDA_Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with th;


procedure th_sujet is

    -- Fonction de hachage pour initialiser TH
    function Hachage (Cle : Unbounded_String) return Integer is
    begin
        if Length (Cle) <= 11 then
            return Length (Cle);
        else
            return Length (Cle) mod 11 + 1;
        end if;
    end Hachage;

    package TH_String_Integer is
		new TH (T_Cle => Unbounded_String, T_Donnee => Integer, CAPACITE => 11, Fct_H => Hachage);
	use TH_String_Integer;



	-- Retourner une chaîne avec des guillemets autour de S
	function Avec_Guillemets (S: Unbounded_String) return String is
	begin
		return '"' & To_String (S) & '"';
	end;


	function "&" (Left: String; Right: Unbounded_String) return String is
	begin
		return Left & Avec_Guillemets (Right);
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

	-- Afficher le TH.
	procedure Afficher is
		new Pour_Chaque (Afficher);

    tab_h : T_TH ;
	Nb_Cles : constant Integer := 7;
    Cles : constant array (1..Nb_Cles) of Unbounded_String := (+"un", +"deux", +"trois",
                          +"quatre", +"cinq", +"quatre-vingt-dix-neuf", +"vingt-et-un");
	Donnees : constant array (1..Nb_Cles) of Integer := (1, 2, 3, 4, 5, 99, 21);


 begin

    Initialiser(tab_h);

    for i in 1..Nb_Cles loop
        Enregistrer (tab_h, Cles(i), Donnees(i));
    end loop;

    Afficher(tab_h);

end th_sujet;
