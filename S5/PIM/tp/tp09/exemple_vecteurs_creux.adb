with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Vecteurs_Creux;    use Vecteurs_Creux;

-- Exemple d'utilisation des vecteurs creux.
procedure Exemple_Vecteurs_Creux is

	V : T_Vecteur_Creux;
	Epsilon: constant Float := 1.0e-5;
begin
	Put_Line ("Début du scénario");

    -- TODO : \u00e0 compl\u00e9ter
    Initialiser(V);
    Afficher(V);

    if Est_Nul(V) then
        Put_Line("V est nul");
    end if;
    Modifier(V,2,18);
    Detruire(V);

	Put_Line ("Fin du scénario");
end Exemple_Vecteurs_Creux;
