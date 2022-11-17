package body TH is

	procedure Initialiser(TH: out T_TH) is
	begin

        for i in 1..CAPACITE loop
                Initialiser(TH(i));
        end loop;

	end Initialiser;


	function Est_Vide (TH : T_TH) return Boolean is
        res : Boolean;
        i : Integer;
    begin

        i := 1;
        loop
            res := Est_Vide (TH(i));
            i := i+1;
            exit when i > CAPACITE or not res;
        end loop;
        return res;

     end Est_Vide;


	function Taille (TH : in T_TH) return Integer is
        taille_th : Integer;
     begin

        taille_th := 0;
        for i in 1..CAPACITE loop
            taille_th := taille_th + Taille(TH(i));
        end loop;
        return taille_th;

	end Taille;

    function Hachage (Cle : Unbounded_String) return Integer is
    begin
        if Length (Cle) <= 11 then
            return Length (Cle);
        else
            return Length (Cle) mod 11 + 1;
        end if;
    end Hachage;

	procedure Enregistrer (TH : in out T_TH ; Cle : in T_Cle ; Donnee : in T_Donnee) is
        k : Integer;
    begin

         k := Hachage(Cle);
         Enregistrer (TH(k), Cle, Donnee);

	end Enregistrer;


	function Cle_Presente (TH: in T_TH ; Cle : in T_Cle) return Boolean is
        k : Integer;
    begin

        k := Hachage (Cle);
        return Cle_Presente (TH(k), Cle);

     end;


	function La_Donnee (TH : in T_TH ; Cle : in T_Cle) return T_Donnee is
        k : Integer;
    begin

        k := Fct_H (Cle);
        return La_Donnee (TH(k), Cle);

     end La_Donnee;


	procedure Supprimer (TH : in out T_TH ; Cle : in T_Cle) is
	        k : Integer;
    begin

        k := Fct_H (Cle);
        Supprimer (TH(k), Cle);

	end Supprimer;


	procedure Vider (TH : in out T_TH) is
	begin

        for i in 1..CAPACITE loop
            Vider (TH(i));
        end loop;

	end Vider;


	procedure Pour_Chaque (TH : in T_TH) is
        procedure Pour_Chaque_LCA is new Liste.Pour_Chaque (Traiter);
    begin

        for i in 1..CAPACITE loop
            Pour_Chaque_LCA (TH(i));
        end loop;
	end Pour_Chaque;


end TH;
