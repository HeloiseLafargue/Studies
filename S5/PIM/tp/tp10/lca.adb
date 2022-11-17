with SDA_Exceptions;         use SDA_Exceptions;
with Ada.Unchecked_Deallocation;

package body LCA is

	procedure Free is
		new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_LCA);


	procedure Initialiser(Sda: out T_LCA) is
	begin
		Sda := Null ;
	end Initialiser;


	function Est_Vide (Sda : T_LCA) return Boolean is
	begin
		return Sda = Null;
	end;


	function Taille (Sda : in T_LCA) return Integer is
    begin
        if Sda = Null then
             return (0);
        else
             return ( Taille(Sda.all.Suivant) + 1);
        end if;
	end Taille;


	procedure Enregistrer (Sda : in out T_LCA ; Cle : in T_Cle ; Donnee : in T_Donnee) is

    begin

         if Sda.all.Cle = Cle then

            Sda.all.Donnee := Donnee;
            Sda.all.Suivant := Sda;

         elsif Sda = Null then

            Sda := new T_Cellule'(Cle, Donnee, null);

         else

            Enregistrer(Sda.all.Suivant, Cle, Donnee);

         end if;

	end Enregistrer;


    function Cle_Presente (Sda : in T_LCA ; Cle : in T_Cle) return Boolean is
	begin
            if Sda= null then
                raise Cle_Absente_Exception;

            elsif Sda.all.Cle = Cle then
                 return True;

            else
                 return Cle_Presente(Sda.all.Suivant, Cle);

            end if;

		exception
		when Cle_Absente_Exception =>
			return False;
     end;


	function La_Donnee (Sda : in T_LCA ; Cle : in T_Cle) return T_Donnee is
	begin
            if Sda = Null then
                  raise Cle_Absente_Exception;

            elsif Sda.all.Cle = Cle then
                  return Sda.all.Donnee;

            else
                  return La_Donnee(Sda.all.Suivant, Cle);

            end if;

     end La_Donnee;


	procedure Supprimer (Sda : in out T_LCA ; Cle : in T_Cle) is
        Sda_2 : T_LCA;
    begin
            if Sda = Null then
                raise Cle_Absente_Exception;

            elsif Sda.all.Cle = Cle then
                Sda_2 := Null;
                Sda_2 := Sda;
                Sda := Sda.all.Suivant;
                Free (Sda_2);

            else
                Supprimer(Sda.all.Suivant, Cle);

            end if;
	end Supprimer;


	procedure Vider (Sda : in out T_LCA) is
	begin
        if Sda /= Null then
            Vider( Sda.all.Suivant);
            Free (Sda);
        end if;
	end Vider;


	procedure Pour_Chaque (Sda : in T_LCA) is
	begin
        if Sda /= Null then
            Pour_Chaque (Sda.Suivant);
            Traiter (Sda.all.Cle, Sda.all.Donnee);
        end if;
	end Pour_Chaque;


end LCA;
