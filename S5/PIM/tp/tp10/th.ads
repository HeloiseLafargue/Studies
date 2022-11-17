with LCA ;

-- Définition de structures de données associatives sous forme d'un tableau
-- de hachage (TH).
generic
	type T_Cle is private;
     type T_Donnee is private;

     CAPACITE : Integer;
    with function Fct_H(Cle : in T_Cle) return Integer;


package TH is

	type T_TH is limited private;

	-- Initialiser un TH.  Le TH est vide.
	procedure Initialiser(TH: out T_TH) with
		Post => Est_Vide (TH);


	-- Est-ce qu'un TH est vide ?
	function Est_Vide (TH : T_TH) return Boolean;


	-- Obtenir le nombre d'éléments d'un TH.
	function Taille (TH : in T_TH) return Integer with
		Post => Taille'Result >= 0
			and (Taille'Result = 0) = Est_Vide (TH);


	-- Enregistrer une Donnée associée à une Clé dans un TH.
	-- Si la clé est déjà présente dans le TH, sa donnée est changée.
	procedure Enregistrer (TH : in out T_TH ; Cle : in T_Cle; Donnee : in T_Donnee) with
		Post => Cle_Presente (TH, Cle) and (La_Donnee (TH, Cle) = Donnee)  -- donnée insérée
				and (not (Cle_Presente (TH, Cle)'Old) or Taille (TH) = Taille (TH)'Old)
				and (Cle_Presente (TH, Cle)'Old or Taille (TH) = Taille (TH)'Old + 1);

	-- Supprimer la Donnée associée à une Clé dans un TH.
	-- Exception : Cle_Absente_Exception si Clé n'est pas utilisée dans le TH
	procedure Supprimer (TH : in out T_TH ; Cle : in T_Cle) with
		Post =>  Taille (TH) = Taille (TH)'Old - 1 -- un élément de moins
			and not Cle_Presente (TH, Cle);         -- la clé a été supprimée


	-- Savoir si une Clé est présente dans un TH.
	function Cle_Presente (TH : in T_TH ; Cle : in T_Cle) return Boolean;


	-- Obtenir la donnée associée à une Cle dans le TH.
	-- Exception : Cle_Absente_Exception si Clé n'est pas utilisée dans le TH
	function La_Donnee (TH : in T_TH ; Cle : in T_Cle) return T_Donnee;


	-- Supprimer tous les éléments d'un TH.
	procedure Vider (TH : in out T_TH) with
		Post => Est_Vide (TH);


	-- Appliquer un traitement (Traiter) pour chaque couple d'un TH.
	generic
		with procedure Traiter (Cle : in T_Cle; Donnee: in T_Donnee);
	procedure Pour_Chaque (TH : in T_TH);

private

    package Liste is new LCA(T_Cle, T_Donnee);
    use Liste;

    type T_TH is array (1..CAPACITE) of T_LCA;

end TH;
