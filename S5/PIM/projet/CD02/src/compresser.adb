with Arbres;                    use Arbres;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Command_line;          use Ada.Command_line;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings; use Ada.Strings;

procedure Compresser is
	bavard : Boolean := False; 
	Arbre_Huffman : T_Arbre;

    function Construire_Arbre (nom_fichier : in String) return T_Arbre is
		Huff: T_Arbre;
		New_Arbre : T_Arbre;
		--Fichier : Ada.Text_IO.File_Type ; 
  
		type T_Octet is mod 2 ** 8;	-- sur 8 bits
		for T_Octet'Size use 8;
		Fichier : Ada.Streams.Stream_IO.File_Type;		
		S         : Stream_Access;

		Lettre : Character;
		FilsG : T_Arbre;
		FilsD : T_Arbre;
		Suite : T_Arbre;
		Freq : Integer;
		
		Octet : T_Octet;
    begin 
        
		-- Construction de la liste chaÃ®nÃ©e Ã  partir du fichier texte
         Initialiser(Huff);
		Open(Fichier, In_File, nom_fichier);
		S := Stream ( Fichier );
		while not End_Of_File (Fichier) loop
			T_OCtet'Read (S, Octet);
			--Get(Fichier,Lettre);
			Incr_Freq(Huff,Character'Val(Octet));
		end loop;
		Inserer(Huff, CreerFeuille(0,Character'Val(0)) );
		Close(Fichier);
		-- Tri de la liste chaÃ®nÃ©e
  
		Huff := Tri(Huff);
		-- Construction de l'arbre
		while not Est_Vide(Suivant(Huff)) loop 
			-- Selection des deux arbres avec la plus petite racine (les deux premiers dans la liste chaÃ®nÃ©e)
			-- Fusion des deux arbres en un seul, avec comme racine la somme de leur deux racines)
			-- Insertion dans la liste chaÃ®nÃ©e de l'arbre obtenu
              
			
			
		
			FilsG :=  Huff;
            FilsD := Suivant(Huff);
			Suite := Suivant(Suivant(Huff));


			Del_Suivant(FilsG);
			Del_Suivant(FilsD);

			New_Arbre := Enregistrer(FilsG,FilsD);
			Modifier_Valeur(New_Arbre, '^');
			Freq := Frequence(FilsG)+Frequence(FilsD);
			Modifier_Frequence(New_Arbre, Freq);


			

			Inserer(Suite, New_Arbre);
			Huff := Suite;
			

		end loop;


		return Huff;
        
	end Construire_Arbre;

	function Bin_To_Dec(m : in  Integer) return Integer is
		pow : Integer := 1;
		s : Integer := 0;
		bit : Integer;
		n : Integer := m;
	begin
		while n >= 1 loop
			bit := n mod 10;
			s := s + bit*pow;
			n := (n - bit)/10;
			pow := 2*pow;
		end loop;
		return s;
	end Bin_To_Dec;

	
	
	procedure Construire_Fichier (Huff : in T_Arbre; nom_fichier : in String) is
		type T_Octet is mod 2 ** 8;	-- sur 8 bits
		for T_Octet'Size use 8;
		
		--File : Ada.Text_IO.File_Type;
  
		File_Name : String :=  nom_fichier & ".hff";
		File_Compressed      : Ada.Streams.Stream_IO.File_Type;	
		File : Ada.Streams.Stream_IO.File_Type;
		S         : Stream_Access;
		S2         : Stream_Access;
		Lettre : Character;
		Binaire : Unbounded_String;
		index : Integer := 1;
		Octet : T_Octet;
		ListeInfixe: T_Arbre;
		Codages : T_Arbre;
		PosFin : Integer;
		ParcoursOrdreInfixe : Unbounded_String;

		procedure ajout (Arbre : in T_Arbre) is
		begin
			if estFeuille(Arbre) then Ajouter(ListeInfixe, CreerFeuille(0, Valeur(Arbre)));
			end if;									
		end;

		procedure CreationListeInfixe is new Parcours_Infixe (Traiter => ajout);
		
		procedure EcrireOctet(Arbre : in T_Arbre) is 
			Octet : T_Octet;
		begin
			Octet :=  T_Octet ( Character'Pos (Valeur(Arbre)) );
			T_Octet'Write(S,Octet);
		end;

		procedure EcrireCodage is new Pour_Chaque (Traiter => EcrireOctet);
		

		-- Ecrit un unbounded string dans le fichier sous forme de paquet de 8 bits + un paquet éventuellement incomplet
		procedure EcrireUnboundedString (Binaire : in Unbounded_String) is
			index : Integer := 1;
		begin
			while (index+7 <= Length(Binaire)) loop -- regroupement par paquet de 8
			
			
				--Put_Line( Integer'Image (  Bin_To_Dec( Integer'Value ( Slice(Binaire, index, index+7) ) ) ));
			
				Octet := T_Octet(  Bin_To_Dec( Integer'Value ( Slice(Binaire, index, index+7) ) )  );
				T_Octet'Write(S,Octet);
				index := index + 8;
			end loop;
			if index <= Length(Binaire) then -- Cas de l'octet incomplet
				Octet := T_Octet(  Bin_To_Dec( Integer'Value ( Slice(Binaire, index, Length(Binaire)) ) )  )*(2**(8-(Length(Binaire)-index+1))); -- la multiplication permet la completion de l'octet par des 0 à droite
				

				T_Octet'Write(S,Octet);
				index := index + 8;
			end if;
		end EcrireUnboundedString;
		
		
	Bit : T_Octet;
	Aecrire : Unbounded_String;
	N : Integer;
	begin
		Open(File, In_File, nom_fichier);
		Create (File_Compressed, Out_File, File_Name);
		S := Stream (File_Compressed);
		S2 := Stream (File);

		-- Ajout du codage
        CreationListeInfixe(Huff);
        
		PosFin := Indice(ListeInfixe,Character'Val(0));
		

        T_Octet'Write(S,T_Octet(PosFin));
        
		New_Line;
		ListeInfixe := Supprimer(ListeInfixe, Character'Val(0));
		DupliquerDernier(ListeInfixe);
		EcrireCodage(ListeInfixe);
		Codages := GenererCodages(Huff);
		
		if bavard then Afficher_Codages(Codages);
		end if;


		-- Ajout de l'encodage du parcours infixe
		ParcoursOrdreInfixe := CreerCodageInfixe (Huff);
		EcrireUnboundedString(ParcoursOrdreInfixe);
		-- Ajout du texte

		--Aecrire := Code(Codages, Character'Val(0));
		--while not End_Of_File ( File) loop
			
		--	while Length(Aecrire) < 8 loop
		--		T_OCtet'Read (S2, Octet);
		--		Aecrire := Aecrire & Code(Codages, Character'Val(Octet));
		--	end loop;
			
		--	N := Length(Aecrire);
		--	T_Octet'Write(S, T_Octet(Bin_To_Dec(Integer'Value ( Slice(Aecrire, 1, 8) ) )));
		--	if N > 8 then 
		--		Aecrire := To_Unbounded_String( (Aecrire, 9, N) );
		--	end if;
			
		--end loop;

		while not End_Of_File (File) loop
			T_OCtet'Read (S2, Octet);
			 --Get(Fichier,Lettre);
			 Binaire := Binaire & Code(Codages,Character'Val(Octet));
		end loop;

		Binaire := Binaire & Code(Codages,Character'Val(0)); -- ajout de /$
		EcrireUnboundedString(Binaire);
		Close (File);
		Close (File_Compressed);
	end Construire_Fichier;
	  
	function CheckTree (Arbre : in T_Arbre) return Boolean is
	begin
		if Est_Vide(Arbre) then return True;
		elsif not ( Est_Vide(FG(Arbre)) xor Est_Vide(FD(Arbre))  ) then 
				return ( CheckTree(FG(Arbre)) and CheckTree(FD(Arbre)) );
		else return False;
		end if;
    end CheckTree;
    
  
begin
	if Argument_Count = 0 then Put_Line("USAGE : compresser (-b ou --bavard)  fichier1 fichier2 ...");
	elsif Argument_Count >= 2 and (Argument(1) = "-b" or Argument(1) = "--bavard") then		
		bavard := True;
		for i in 2 .. Argument_Count loop	
			New_Line(2);
			Put_Line("### Compression de " & Argument(i) & " ###");
			Put_Line("--/ Construction de l'arbre /--");
			Arbre_Huffman := Construire_Arbre(Argument(i));
			New_Line;
			Afficher_arbre(Arbre_Huffman);
			New_Line;
			New_Line;
			Put_Line("--/ Construction du fichier /--");
			Construire_Fichier( Arbre_Huffman, Argument(i) );
			Put_Line("### Fin de la compression de " & Argument(i) & " ###");
			New_Line(2);
		end loop;
	else 
		for i in 1 .. Argument_Count loop	
			New_Line(2);
			Put_Line("### Compression de " & Argument(i) & " ###");			
			Arbre_Huffman := Construire_Arbre(Argument(i));			
			Construire_Fichier( Arbre_Huffman, Argument(i) );
			Put_Line("### Fin de la compression de " & Argument(i) & " ###");
			New_Line(2);
		end loop;
	end if;
end Compresser;
