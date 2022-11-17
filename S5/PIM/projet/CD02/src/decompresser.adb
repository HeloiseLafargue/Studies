with Arbres;                    use Arbres;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Command_line;          use Ada.Command_line;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings; use Ada.Strings;

procedure Decompresser is

	File      : Ada.Streams.Stream_IO.File_Type;	-- car il y a aussi Ada.Text_IO.File_Type
	S         : Stream_Access;

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

	function Dec_To_Bin(m : in  Integer) return Integer is
		pow : Integer := 1;
		s : Integer := 0;
		bit : Integer;
		n : Integer := m;
	begin
		while n >= 1 loop
			bit := n mod 2;
			s := s + bit*pow;
			n := (n - bit)/2;
			pow := 10*pow;
		end loop;
		return s;
	end Dec_To_Bin;

	function Char_To_String_Of_Single_Char ( C : in Character) return String is
		Chaine : string(1..1);
	begin
		Chaine(1) := C;
		return Chaine;
	end Char_To_String_Of_Single_Char;

    
	
	function Construire_Arbre(nom_fichier : in String) return T_Arbre is
		type T_Octet is mod 2 ** 8;	-- sur 8 bits
		for T_Octet'Size use 8;
		Huff : T_Arbre;
		

		OctetCour     : T_Octet;
		OctetSuivant : T_Octet;
		ListeCaracteres : T_Arbre;
		Encodage : Unbounded_String := To_Unbounded_String("");
		N_Chars : Integer := 0; -- Nombre de symboles différents dans le texte;
		i : Integer := 1;
		Bit : T_Octet;
		PosFin : Integer;
		bit_str : Unbounded_String;
		function ReconstArbre (Parcours : in Unbounded_String) return T_Arbre is
			index : Integer := 0;
			NIL : T_Arbre;
			Huff : T_Arbre;
			procedure aux (Str : in Unbounded_String; Arbre : in out T_Arbre) is
				FilsG : T_Arbre;
				FilsD : T_Arbre;
			begin
				index := index + 1;
				if  index <= Length(Parcours)  then
					
					if Element(Parcours, index) = '1' then 
						Modifier_FG(Arbre,NIL);
						Modifier_FD(Arbre,NIL);
						Modifier_Valeur(Arbre, '*');
					else 
						Modifier_FG(Arbre,CreerFeuille(0,'^'));
						FilsG := FG(Arbre);
						aux(Str,FilsG);
						Modifier_FD(Arbre,CreerFeuille(0,'^'));
						FilsD := FD(Arbre);
						aux(Str,FilsD);
					end if;
				end if;
			end aux;
		begin
			Initialiser(NIL);
			InitCellule(Huff);
			aux(Parcours, Huff);
			return Huff;
		end ReconstArbre;


		procedure RemplirHuff(Huff: in out T_Arbre; Liste : in T_Arbre) is
			Tmp : T_Arbre := Liste;
			
			procedure aux (Arbre : in out T_Arbre)  is
				FilsG : T_Arbre := FG(Arbre);
				FilsD : T_Arbre := FD(Arbre);
			begin
				if estFeuille(Arbre) then
					Modifier_Valeur(Arbre, Valeur(Tmp));
					Tmp := Suivant(Tmp);
				else
					aux(FilsG);
					aux(FilsD);
				end if;
			end aux; 
		begin
			aux(Huff);
		end RemplirHuff;
	begin
		--Open(File, In_File, nom_fichier);
		
		--  Recuperation de la liste des caractères 
		S := Stream(File);
		OctetSuivant := T_Octet'Input(S); -- ignore le premier octet. il semblerait qu'un caractère vide soit écrit en début de fichier à la compression...
		PosFin := Integer(OctetSuivant); -- position du caractere de fin;
		
		OctetSuivant := T_Octet'Input(S);
		loop
			
			OctetCour := OctetSuivant;
			--Put("Octet = " & T_Octet'Image(Octet));
   
			Ajouter(ListeCaracteres, CreerFeuille(0, Character'Val(OctetCour)));
			
			N_Chars := N_Chars + 1;
			
			OctetSuivant := T_Octet'Input(S);
			exit when End_Of_File(File) or (OctetCour = OctetSuivant); 
		end loop;
		
		Inserer_A_Indice(ListeCaracteres,CreerFeuille(0, Character'Val(0)), PosFin); -- insertion du caractere de fin, de frequence 0
		
		-- Recuperation de l'encodage
		while (not End_Of_File(File)) and  i <= N_Chars loop
			OctetCour := T_Octet'Input(S);
			for N in 1..8 loop
				Bit := OctetCour / 128;
				OctetCour := OctetCour * 2;
				bit_str := To_Unbounded_String(Integer'Image ( Integer(Bit) ));
				Encodage := Encodage & To_Unbounded_String( Char_To_String_Of_Single_Char(Element(bit_str,2)) );
				if Integer(Bit) = 1 then 
					
					i := i + 1;
				end if;
			end loop;
		end loop;
		-- Purge des zeros surnuméraires
         i := Length(Encodage); 
		while i > 0 and Element(Encodage,i) = '0' loop
			i := i - 1;
		end loop;
		Encodage := To_Unbounded_String (Slice (Encodage,1,i));
	
		
		
		--   Fermer le fichier
		--Close (File);
		Huff := ReconstArbre(Encodage);
		RemplirHuff(Huff, ListeCaracteres);
		return Huff;
	end Construire_Arbre;

	

	procedure Construire_Fichier(nom_fichier : String; Codages : in T_Arbre) is
		--File_Name : String := "lol.txt";
		File_Name : String :=  Slice ( To_Unbounded_String( nom_fichier ) , 1 , Length( To_Unbounded_String(nom_fichier) ) - 4 );
		File2      : Ada.Streams.Stream_IO.File_Type;
		S2        : Stream_Access;
		type T_Octet is mod 2 ** 8;	-- sur 8 bits
		for T_Octet'Size use 8;
		Octet : T_Octet;
		CodeCour : Unbounded_String := To_Unbounded_String("");
		Bit : T_Octet;
		Char : String(1..1);
		Valide : Boolean := False;

		function Octet_to_String (Oct : T_Octet) return Unbounded_String is
			Str : Unbounded_String := To_Unbounded_String("");
		begin
			for N in 1..8 loop
				Bit := Octet / 128;
				if Integer(Bit) = 1 then Str := Str & To_Unbounded_String("1");
				else Str := Str & To_Unbounded_String("0");
				end if;
				Octet := Octet * 2;

			end loop;
			return Str;
		end Octet_to_String;
		
		procedure GetCharacter(Codages : in T_Arbre; Code : in Unbounded_String) is
		begin
			if Est_Vide(Codages) then Valide := False;
			elsif Codage(Codages) = Code then 
				Valide := True;
				Char(1) := Valeur(Codages);
			else GetCharacter(Suivant(Codages), Code);
			end if;
		end GetCharacter;
		i : Integer := 1;
		Str : Unbounded_String;
	begin
		Create (File2, Out_File, File_Name);
		S2 := Stream (File2);
		while not(End_Of_File(File)) and not (Char(1) = Character'Val(0)) loop
			
			loop
				if i >= Length(CodeCour) then -- si on arrive à la fin de CodeCour on recolle un octet à droit)
					Octet := T_Octet'Input(S);	
					Str := Octet_to_String(Octet);
				else 
					Str := To_Unbounded_String("");
				end if;
				CodeCour := CodeCour & Str;
				GetCharacter(Codages, To_Unbounded_String(Slice(CodeCour, 1, i)));
				i := i mod Length(CodeCour) + 1;
				exit when Valide;
			end loop;
			CodeCour := To_Unbounded_String(Slice(CodeCour, i, Length(CodeCour))); -- on garde le reste													 
			if Char(1) /= Character'Val(0) then 														
				String'Write(S2, Char);
			end if;
			Valide := False;
			i := 1;
		end loop;
		Close(File2);
	end Construire_Fichier;

	Arbre_Huffman : T_Arbre;
	Liste_Codages : T_Arbre;
	
begin
	if Argument_Count = 0 then Put_Line("USAGE : decompresser (-b ou --bavard) fichier1 fichier2 ...");
	elsif Argument_Count >= 2 and (Argument(1) = "-b" or Argument(1) = "--bavard") then	
		for i in 2 .. Argument_Count loop
			Open(File, In_File, Argument(i));

			New_Line(2);
			Put_Line("### Decompression de " & Argument(i) & " ###");
			Put_Line("--/ Reconstruction de l'arbre /--");
			Arbre_Huffman := Construire_Arbre(Argument(i));
			Afficher_arbre(Arbre_Huffman);
			New_Line;
			Put_Line("--/ Reconstruction de la liste des codages /--");
			Liste_Codages := GenererCodages(Arbre_Huffman);
			Afficher_Codages(Liste_Codages);
			Put_Line("--/ Reconstruction du fichier /--");
			Construire_Fichier( Argument(i), Liste_Codages );
			Put_Line("### Fin de la decompression de " & Argument(i) & " ###");
			New_Line(2);

			Close (File);
		end loop;   
	else
		for i in 1 .. Argument_Count loop
			Open(File, In_File, Argument(i));

			New_Line(2);
			Put_Line("### Decompression de " & Argument(i) & " ###");
			Arbre_Huffman := Construire_Arbre(Argument(i));
			Liste_Codages := GenererCodages(Arbre_Huffman);
			Construire_Fichier( Argument(i), Liste_Codages );
			Put_Line("### Fin de la decompression de " & Argument(i) & " ###");
			New_Line(2);

			Close (File);
		end loop;   		
	end if;
end Decompresser;
