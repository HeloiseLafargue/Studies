create or replace PROCEDURE proc3plus2 (
	dnibusca cliente.DNI%TYPE, 
	NombreCL   cliente.NombreC%TYPE,
	TelCL 	cliente.Telefono%TYPE ,
	DirCL      cliente.Direccion%TYPE
      ) as

-- variable solo visible dentro del procedimiento

-- vars de trabajo
      TDNICL CHAR(8);
      TNombreCL CHAR(30);

      Tcoderror NUMBER;
      Ttexterror VARCHAR2(100);
-- vars para leer datos



BEGIN

  DBMS_output.put_line('--- proc3plus, begin '|| dnibusca);
  update cliente 
       set NombreC = NombreCL  
  WHERE DNI = dnibusca;

/*  ------ EXPLICATON : 
If a SELECT INTO statement fails to return a row, 
  PL/SQL raises the predefined exception NO_DATA_FOUND 
   whether you check SQL%NOTFOUND on the next line or not.
   
However, a SELECT INTO statement that calls a SQL aggregate function
  never raises NO_DATA_FOUND because those functions 
  always return a value or a null. 
  In such cases, SQL%NOTFOUND yields FALSE. 
*/

 IF SQL%NOTFOUND THEN
  DBMS_output.put_line('--- proc3plus, no encuentra y crea el DNI:  '|| dnibusca);
	insert into cliente 
		values (dnibusca,NombreCL,DirCL,TelCL);
  END IF;
TDNICL := dnibusca;
IF TDNICL = '00000005' THEN
      DBMS_output.put_line('--- proc3plus, en el IF: '|| TDNICL);
   ELSE
      TDNICL := ':ELSE ';
      DBMS_output.put_line('--- proc3plus, otro cliente: '|| TDNICL);
   END IF;

END proc3plus2;


---------- para ejecutarlo en l la Hoja de Trabajo -----------------

--   begin     -- CASO: cliente no existe
--     proc3plus2('000xx005','nombreXX5nuevo', '5550055','dir 5 plusx');
--   end;


--   begin     -- CASO: cliente YA existe
--     proc3plus('00000005','nombre 5 nuevo', '5550055','dir 5 nueva');
--   end;
