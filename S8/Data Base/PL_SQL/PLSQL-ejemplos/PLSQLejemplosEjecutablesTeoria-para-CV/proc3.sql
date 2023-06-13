create or replace PROCEDURE proc3 (
	dnibusca cliente.DNI%TYPE, 
	NombreCL   cliente.NombreC%TYPE,
	TelCL 	cliente.Telefono%TYPE ,
	DirCL      cliente.Direccion%TYPE
      ) as

-- variables solo visible dentro del procedimiento

-- vars de trabajo
      TDNICL CHAR(8);
      TNombreCL CHAR(30);

-- para manejo de excepciones
      Tcoderror NUMBER;  
      Ttexterror VARCHAR2(100);
      
-- vars para leer datos

	cliente_listillo EXCEPTION;

BEGIN

  SELECT DNI, NombreC
    INTO TDNICL, TNombreCL 
    FROM Cliente
    WHERE DNI = dnibusca;



IF TDNICL = '00000005' THEN
      RAISE cliente_listillo;
   ELSE
      TDNICL := 'PROC3: -- ELSE ';
   END IF;

 DBMS_output.put_line('--- proc3, después IF: '|| TDNICL);
 
EXCEPTION
  WHEN NO_DATA_FOUND THEN
	Tcoderror:= SQLCODE;
	Ttexterror:= SUBSTR(SQLERRM,1, 100);

   DBMS_OUTPUT.PUT_LINE('-- proc3, nodataFound: no está, lo inserta '
               || Tcoderror || ' -- ' || Ttexterror);

	insert into cliente values (dnibusca,NombreCL,DirCL,TelCL);

  WHEN cliente_listillo THEN
	Tcoderror:= SQLCODE;   -- es de oracle, da el código de error
	Ttexterror:= '----- PROC3: CLIENTE PELIGROSO ------';
    DBMS_OUTPUT.PUT_LINE('-- proc3, ERROR '|| Tcoderror || ' -- ' || Ttexterror);


  WHEN OTHERS THEN
	Tcoderror:= SQLCODE; -- es de oracle, da el código de error
	Ttexterror:= SUBSTR(SQLERRM,1, 100); -- es de oracle, da el texto de error
     DBMS_OUTPUT.PUT_LINE('-- proc3, OTHERS ERROR '|| Tcoderror || ' -- ' || Ttexterror);

END proc3;


---------- para ejecutarlo en Hoja de Trabajo -----------------

--   begin       -- CASO: cliente YA existe
--     proc3('00000005','nombre 5 nuevo', '5550055','dir 5 nueva');
--   end;

-- begin         -- CASO: Cliente que no existe: lo crea
--  proc3('0000yy05','nombre 5 nuevo', '5550055','dir 5 proc3');
-- end;
