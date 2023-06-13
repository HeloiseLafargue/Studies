CREATE  OR REPLACE PROCEDURE cursor3  as

-- variable solo visible dentro del procedimiento
-- no permite usar variables exteriores (de acoplamiento)
-- vars de trabajo
      TDNICL CHAR(8);
      TNombreCL CHAR(30);

      Tcoderror NUMBER;
      Ttexterror VARCHAR2(100);



-- cursor para leer datos

      CURSOR cursor_ricos IS
      select dni, nombreC
      from cliente
      where dni in ( select dni from
                     invierte
                     group by dni
                     having sum(cantidad) > 650000);

BEGIN

  OPEN cursor_ricos;

LOOP
  FETCH  cursor_ricos
    INTO TDNICL, TNombreCL;
  EXIT WHEN cursor_ricos%NOTFOUND;
--            --> atencion  DBMS_output.put_line necesita "set serveroutput on"
  IF TDNICL LIKE '%2' THEN
  DBMS_output.put_line('---- cursor3: sumando , DNI: '|| TDNICL);
      update invierte
          set cantidad = cantidad + 1
      where dni = TDNICL;
  END IF;
  IF TDNICL LIKE '%4' THEN
  DBMS_output.put_line('---- cursor3: restando , DNI: '|| TDNICL);
      update invierte
          set cantidad = cantidad - 1
      where dni = TDNICL;
  END IF;
END LOOP;

DBMS_output.put_line('--> cursor3: proceso acabado por EXIT loop');
IF cursor_ricos%ISOPEN 
   THEN  CLOSE cursor_ricos; 
END IF;

EXCEPTION
  WHEN NO_DATA_FOUND THEN
   DBMS_output.put_line('--> cursor3: proceso acabado por NODATAFOUND');
   IF cursor_ricos%ISOPEN 
     THEN  CLOSE cursor_ricos; 
   END IF;

  WHEN OTHERS THEN
	Tcoderror:= SQLCODE;
	Ttexterror:= SUBSTR(SQLERRM,1, 100);
  DBMS_output.put_line('--> cursor3: ERROR en DNI : '|| TDNICL );
  DBMS_output.put_line('--> cursor3: ERROR COD: '|| Tcoderror);
  DBMS_output.put_line('--> cursor3: ERROR texto: ' ||  Ttexterror );
END cursor3;

/

show errors

---------- para ejecutarlo en SQL*PLUS -----------------
--   begin
--     cursor3;
--   end;







