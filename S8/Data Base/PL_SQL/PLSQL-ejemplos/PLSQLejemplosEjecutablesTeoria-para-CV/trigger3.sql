CREATE OR REPLACE TRIGGER trigger3

AFTER UPDATE OR INSERT 
 OF cantidad ON invierte
-- 
 
FOR EACH ROW
WHEN (new.cantidad > 1000000.00)

DECLARE

-- vars de trabajo
 

      Tcoderror NUMBER;
      Ttexterror VARCHAR2(100);
      Tnumfact compras.NumF%Type;
 

BEGIN
select cuentapremio.nextval into Tnumfact from dual;


DBMS_output.put_line('--- trigger3, CREAR: '|| :new.DNI ||'-'|| Tnumfact ||'-'|| :new.NombreE || '100' );

  INSERT into compras
    VALUES (:new.DNI, 10000001,Tnumfact,999,'REGALO', 100);

DBMS_output.put_line('--- trigger3, CREADO');


EXCEPTION
  WHEN OTHERS THEN
	Tcoderror:= SQLCODE;
	Ttexterror:= SUBSTR(SQLERRM,1, 100);
DBMS_OUTPUT.PUT_LINE('--trigger3, ERROR '|| Tcoderror || ' -- ' || Ttexterror);

END trigger3;

/


show errors

---------- para ejecutarlo en SQL*PLUS -----------------
-- create sequence cuentapremio;
-- select cuentapremio.nextval from dual;
-- 
--update invierte 
-- set cantidad = 1000001
-- where dni = '00000003';


