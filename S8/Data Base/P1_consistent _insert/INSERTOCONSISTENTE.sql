create or replace PROCEDURE insertoconsistente (
    dni_p      invierte.dni%TYPE,
    nombree_p  invierte.nombree%TYPE,
    cantidad_p invierte.cantidad%TYPE,
    tipo_p     invierte.tipo%TYPE
) AS


-- variables de trabajo para las decisiones (indicadas después del BEGIN)
    VARIABLE_SIESTA NUMBER;
    VARIABLE_TIPO NUMBER;
    VARIABLE_EMPRESA NUMBER;
    CASO NUMBER;

-- VARS para excepciones 
    tcoderror  NUMBER;
    ttexterror VARCHAR2(100);

BEGIN

-- Decido si ese DNI es nuevo (no tiene inversiones) : si 0 es nuevo


-- Decido si es Tipo nuevo para ese cliente: si 0 es nuevo


-- Decido si es empresa nueva para ese cliente: si 0 es nuevo

    SELECT COUNT(*)INTO VARIABLE_SIESTA
        FROM INVIERTE
        WHERE DNI = dni_p;

    dbms_output.put_line('Aquí empieza INSERTOCONSISTENTE');
--
--------- muestro los datos de entrada (parámetros) con los que trabajo
    dbms_output.put_line('Datos de entrada: ' || dni_p || ', ' || nombree_p || ', ' || cantidad_p || ', ' || tipo_p);

-- Si no exístían inversiones para ese DNI 


-- CASO 0.- No hay inversiones de ese DNI: inserto la fila y termino

--- imprimo la fila nueva

    IF VARIABLE_SIESTA = 0 THEN
        DBMS_output.put_line('CASO 0: inserto '|| dni_p || ', ' || nombree_p || ', ' || cantidad_p || ', ' || tipo_p);
        insert into invierte 
        values (dni_p,nombree_p,cantidad_p,tipo_p);
    
    ELSE

  --- ==  RESTO DE CASOS (tenemos el mismo DNI que en alguna fila de Invierte == ------

-- EN que CASO estoy?
        SELECT COUNT(*)INTO VARIABLE_EMPRESA
            FROM INVIERTE
            WHERE DNI = dni_p AND NombreE = nombree_p;

        SELECT COUNT(*)INTO VARIABLE_TIPO
            FROM INVIERTE
            WHERE DNI = dni_p AND Tipo = tipo_p;

        CASE 
            WHEN VARIABLE_TIPO > 0 AND VARIABLE_EMPRESA > 0 THEN CASO := 1; -- Ya existe una fila con mismo Tipo (1) y Empresa (1)
            WHEN VARIABLE_TIPO = 0 AND VARIABLE_EMPRESA > 0 THEN CASO := 2; -- Tipo nuevo para una Empresa que ya hay inversiones
            WHEN VARIABLE_TIPO > 0 AND VARIABLE_EMPRESA = 0 THEN CASO := 3; -- Empresa nueva para un tipo que ya hay inversiones
            ELSE CASO := 4; -- El tipo y la empresa son nuevos
        END CASE;
        
-- imprimo en el caso que estamos
        DBMS_output.put_line('CASO ' || CASO || ': ');

-- CASO 1.-  Ya existe una fila con mismo Tipo (1) y Empresa (1) : es un error, no se lo permito
        IF CASO = 1 THEN
            DBMS_output.put_line('ERROR');
            --RAISE_APPLICATION_ERROR(-20001, 'ERROR: Ya existe una fila con mismo Tipo y Empresa');  

-- CASO 2.-  tipo nuevo para una Empresa que ya hay inversiones: debo insertar filas con ese tipo para todas sus empresas
    -- Imprimo cada fila nueva
        ELSIF CASO = 2 THEN
            DBMS_output.put_line('Forzado a insertar el tipo nuevo "' || tipo_p || '" para todas las empresas: ');
            FOR empresa IN (SELECT DISTINCT NombreE FROM INVIERTE WHERE DNI = dni_p)
            LOOP
                DBMS_output.put_line('inserto '|| dni_p || ', ' || empresa.NombreE || ', ' || cantidad_p || ', ' || tipo_p);
                insert into invierte 
                values (dni_p,empresa.NombreE,cantidad_p,tipo_p);
            END LOOP;

-- CASO 3.- Empresa nueva para un tipo que ya hay inversiones: debo insertar filas con ese empresa para todos sus tipos
--          No tomo en cuenta la nueva cantidad (es complejo comprobar la antigüa si hay varias empresas con ese Tipo)
    -- Imprimo cada fila nueva
        ELSIF CASO = 3 THEN
            DBMS_output.put_line('Forzado a insertar para la empresa nueva "' || nombree_p || '" todos los tipos: ');
            FOR tipoinv IN (SELECT DISTINCT Tipo FROM INVIERTE WHERE DNI = dni_p)
            LOOP
                DBMS_output.put_line('inserto '|| dni_p || ', ' || nombree_p || ', ' || cantidad_p || ', ' || tipoinv.Tipo);
                insert into invierte
                values (dni_p,nombree_p,cantidad_p,tipoinv.Tipo);
            END LOOP;

-- CASO 4.- El tipo y la empresa son nuevos: Como  CASO 2 + CASO 3
  
  ------ para cada empresa que había tengo que insertar el bono nuevo
   -- Imprimo cada fila nueva
        ELSIF CASO = 4 THEN 
            DBMS_output.put_line('Forzado a insertar el tipo nuevo "' || tipo_p || '" para todas las empresas: ');
            FOR empresa IN (SELECT DISTINCT NombreE FROM INVIERTE WHERE DNI = dni_p)
            LOOP
                DBMS_output.put_line('inserto '|| dni_p || ', ' || empresa.NombreE || ', ' || cantidad_p || ', ' || tipo_p);
                insert into invierte
                values (dni_p,empresa.NombreE,cantidad_p,tipo_p);
            END LOOP;
  ------- para esta empresa nueva tengo que insertar todos los bonos que había 
  --      (como incluye el bono nuevo, que está ya en la BD, genera también 
  --         la fila de los argumentos de entrada)
   -- Imprimo cada fila nueva
            DBMS_output.put_line('Forzado a insertar para la empresa nueva "' || nombree_p || '" todos los tipos: ');
            FOR tipoinv IN (SELECT DISTINCT Tipo FROM INVIERTE WHERE DNI = dni_p)
            LOOP
                DBMS_output.put_line('inserto '|| dni_p || ', ' || nombree_p || ', ' || cantidad_p || ', ' || tipoinv.Tipo);
                insert into invierte
                values (dni_p,nombree_p,cantidad_p,tipoinv.Tipo);
            END LOOP;
        END IF;
    END IF;

EXCEPTION
    WHEN OTHERS THEN
        tcoderror := sqlcode;
        ttexterror := substr(sqlerrm, 1, 100);
        dbms_output.put_line('Sale por una excepcion: '
                             || tcoderror
                             || '  -- '
                             || ttexterror);
        dbms_output.put_line('$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$');


END;