
create or replace PROCEDURE suma_inversiones (dniCliente cliente.dni%TYPE) AS

-- Obtengo lista de todas las empresas que hay
 
    CURSOR empresasCursor IS
    SELECT nombree
    FROM empresa;
    regEmpresa empresasCursor%ROWTYPE;


    sumaInversiones INT;
    plsql_block VARCHAR2(3000);
    existeEmp INT := 0;
    sumaTotal INT := 0;

BEGIN
    dbms_output.put_line('---- Cálculo de Suma de Inversiones para el DNI: ' 
	                       || dniCliente || '------');
--                       para cada empresa: 
    OPEN empresasCursor;
    LOOP
        FETCH empresasCursor INTO regempresa;
        EXIT WHEN empresasCursor%NOTFOUND;
--                         consulto si tiene tabla de inversiones
        SELECT COUNT(*) INTO existeEmp
        FROM tabs
        WHERE table_name = 'INVERSIONES_' || REPLACE(UPPER(regempresa.nombree), ' ', '');

        IF existeEmp <> 0 THEN
--		                          recupero todas las inversiones del cliente dado
            plsql_block := 'BEGIN
                            SELECT NVL(SUM(cantidad), 0) INTO :tot 
                            FROM inversiones_' || REPLACE(regempresa.nombree, ' ', '') || 
                           ' WHERE dni = ' || dniCliente || ';
                            END;';
            EXECUTE IMMEDIATE plsql_block USING OUT sumaInversiones;
            IF sumainversiones <> 0 THEN
		    	dbms_output.put_line('El total invertido en la empresa: ' 
                      || regempresa.nombree || 'es: ' || sumaInversiones);
            END IF;
            sumatotal := sumaTotal + sumaInversiones;
        END IF;     
    END LOOP;
    CLOSE empresasCursor;

    dbms_output.put_line(' ==> La suma total de la inversion es: ' || sumatotal);
END;
/* 

execute suma_inversiones ('00000002');

*/
