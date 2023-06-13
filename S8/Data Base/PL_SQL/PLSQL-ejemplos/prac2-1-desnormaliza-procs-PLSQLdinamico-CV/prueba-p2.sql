-------------  SCRIPT AUTOCONTENIDO PARA PROBAR TODOS LOS CASOS  -----------

SET SERVEROUTPUT ON SIZE 100000;

-- 1° Procedimiento crea_sec_inversion(nombreEmpresa)
-- Caso en el que la secuencia no existe
execute CREA_SEC_INVERSION('PEPE');
-- Caso en el que la secuencia ya existe
execute CREA_SEC_INVERSION('PEPE');

-- 2º Procedimiento crea_tabla_inversiones(nombreEmpresa)
-- Caso en el que la tabla no existe
execute CREA_TABLA_INVERSIONES('PEPE3');
-- Caso en el que la tabla ya existe
execute CREA_TABLA_INVERSIONES('PEPE3');


