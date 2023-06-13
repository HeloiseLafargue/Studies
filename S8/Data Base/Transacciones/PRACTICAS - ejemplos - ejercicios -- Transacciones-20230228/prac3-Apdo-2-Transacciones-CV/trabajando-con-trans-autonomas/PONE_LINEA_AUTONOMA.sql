create or replace PROCEDURE pone_linea_autonoma (milinea IN varchar)
  as
;
 
--- en proc que llama tiene: milinea varchar(100) :=  ' ';
 
 -- Hacemos transacción autónoma 
 PRAGMA AUTONOMOUS_TRANSACTION;
 
BEGIN 

SET TRANSACTION ISOLATION LEVEL READ COMMITTED 
    NAME 'Línea_Autónoma';


DBMS_OUTPUT.PUT_LINE(milinea || ' Num.Trans.Secun: ' ||
                       dbms_transaction.local_transaction_id);

commit;  -- termina transacción, es obligatorio


end pone_linea_autonoma;

