
---------------------- crearNotificaciones.sql ----------------------------

-- Notificaciones(Usuario_Origen, Fecha, DNI_cli, NombreE, Tipo , Comision)



DROP TABLE NOTIFICACIONES CASCADE CONSTRAINTS;

create table NOTIFICACIONES ( 
Usuario_Origen	    VARCHAR(20), 
Fecha		    TIMESTAMP(2),
DNI_cli	            CHAR(8)  not null REFERENCES Cliente(DNI),
NombreE	            CHAR(20) not null,
Tipo                CHAR(10) not null,
Comision            FLOAT,
CONSTRAINT PK_notificaciones_prim
 		PRIMARY KEY (Usuario_Origen,Fecha ),
CONSTRAINT FK_notificaciones_empresa
	FOREIGN KEY (NombreE) REFERENCES Empresa(NombreE)  );


GRANT select,insert,update ON  notificaciones TO public;


----- insert INTO ABD1234.notificaciones VALUES('ABDMITUIL', systimestamp, '00000005', 'Empresa 55          ','prueba5   ',55);


