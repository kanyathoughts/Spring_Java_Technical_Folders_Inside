IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN6069.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       EXEC SQL
      		VALUES IDENTITY_VAL_LOCAL()                                       
            INTO : ID912-CLNT-GR-RULE-ID
       END-EXEC
       
       EXEC SQL
            DECLARE ALIAS FILENAME 'HSI_DB'
       END-EXEC
       
       EXEC SQL
       		DECLARE TRANSACTION READ ONLY WAIT 5
       END-EXEC
       
       EXEC SQL
       		EXECUTE IMMEDIATE 'CREATE TABLE scheme (id NUMBER, amt NUMBER)'
       END-EXEC
       
       EXEC SQL
       		GETERROR INTO :RDMS-ERROR-TEXT(1), :RDMS-ERROR-TEXT(2),
       					  :RDMS-ERROR-TEXT(3)
       END-EXEC
