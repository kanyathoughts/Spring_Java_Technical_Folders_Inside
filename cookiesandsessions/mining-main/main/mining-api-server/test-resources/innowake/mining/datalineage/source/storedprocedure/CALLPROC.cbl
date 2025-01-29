       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALLPROC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
         01  HOST-VARS.
           10 ALPHA-LONG PIC X(80).
           10 ALPHA-SHORT PIC X(8).
           10 OTHER-FIELD PIC X(8).
           10 NUMBER-FIELD PIC S9(4) COMP.
       PROCEDURE DIVISION.
       BEGIN.

           EXEC SQL
           CALL SP_SUBSCRIBE (:ALPHA-LONG,
                        :ALPHA-SHORT,
                        'HELLO OO',
                        :NUMBER-FIELD,
                        5,
                        :OTHER-FIELD);
           END-EXEC.

           GOBACK.
       END PROGRAM CALLPROC.

