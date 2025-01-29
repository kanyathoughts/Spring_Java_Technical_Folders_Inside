       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           TEST20.
      *
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
      *
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       WORKING-STORAGE SECTION.
      *
           01 KCSPAB.
               09 KOMM-RETURN-CODE         PIC  +9(0008) VALUE ZERO.
                  88    KOMM-RETURN-CODE-OKAY   VALUE ZERO.
                  88    KOMM-FEHLERMELDUNG      VALUE "+00001000"
                                                       THRU
                                                      "+99999999",
                                                      "-00000001"
                                                       THRU
                                                       "-99999999".
      *
       PROCEDURE DIVISION.
      *
       TEST20 SECTION.
       TEST20-START.
      *
           IF KOMM-RETURN-CODE-OKAY
                DISPLAY "SHOULD GO IN HERE"
           END-IF

           IF KOMM-RETURN-CODE IS EQUAL ZERO
                DISPLAY "SHOULD ALSO GO IN HERE"
           END-IF.

           CALL "CAP_UTL_SQL_23_CURSOR_OPEN" USING
              SQLCA
              LAST_MONTHS_DATE_QUAD
           END-CALL
      *
       TEST20-EXIT.
           STOP RUN.
