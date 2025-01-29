       IDENTIFICATION DIVISION.
       PROGRAM-ID. MODULEB.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        EXEC SQL
          INCLUDE MULTI_TABLES_MODULEC
        END-EXEC
       PROCEDURE DIVISION.
       0000-INITIALIZE.
           EXEC SQL
               DELETE FROM
                        TABLE2
                      WHERE
                        SUBJ_ID = :W-SUBJECT-ID
            END-EXEC
