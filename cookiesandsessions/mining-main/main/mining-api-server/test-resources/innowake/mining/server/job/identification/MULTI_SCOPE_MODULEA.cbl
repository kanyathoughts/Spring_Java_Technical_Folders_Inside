       IDENTIFICATION DIVISION.
       PROGRAM-ID. MODULEA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        EXEC SQL
          INCLUDE MULTI_SCOPE_MODULEC
        END-EXEC
       PROCEDURE DIVISION.
       0000-INITIALIZE.
           EXEC SQL
               DELETE FROM
                        TABLE1
                      WHERE
                        SUBJ_ID = :W-SUBJECT-ID
            END-EXEC
