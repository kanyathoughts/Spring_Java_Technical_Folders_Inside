           IDENTIFICATION DIVISION.
           PROGRAM-ID. SQLALIASES.
           ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.

           DATA DIVISION.
	       WORKING-STORAGE SECTION.
           EXEC SQL
              INSERT INTO TABLE1
                       (
                          PRN_ID
                         ,A_ID
                        )
                    VALUES
                     (
                       :PRINT_ID
                       :AD_ID123
                     )
                   FOR :A-NUM-ROWS ROWS
                   NOT ATOMIC CONTINUE ON SQLEXCEPTION
           END-EXEC.
           PROCEDURE DIVISION.
              GOBACK.
