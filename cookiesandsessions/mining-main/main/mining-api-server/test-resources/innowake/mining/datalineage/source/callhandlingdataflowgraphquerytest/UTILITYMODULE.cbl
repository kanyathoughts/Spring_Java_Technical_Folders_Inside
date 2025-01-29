        IDENTIFICATION DIVISION.
        PROGRAM-ID. UTILITYMODULE.
        DATA DIVISION.
        WORKING-STORAGE SECTION.

         01 A PIC X.
         01 B PIC X.
         01 C PIC X.
         01  CBLTDLI                     PIC X(8) VALUE 'CBLTDLI'.

        PROCEDURE DIVISION.

          MOVE "X" TO A.
          CALL CBLTDLI USING A, B, C.

        END PROGRAM UTILITYMODULE.
