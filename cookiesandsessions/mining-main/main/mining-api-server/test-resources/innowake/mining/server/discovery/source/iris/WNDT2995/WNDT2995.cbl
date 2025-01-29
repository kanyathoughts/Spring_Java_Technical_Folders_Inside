       IDENTIFICATION DIVISION.
       PROGRAM-ID. WNDT3025.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.
       BEGIN.
          EXEC SQL
            select 1, concat('Value', '1', '2') order by concat
          END-EXEC.
       GOBACK.
