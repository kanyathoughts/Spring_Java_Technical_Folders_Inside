       IDENTIFICATION DIVISION.
       PROGRAM-ID. FUNC.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GP-1.
          02 A PIC 9(2).
          02 B PIC 9(2).
          02 C PIC 9(2).
          02 D PIC 9(2).
          02 E PIC 9(2).
          02 F PIC 9(2).
          02 G PIC 9(2).
          
       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE 15 TO B.
           MOVE 10 TO C.
           COMPUTE A G = B - C.
           DISPLAY " A IS NOW: " A.
           COMPUTE D ROUNDED = A + B.
           DISPLAY " D IS NOW: " D.
           COMPUTE E ROUNDED = ( A + B ) / ( D - C )
           NOT ON SIZE ERROR
           DISPLAY " NOT ON SIZE ERROR ".
           COMPUTE F ROUNDED = B * C
           ON SIZE ERROR
           DISPLAY " ON SIZE ERROR ".
           DISPLAY " E IS NOW: " E.
           DISPLAY " F IS NOW: " F.
           DISPLAY " G IS NOW: " G.
           STOP RUN.
