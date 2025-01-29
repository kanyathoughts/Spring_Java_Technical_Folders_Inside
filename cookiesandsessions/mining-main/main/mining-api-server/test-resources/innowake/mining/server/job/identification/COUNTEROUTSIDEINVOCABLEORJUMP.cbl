       IDENTIFICATION DIVISION.
       PROGRAM-ID. COUNTEROUTSIDEINVOCABLEORJUMP.
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
       PROCEDURE DIVISION
           MOVE 15 TO B.
           MOVE 10 TO C.
           COMPUTE C = C + 1.
           COMPUTE A = B + 1.
           COMPUTE E = E + A + 1.
           COMPUTE D = 2 * D + 1.
           COMPUTE B = A + C.
           NOT ON SIZE ERROR
           DISPLAY " NOT ON SIZE ERROR ".
           ON SIZE ERROR
           DISPLAY " ON SIZE ERROR ".
           DISPLAY " E IS NOW: " E.
           DISPLAY " F IS NOW: " F.
           STOP RUN.
