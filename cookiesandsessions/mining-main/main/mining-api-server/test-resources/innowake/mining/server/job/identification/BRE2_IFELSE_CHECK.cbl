       IDENTIFICATION DIVISION.
       PROGRAM-ID.  EXTINPUT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 A PIC 9(9) VALUES(5).
       01 B PIC 9(9) VALUES(42).
       01 C PIC 9(9) VALUES(42).
       01 D PIC 9(9) VALUES(42).

        PROCEDURE DIVISION.

      * Candidate: body (if) contains a FILE_ACCESS_STATEMENT
           COMPUTE C = B + A + 1.
           COMPUTE B = B + D + 1.
           IF A > +0
             MOVE A TO C 
            ELSE 
             DiSPLAY '4'
           END-IF 
      
       END PROGRAM TEST.
