       IDENTIFICATION DIVISION.
       PROGRAM-ID. IFSTATEMENT.

       DATA DIVISION.
          WORKING-STORAGE SECTION.
          01 IO-STATUS PIC 9(9).
          01 WS-NUM1 PIC 9(9).
          01 WS-NUM2 PIC 9(9).
          01 WS-NUM3 PIC 9(5).
          01 WS-NUM4 PIC 9(6).

       PROCEDURE DIVISION.
          A000-FIRST-PARA.
          MOVE 25 TO WS-NUM1 WS-NUM3.
          MOVE 15 TO WS-NUM2 WS-NUM4.

          IF WS-NUM1 > WS-NUM2 THEN
       	  DISPLAY WS-NUM1

          IF IO-STATUS
       	     IF WS-NUM3 = WS-NUM4 THEN
                  DISPLAY WS-NUM3
       	     ELSE
                  DISPLAY WS-NUM4
       	     END-IF
       	  END-IF

          ELSE
       	  DISPLAY WS-NUM2
          END-IF.

       STOP RUN.
