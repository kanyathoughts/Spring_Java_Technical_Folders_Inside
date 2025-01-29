       IDENTIFICATION DIVISION.
       PROGRAM-ID. AND3CLEE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

        01 WS-FIELD PIC X.

       LINKAGE SECTION.

        01 G1.
           05 G1-F1 PIC X.
           05 G1-F2 PIC X.
           05 G1-F3 PIC X.
           05 G1-F4 PIC X.

        01 G2.
           05 G2-F1 PIC X.
           05 G2-F2 PIC X.
           05 G2-F3 PIC X.
           05 G2-F4 PIC X.

       PROCEDURE DIVISION USING G1 G2.

           DISPLAY G1 G2.

       END PROGRAM AND3CLEE.
