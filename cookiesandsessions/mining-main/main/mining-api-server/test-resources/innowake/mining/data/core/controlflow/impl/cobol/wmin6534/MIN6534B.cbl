000100 IDENTIFICATION DIVISION.
000100 PROGRAM-ID. MIN6534B.

000100 DATA DIVISION.
000100 WORKING-STORAGE SECTION.
000100 01  N PIC 9 VALUE 2.

000100       PROCEDURE DIVISION.
000100        PARA-A.
              DISPLAY 'IN PARA-A'
              GO TO PARA-C.

              PARA-B.
              DISPLAY 'IN PARA-B '.

              PARA-C.
              DISPLAY 'IN PARA-C '.
              GO TO PARA-E PARA-F PARA-G DEPENDING ON N.

              PARA-D.
              DISPLAY 'IN PARA-D '.

              PARA-E.
              DISPLAY 'IN PARA-E '.

              PARA-F.
              DISPLAY 'IN PARA-F '.

              PARA-G.
              DISPLAY 'IN PARA-G '.

           STOP RUN.
