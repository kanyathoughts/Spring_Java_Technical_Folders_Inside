       IDENTIFICATION DIVISION.
       PROGRAM-ID.  BRE2_TECHNICAL3.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TEST-FILE
             ASSIGN TO OUTFILE
             ORGANIZATION IS SEQUENTIAL
             FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
          FILE SECTION.
          FD STUDENT.
          01 STUDENT-FILE.
             05 STUDENT-ID PIC 9(5).
             05 NAME PIC A(25).
        
          WORKING-STORAGE SECTION.
          01 WS-STUDENT.
             05 WS-STUDENT-ID PIC 9(5).
             05 WS-NAME PIC A(25).
          01 WS-EOF PIC A(1).
        
       PROCEDURE DIVISION.
          OPEN INPUT STUDENT.
      ** Start of Technical Rule:
             PERFORM UNTIL WS-EOF = 'Y'
                READ STUDENT INTO WS-STUDENT
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END DISPLAY WS-STUDENT
                END-READ
             END-PERFORM.
      ** End of Technical Rule
          CLOSE STUDENT.
       STOP RUN.
