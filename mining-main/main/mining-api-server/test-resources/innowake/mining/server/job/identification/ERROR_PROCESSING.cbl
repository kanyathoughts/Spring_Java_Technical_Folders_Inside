           IDENTIFICATION DIVISION.
           PROGRAM-ID. ERROR.

           DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 WS-STRING PIC A(30).
           01 WS-STR1 PIC A(15) VALUE 'Hello'.
           01 WS-STR2 PIC A(7) VALUE 'Welcome'.
           01 WS-STR3 PIC A(7) VALUE 'To AND'.
           01 WS-COUNT PIC 99 VALUE 1.
           01 NOT-EOF                   PIC X(01)      VALUE 'N'.
           01 I-FROM                    PIC S9(4)   COMP VALUE 0.
           01 XML-STRING                PIC X(1000) VALUE SPACES.
           01 EZ-PTR                    PIC S9(4) COMP VALUE 1.

           FILE SECTION.
           FD dataFile.
           
           PROCEDURE DIVISION.
           PERFORM TESTPARA
           GOBACK.
       TESTPARA.
            ADD 1 TO TESTFIELD
            ON SIZE ERROR
            DISPLAY 'B'.
       UNREACHABLE.
            DISPLAY 'UNREACHABLE'
   
            STRING WS-STR2 DELIMITED BY SIZE
            WS-STR3 DELIMITED BY SPACE
            WS-STR1 DELIMITED BY SIZE
             INTO WS-STRING 
             WITH POINTER WS-COUNT
             ON OVERFLOW DISPLAY 'OVERFLOW!' 
            END-STRING.
            DISPLAY 'XML-DOCUMENT=' XML-STRING(1:EZ-PTR)
              XML PARSE XML-STRING(1:EZ-PTR)
                          PROCESSING PROCEDURE XML-HANDLER
              ON EXCEPTION
                   DISPLAY 'XML DOCUMENT ERROR ' XML-CODE
              NOT ON EXCEPTION
                   DISPLAY 'XML DOCUMENT SUCCESSFULLY PARSED'
              END-XML
              
               EXEC CICS HANDLE ABEND
                  LABEL(ABEND-ROUTINE)
           END-EXEC.
           ABEND-ROUTINE.
            MOVE "ABEND OCCURED." TO MSG-DATA1.
            MOVE "TASK CANCELLED WITH ABCODE 9999." TO MSG-DATA2.
            MOVE 65 TO MSG-LEN.
           EXEC CICS SEND
                  FROM (MSG-DATA)
                  LENGTH(MSG-LEN)
                  NOHANDLE
           END-EXEC.
   
            DISPLAY 'WS-STRING : 'WS-STRING.
            DISPLAY 'WS-COUNT : 'WS-COUNT.

           OPEN INPUT dataFile
          PERFORM UNTIL endOfFile
              READ dataFile
                  AT END SET endOfFile TO TRUE
              END-READ
              IF someError THEN
                  PERFORM errorProcedure
              END-IF
          END-PERFORM
          CLOSE dataFile
          GOBACK.
          errorProcedure.
          USE AFTER ERROR PROCEDURE
          DISPLAY "An error occurred while processing the data file."
          CLOSE dataFile
           STOP RUN.
