002000 IDENTIFICATION DIVISION.
       PROGRAM-ID. WCFD236.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FILLER.
           05 FILLER PIC X(32) VALUE 'WORKING STORAGE FOR WCFD'.

       LINKAGE SECTION.

       PROCEDURE DIVISION.

                      CALL 'E900VSA1' USING WS-REQUEST
                                            WS-KEY
                                            WS-ROW
                                            WS-STATUS
           GOBACK.

      **************************************************************
      ***                       E900VSA1 ENTRY                   ***
      **************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E900VSA1 IS COMMON PROGRAM.
           COPY FUXVSACC.
       END PROGRAM E900VSA1.
      **************************************************************
      ***                       E900VSA2 ENTRY                   ***
      **************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. E900VSA2.
           COPY FUXVSACC.
       END PROGRAM E900VSA2.
      **************************************************************
      ***                       E900VSA3 ENTRY                   ***
      **************************************************************
       IDENTIFICATION DIVISION.
      ***
      *PROGRAM-ID. E900VSA?.
      ***
       PROGRAM-ID. E900VSA3.
      ***
           COPY FUXVSACC.
       END PROGRAM E900VSA3.
