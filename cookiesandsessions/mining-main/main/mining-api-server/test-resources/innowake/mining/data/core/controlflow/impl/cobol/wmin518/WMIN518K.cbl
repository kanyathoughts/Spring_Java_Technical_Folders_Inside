       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN518E.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
           PERFORM TESTPARA
           GOBACK.
       TESTPARA.
            START  OUTPUT-RECORD KEY EQUAL PK
            INVALID KEY
            DISPLAY 'A'
            END-START.
       UNREACHABLE.
            DISPLAY 'UNREACHABLE'
