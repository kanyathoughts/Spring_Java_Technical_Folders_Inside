       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN518H.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
           PERFORM TESTPARA
           GOBACK.
       TESTPARA.
            READ RECORDDUMMY NEXT
            AT END
            DISPLAY 'A'
            NOT AT END
            DISPLAY 'B'
            END-READ.
       UNREACHABLE.
            DISPLAY 'UNREACHABLE'
