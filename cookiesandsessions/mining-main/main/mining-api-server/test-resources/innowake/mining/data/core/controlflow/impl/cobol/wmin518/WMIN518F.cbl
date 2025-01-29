       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN518F.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
           PERFORM TESTPARA
           GOBACK.
       TESTPARA.
            SEARCH OUTPUT-RECORD
            WHEN F1 EQUAL 1
            DISPLAY 'A'
            WHEN F1 EQUAL 2
            DISPLAY 'B'
            AT END
            DISPLAY 'C'
            END-SEARCH.
       UNREACHABLE.
            DISPLAY 'UNREACHABLE'
