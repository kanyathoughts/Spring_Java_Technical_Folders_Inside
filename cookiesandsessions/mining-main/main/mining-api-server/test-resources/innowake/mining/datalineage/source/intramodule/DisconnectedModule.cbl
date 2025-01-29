       IDENTIFICATION DIVISION.

       PROGRAM-ID.    MEE5927A.
       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT  REPORT-FILE        ASSIGN TO MYREPORT.
       DATA DIVISION.
       FILE SECTION.
024100 FD  REPORT-FILE                                                  01900001
024200     LABEL RECORDS OMITTED                                        01910001
024300     RECORDING MODE IS F                                          01911001
024400     BLOCK CONTAINS 0 RECORDS                                     01920001
024500     RECORD CONTAINS 133 CHARACTERS                               01930001
024600     REPORT IS W1058235-REPORT.                                   01940001

       WORKING-STORAGE SECTION.
       01  FILE-STATUS PIC 9(2).
       01  R-CLMT-ID PIC X(8).

051100 REPORT SECTION.                                                  03023701
051200*                                                                 03023801
051300 RD  W1058235-REPORT                                              03023901
051400     PAGE LIMIT IS 60 LINES                                       03024001
051500     HEADING 1                                                    03024101
051600     FIRST DETAIL 4.                                              03024201
051700*                                                                 03024301
       01  W1058235-HEADINGS                                            03024401
051900     TYPE IS PAGE HEADING.                                        03024501
052000     05  LINE NUMBER IS 1.                                        03024601
052100         10  COLUMN 58                                            03025001
052200                    VALUE 'STATE OF WASHINGTON'                   03030001
052300                                    PIC  X(19).                   03040001
052400     05  LINE NUMBER IS 2.                                        03070001
052500         10  COLUMN 50                                            03080001
052600                    VALUE 'DEPARTMENT OF LABOR AND INDUSTRIES'    03090001
052700                                    PIC  X(34).                   03100001
052800         10  COLUMN 119                                           03110001
052900                    VALUE 'PAGE     '                             03111001
053000                                    PIC  X(09).                   03112001
053100         10  COLUMN 128                                           03120001
053200                    SOURCE IS PAGE-COUNTER                        03121001
053300                                    PIC ZZZ9.                     03122001
057700 01  W1058235-DETAIL                                              03420001
057800     TYPE IS DETAIL.                                              03421001
057900     05  LINE PLUS 2.                                             03422001
058000         10  COLUMN 1    VALUE ' CLMT-ID:  '                      03440001
058100                                    PIC  X(11).                   03441001
058200         10  COLUMN 12   SOURCE IS R-CLMT-ID                      03450001
058300                                    PIC  X(08).                   03451001
057600                                                                  03410001
097000     EJECT                                                        07340001
       PROCEDURE DIVISION.
           OPEN OUTPUT REPORT-FILE.

           INITIATE W1058235-REPORT.

            DISPLAY PAGE-COUNTER.

           MOVE '11111111' TO R-CLMT-ID.
           GENERATE W1058235-DETAIL.

           MOVE 5 TO PAGE-COUNTER.
           DISPLAY PAGE-COUNTER.

           MOVE '22222222' TO R-CLMT-ID.
           GENERATE W1058235-DETAIL.

           DISPLAY PAGE-COUNTER.

           TERMINATE W1058235-REPORT.

           CLOSE REPORT-FILE.
       GOBACK.
