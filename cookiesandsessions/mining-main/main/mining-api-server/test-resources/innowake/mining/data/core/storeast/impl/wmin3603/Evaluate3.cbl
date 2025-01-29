       IDENTIFICATION DIVISION.
       PROGRAM-ID.     Evaluate1.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       WORKING-STORAGE SECTION.
       01 A PIC 9(9) VALUE 5.
       01 B PIC 9(9) VALUE 42.
       
       PROCEDURE DIVISION.
       MAIN-PROGRAM SECTION.
       
           EVALUATE TRUE
             WHEN A AND B
                DISPLAY 'A&B'
             WHEN A OR B
                DISPLAY 'A|B'
                EVALUATE TRUE
                  WHEN A AND B
                    DISPLAY 'Nested A&B'
                  WHEN A OR B
                    DISPLAY 'Nested A|B'
                  WHEN OTHER
                    DISPLAY 'Nested OTHER'
                END-EVALUATE.
             WHEN OTHER
                DISPLAY 'OTHER'
           END-EVALUATE.
             
           EVALUATE A
             WHEN 5
               DISPLAY '5'
               EVALUATE B
                 WHEN 1
                   DISPLAY 'Nested 1'
                 WHEN 3
                   DISPLAY 'Nested 3'
               END-EVALUATE.
             WHEN 42
               DISPLAY '42'
           END-EVALUATE.
           
           EVALUATE TRUE ALSO A
             WHEN B = 4 ALSO 2
                DISPLAY 'WHAT'
                WHEN A AND B ALSO 42
                    DISPLAY 'Nested IS THIS'
                WHEN B = 4 ALSO 2
                    DISPLAY 'Nested WHAT'
                WHEN A AND B ALSO 42
                    DISPLAY 'Nested IS THIS'
                END-EVALUATE.
           END-EVALUATE.

           GOBACK.