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
       
           EVALUATE
      * Comment 1
                    TRUE
             WHEN
      * Comment 2
                  A AND B
                DISPLAY 'A&B'
             WHEN
      * Comment 3
                  A OR B
                DISPLAY 'A|B'
             WHEN
      * Comment 4
                  OTHER
                DISPLAY 'OTHER'
           END-EVALUATE.

           EVALUATE
      * Comment 5
                    A
      * Comment 6
             WHEN
      * Comment 7
                  5
               DISPLAY '5'
             WHEN
      * Comment 8
                  42
               DISPLAY '42'
           END-EVALUATE.
           
           EVALUATE
      * Comment 9
                    TRUE
      * Comment 10
                         ALSO
      * Comment 11
                              A
             WHEN
      * Comment 12
                  B = 4
      * Comment 13
                        ALSO 2
                DISPLAY 'WHAT'
             WHEN 
      * Comment 14
                  A AND B ALSO 42
                DISPLAY 'IS THIS'
           END-EVALUATE.

           GOBACK.