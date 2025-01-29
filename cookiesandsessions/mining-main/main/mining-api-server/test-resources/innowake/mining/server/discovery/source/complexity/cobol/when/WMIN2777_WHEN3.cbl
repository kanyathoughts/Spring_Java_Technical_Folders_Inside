       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN2777_WHEN3.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
           PARA-A.
           EVALUATE FIELD-3
              WHEN 8
                 DISPLAY 'TRUE'
              WHEN OTHER
                 DISPLAY 'OTHER'
            DISPLAY 'IN PARA-A'
            GO TO PARA-B DEPENDING ON CONDITION.
           END-EVALUATE.
           PARA-B.
            GO TO PARA-B
            DISPLAY 'IN PARA-B'.
           PARA-C.
            DISPLAY 'IN PARA-C'
       END PROGRAM WMIN2777K.