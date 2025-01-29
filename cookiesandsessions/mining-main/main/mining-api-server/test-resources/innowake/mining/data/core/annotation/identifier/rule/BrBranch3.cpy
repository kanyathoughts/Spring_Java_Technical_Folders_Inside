           EVALUATE TRUE
             WHEN A 
                DISPLAY '1'
             WHEN OTHER
                EVALUATE TRUE
                   WHEN A 
                      DISPLAY '1'
                   WHEN OTHER
                      DISPLAY 'OTHER'
                END-EVALUATE.
           END-EVALUATE.
