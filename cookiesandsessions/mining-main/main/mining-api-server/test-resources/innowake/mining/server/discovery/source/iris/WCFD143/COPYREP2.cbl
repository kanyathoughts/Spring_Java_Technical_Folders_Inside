000010 ID  DIVISION.
000020 PROGRAM-ID.             COPYREP2.
           COPY PAYLIB2 REPLACING ==:TAG:== BY ==Payroll==.
           COPY PAYLIB REPLACING ==(01)== BY ==(01)==
                                 == 01 == BY == 05 ==.
