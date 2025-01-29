       01  :TAG:.
        02  :TAG:-WEEK          PIC S99.
        02  :TAG:-GROSS-PAY     PIC S9(5)V99.
        02  :TAG:-HOURS         PIC S999  OCCURS 1 TO 52 TIMES
                 DEPENDING ON :TAG:-WEEK OF :TAG:.
