
        01 CUSTOMER-SSA-UNQUALIFIED.
           05 SEGMENT-NAME    PIC X(008) VALUE 'CUSTROOT'.
           05 FILLER          PIC X(992) VALUE SPACE.

        01 CUSTOMER-SSA-QUALIFIED.
           05 FILLER          PIC X(009)  VALUE 'CUSTROOT('.
           05 FILLER          PIC X(010)  VALUE 'CUSACTNO ='.
           05 CUSTNO          PIC X(008).
           05 FILLER          PIC X(001)  VALUE ')'.
           05 FILLER          PIC X(972) VALUE SPACE.

        01 LOAN-SSA-UNQUALIFIED.
           05 SEGMENT-NAME    PIC X(008) VALUE 'LOANSEGM'.
           05 FILLER          PIC X(992) VALUE SPACE.

        01 LOAN-SSA-QUALIFIED.
           05 FILLER          PIC X(009)  VALUE 'LOANSEGM('.
           05 FILLER          PIC X(008)  VALUE 'LOANNO ='.
           05 LOANNO          PIC X(002).
           05 FILLER          PIC X(001)  VALUE ')'.
           05 FILLER          PIC X(980) VALUE SPACE.
