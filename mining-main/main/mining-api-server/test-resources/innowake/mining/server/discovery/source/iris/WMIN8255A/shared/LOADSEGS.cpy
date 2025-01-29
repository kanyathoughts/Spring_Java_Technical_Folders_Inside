        01 CUSTOMER-SEGMENT.
           05 CUSTOMER-NUMBER          PIC X(8).
           05 CUSTOMER-NAME            PIC X(15).
           05 CUSTOMER-ADDRESS         PIC X(40).
           05 CUSTOMER-BLANK           PIC X(35).

        01 LOAN-SEGMENT.
           05 LOAN-NUMBER              PIC X(2).
           05 LOAN-TYPE                PIC X(1).
           05 LOAN-INFO                PIC X(29).
           05 LOAN-AMOUNT              PIC S9(15) COMP-3.
           05 LOAN-DATE                PIC X(8).
           05 LOAN-MATU                PIC X(8).
           05 LOAN-RATE                PIC S9(3) COMP-3.
           05 LOAN-PAY                 PIC S9(11) COMP-3.
           05 LOAN-BALANCE             PIC S9(15) COMP-3.
           05 LOAN-INTEREST-TO-DATE    PIC S9(11) COMP-3.
           05 LOAN-DAY-LAST-PAYMENT    PIC X(7).
           05 LOAN-BLANK               PIC X(7).
