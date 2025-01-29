       IDENTIFICATION DIVISION.
       PROGRAM-ID. GroupGraphNodes.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 NODE-A PIC 9(4).
       77 NODE-B PIC 9(4).
       77 NODE-C PIC 9(4).
       77 NODE-D PIC 9(4).
       77 NODE-E PIC 9(4).
       PROCEDURE DIVISION.
        MOVE 1 TO NODE-A.
        MOVE 10 TO NODE-B.
        ADD NODE-A TO NODE-B GIVING NODE-C.
        COMPUTE NODE-D = NODE-A + NODE-B.
        IF NODE-C == NODE-D  THEN
           DISPLAY 'NODE-C EQUALS NODE-D'
        ELSE
           MOVE NODE-A TO NODE-E
           MOVE NODE-C TO NODE-A
           MOVE NODE-E TO NODE-C
        END-IF.
       STOP RUN.
