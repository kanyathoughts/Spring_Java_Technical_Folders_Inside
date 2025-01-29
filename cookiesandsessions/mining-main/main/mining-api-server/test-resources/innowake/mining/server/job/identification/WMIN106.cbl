       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 FIELD1                           PIC X(8).
       01 WSNEXTNAME.
           02 AAAAA.
             03 BBBBB.
               04 CCCCC.
                 05 DADF               PIC X(8).
                 05 DDDDDD             PIC X(8).
               04 CCC.
                 05 DADSF               PIC X(8).
                 05 DDDDSDD             PIC X(8).
               04 DEF REDEFINES CCC.
                  05 BDBDB PIC X(16).

       01 PNAME                        PIC X(8).
       01 L88PARENT                PIC X(8).
             88 L88FIELD                   VALUE 'XXXXX'
                                                     'ASDFDSAAS'
                                                           '-----'.
       LINKAGE SECTION.
       PROCEDURE DIVISION USING DFHCOMMAREA .
           MOVE WSNEXTNAME                       TO
                L88PARENT
           IF L88FIELD
             MOVE DDDDDD                     TO
                  PNAME
             MOVE FIELD1                 TO
                  CCC
             MOVE '1' TO FIELD1
           END-IF
           EXIT .
