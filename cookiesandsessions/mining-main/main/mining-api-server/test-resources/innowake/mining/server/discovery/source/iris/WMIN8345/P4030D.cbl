       IDENTIFICATION DIVISION.
       PROGRAM-ID. P4030D.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 DLI-FUNCTIONS.
          05 DLI-GN        PIC X(4)    VALUE 'GN  '.
          05 DLI-GHN       PIC X(4)    VALUE 'GHN '.
       01  SEGMENT-I-O-AREA        PIC X(150).
       LINKAGE SECTION.
       01  STUDENT-PCB-MASK.
           05 STD-DBD-NAME              PIC X(8).
           05 STD-SEGMENT-LEVEL         PIC XX.
           05 STD-STATUS-CODE           PIC XX.
           05 STD-PROC-OPTIONS          PIC X(4).
           05 FILLER                    PIC S9(5) COMP.
           05 STD-SEGMENT-NAME          PIC X(8).
           05 STD-KEY-LENGTH            PIC S9(5) COMP.
           05 STD-NUMB-SENS-SEGS        PIC S9(5) COMP.
           05 STD-KEY                   PIC X(11).
       PROCEDURE DIVISION.
            ENTRY 'DLITCBL' USING STUDENT-PCB-MASK.
       A000-READ-PARA.
       110-GET-INVENTORY-SEGMENT.
              CALL ‘CBLTDLI’ USING DLI-GN
                                   STUDENT-PCB-MASK
                                   SEGMENT-I-O-AREA.
            GOBACK.