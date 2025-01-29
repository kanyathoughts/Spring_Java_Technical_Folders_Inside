        IDENTIFICATION DIVISION.
        PROGRAM-ID TESTA.

        DATA DIVISION.
        WORKING-STORAGE SECTION.

        01 SWITCHES.
           05 END-OF-DATA-BASE-SW      PIC X VALUE 'N'.
               88 END-OF-DATA-BASE           VALUE 'Y'.
        01 ERRORS.
           05 FILLER PIC X(42) VALUE
                'INV2100 I 1 DATA BASE ERROR - STATUS CODE '.
           05 WS-STATUS-CODE PIC XX.

        01 DLI-FUNCTIONS.
           05 DLI-GU                 PIC X(4) VALUE 'GU  '.
           05 DLI-GHU                PIC X(4) VALUE 'GHU '.
           05 DLI-GN                 PIC X(4) VALUE 'GN  '.
           05 DLI-GHN                PIC X(4) VALUE 'GHN '.
           05 DLI-GNP                PIC X(4) VALUE 'GNP '.
           05 DLI-GHNP               PIC X(4) VALUE 'GHNP'.
           05 DLI-ISRT               PIC X(4) VALUE 'ISRT'.
           05 DLI-DLET               PIC X(4) VALUE 'DLET'.
           05 DLI-REPL               PIC X(4) VALUE 'REPL'.
           05 DLI-CHKP               PIC X(4) VALUE 'CHKP'.
           05 DLI-XRST               PIC X(4) VALUE 'XRST'.
           05 DLI-PCB                PIC X(4) VALUE 'PCB '.

        01 INVENTORY-VENDOR-SEGMENT.
           05 IVS-VENDOR-CODE        PIC X(3).
           05 IVS-VENDOR-NAME        PIC X(30).
           05 IVS-VENDOR-ADDRESS     PIC X(30).
           05 IVS-VENDOR-CITY        PIC X(17).
           05 IVS-VENDOR-STATE       PIC XX.
           05 IVS-VENDOR-ZIP-CODE    PIC X(9).
           05 IVS-VENDOR-TELEPHONE   PIC X(10).
           05 IVS-VENDOR-CONTACT     PIC X(30).

        01 INVENTORY-ITEM-SEGMENT.
           05 IIS-NUMBER             PIC X(5).
           05 IIS-DESCRIPTION        PIC X(35).
           05 IIS-UNIT-PRICE         PIC S9(5)V99 COMP-3.
           05 IIS-AVG-UNIT-COST      PIC S9(5)V99 COMP-3.

        01 VENDOR-SSA-QUALIFIED.
           05 FILLER          PIC X(009)  VALUE 'INVENSEG('.
           05 FILLER          PIC X(010)  VALUE 'INVENCOD ='.
           05 VENDOR-CODE     PIC X(003).
           05 FILLER          PIC X(001)  VALUE ')'.
           05 FILLER          PIC X(977) VALUE SPACE.

        01 VENDOR-SSA-UNQUALIFIED.
           05 SEGMENT-NAME    PIC X(008) VALUE 'INVENSEG'.
           05 FILLER          PIC X(992) VALUE SPACE.

        01 ITEM-SSA-UNQUALIFIED.
           05 SEGMENT-NAME    PIC X(008) VALUE 'INITMSEG'.
           05 FILLER          PIC X(992) VALUE SPACE.

        01 ITEM-SSA-QUALIFIED.
           05 FILLER          PIC X(009)  VALUE 'INITMSEG('.
           05 FILLER          PIC X(010)  VALUE 'INITMNUM ='.
           05 ITEM-CODE       PIC X(005).
           05 FILLER          PIC X(001)  VALUE ')'.
           05 FILLER          PIC X(975) VALUE SPACE.

        01 SEGMENT-I-O-AREA           PIC X(1000).


        LINKAGE SECTION.

        01 INVENTORY-PCB-MASK.
           05 IPCB-DBD-NAME          PIC X(8).
           05 IPCB-SEGMENT-LEVEL     PIC XX.
           05 IPCB-STATUS-CODE       PIC XX.
           05 IPCB-PROC-OPTIONS      PIC X(4).
           05 FILLER                 PIC S9(5) COMP.
           05 IPCB-SEGMENT-NAME      PIC X(8).
           05 IPCB-KEY-LENGTH        PIC S9(5) COMP.
           05 IPCB-NUMB-SENS-SEGS    PIC S9(5) COMP.
           05 IPCB-KEY               PIC X(11).

        01 IO-PCB.
           05 LOGICAL-TERMINAL-NAME          PIC XXXXXXXX.
           05 RESERVED1                      PIC XX.
           05 STATUS-CODE                    PIC XX.
           05 LOCAL-DATE                     PIC S9(9) COMP-5.
           05 LOCAL-TIME                     PIC S9(9) COMP-5.
           05 INPUT-MESSAGE-SEQUENCE-NUMBER  PIC S9(9) COMP-5.
           05 MESSAGE-OUTPUT-DESCRIPTOR-NAME PIC XXXXXXXX.
           05 USER-ID                        PIC XXXXXXXX.
           05 GROUP-NAME                     PIC XXXXXXXX.
           05 INTERNAL-TIME-STAMP            PIC XXXXXXXXXXXX.
           05 USER-ID-INDICATOR              PIC X.
           05 RESERVED2                      PIC XXX.

        PROCEDURE DIVISION USING IO-PCB INVENTORY-PCB-MASK.
           PERFORM 110-GET-INVENTORY-SEGMENT UNTIL END-OF-DATA-BASE.
           GOBACK.

        110-GET-INVENTORY-SEGMENT.

           CALL 'CBLTDLI' USING
             DLI-GHN
             INVENTORY-PCB-MASK
             SEGMENT-I-O-AREA
             VENDOR-SSA-UNQUALIFIED.
