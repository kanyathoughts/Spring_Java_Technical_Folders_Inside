        IDENTIFICATION DIVISION.
        PROGRAM-ID IMSDBRW01.

        DATA DIVISION.

        WORKING-STORAGE SECTION.

        01 SWITCHES.
           05 END-OF-DATA-BASE-SW      PIC X VALUE 'N'.
               88 END-OF-DATA-BASE           VALUE 'Y'.
        01 ERRORS.
           05 FILLER PIC X(42) VALUE
                'INV2100 I 1 DATA BASE ERROR - STATUS CODE '.
           05 WS-STATUS-CODE PIC XX.
        01 OUT-REC PIC X(80).

        COPY DLIFUNCT.

        01 SEGMENT-I-O-AREA           PIC X(1000).

        COPY SEGS0001.
        COPY SSAS0001.

        LINKAGE SECTION.

        COPY INVENPCB.

        PROCEDURE DIVISION.
           DISPLAY 'HELLO IMS'.
           ENTRY 'DLITCBL' USING INVENTORY-PCB-MASK.

        000-PREPARE-INV-AVAIL-REPORT.

           PERFORM 110-GET-INVENTORY-SEGMENT
             UNTIL END-OF-DATA-BASE.

           GOBACK.

        110-GET-INVENTORY-SEGMENT.

          CALL 'CBLTDLI' USING
            DLI-GN
            INVENTORY-PCB-MASK
            SEGMENT-I-O-AREA
            LOCATION-SSA-UNQUALIFIED.

            MOVE SEGMENT-I-O-AREA TO INVENTORY-STOCK-LOC-SEGMENT.
          IF IPCB-STATUS-CODE = 'GB'
            MOVE 'Y' TO END-OF-DATA-BASE-SW
          ELSE
            IF     IPCB-STATUS-CODE NOT = 'GA'
               AND IPCB-STATUS-CODE NOT = SPACE
                   MOVE 'Y' TO END-OF-DATA-BASE-SW
                   MOVE IPCB-STATUS-CODE TO WS-STATUS-CODE
            ELSE
                STRING ISLS-LOCATION,
                     ISLS-QUANTITY-ON-HAND,
                     ISLS-REORDER-POINT,
                     ISLS-QUANTITY-ON-ORDER,
                     ISLS-LAST-REORDER-DATE
                     DELIMITED BY ' / '
                     INTO OUT-REC.
