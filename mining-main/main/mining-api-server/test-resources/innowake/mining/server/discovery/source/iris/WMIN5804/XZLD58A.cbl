       IDENTIFICATION DIVISION.
*#01#* PROGRAM-ID.    XZLD58A.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
*#05#  PROCEDURE DIVISION USING DFHEIBLK
*#05#                           DFHCOMMAREA
*#05#                           GUI-AREA
*#05#                           SMSGS01A
*#05#                           SKONT01A.
           IF   EF10BERART OF CA-EFSATZ NOT = 'T'
                MOVE W-TEXT-LESEN           TO CA-ZL-MSG
                SET  CA-ZL-FKT-MASKE        TO TRUE
                GO TO B4500-ZURUECK
           END-IF
           MOVE SPACE                       TO W-EFSATZ
           MOVE 3                           TO ZL00TS-ITEM
           PERFORM UP-TSQ-LESEN-ERF
           MOVE W-EFSATZ                    TO CA-EFSATZ-TR
           IF   CA-EFSATZ-TR = SPACE
                PERFORM UP-VORBELEGEN-EF25
                   THRU UP-VORBELEGEN-EF25-EX
           END-IF
           GO TO B3000-SEND-MAPS
           .
       A3000-FOLGEAUFRUF.
*#DDA#     IF  S-ZL-SPERR-KZ = 'J'
*#DDA#         IF  S-ZL-SPERR-SB = S-USER
*#DDA#          AND S-ZL-SPERR-TERMID = EIBTRMID
*#DDA#            MOVE 'E'            TO W-GESPERRT
*#DDA#         ELSE
*#DDA#            MOVE 'J'            TO W-GESPERRT
*#DDA#         END-IF
*#DDA#     ELSE
*#DDA#         MOVE SPACE             TO W-GESPERRT
*#DDA#     END-IF
SCHC       MOVE S-ZL-FREI (1:1)  TO S-EF10-RENTBEGINN
           EVALUATE TRUE
           WHEN EIBAID = DFHENTER
            AND (CA-PSS2 = 'N' OR 'A')
      D          DISPLAY 'Alle Seiten verarbeiten'
           IF   EF10KEZRENBETX     OF CA-EFSATZ > SPACE
           MOVE EF10KEZRENBETH     OF CA-EFSATZ   TO W-MERK-M21KRB
           ELSE
             MOVE ZERO                            TO W-MERK-M21KRB
           END-IF
           IF   EF10KEZRENBETOSTX  OF CA-EFSATZ > SPACE
           MOVE EF10KEZRENBETOSTH  OF CA-EFSATZ   TO W-MERK-M21KRBO
           ELSE
             MOVE ZERO                            TO W-MERK-M21KRBO
           END-IF
           IF   EF10RVGESAMTX      OF CA-EFSATZ > SPACE
           MOVE EF10RVGESAMTH      OF CA-EFSATZ   TO W-MERK-M22RVG
           ELSE
             MOVE ZERO                            TO W-MERK-M22RVG
           END-IF
           IF   EF10RVGESAMTOSTX   OF CA-EFSATZ > SPACE
           MOVE EF10RVGESAMTOSTH   OF CA-EFSATZ   TO W-MERK-M22RVGO
           ELSE
             MOVE ZERO                            TO W-MERK-M22RVGO
           END-IF
      D     DISPLAY 'XZLD58A: W-MERK-Felder versorgt'
*#DDA#          PERFORM VARYING CA-RT-SEITE
*#DDA#            FROM 20 BY 1 UNTIL CA-RT-SEITE > 25
*#DDA#              PERFORM B2000-RECEIVE-MAPS-ANF
*#DDA#                 THRU B2999-RECEIVE-MAPS-END
*#DDA#          END-PERFORM
SCHC            IF S-EF10-RENTBEG-BERRECH
SCHC              SET S-EF10-RENTBEG-NEIN TO TRUE
SCHC              MOVE S-EF10-RENTBEGINN  TO S-ZL-FREI (1:1)
SCHC            END-IF
                PERFORM F2200-SV-RENTE-BERECHNEN-ANF
                   THRU F2200-SV-RENTE-BERECHNEN-END
                PERFORM F2300-R-STEIG-BERECHNEN-ANF
                   THRU F2300-R-STEIG-BERECHNEN-END
                IF   ZR0060ABBRUCH = 'J'
                     MOVE 26210103          TO W-RC
                     GO TO U1000-FEHLER
                END-IF
                PERFORM B1000-VERARBEITEN
                GO TO B3000-SEND-MAPS
           WHEN EIBAID = DFHPF3
            AND (CA-PSS2 = SPACE OR 'N')
                SET  CA-ZL-FKT-MASKE        TO TRUE
                GO TO B4500-ZURUECK
           WHEN EIBAID = DFHPF3
            AND CA-PSS2 = 'A'
                MOVE SPACE                  TO CA-PSS2
                MOVE 1                      TO ZL00TS-ITEM
                PERFORM UP-TSQ-LESEN-ERF
                MOVE W-EFSATZ               TO CA-EFSATZ
                MOVE 3                      TO ZL00TS-ITEM
                PERFORM UP-TSQ-LESEN-ERF
                MOVE W-EFSATZ               TO CA-EFSATZ-TR
                GO TO B3000-SEND-MAPS
           WHEN EIBAID = DFHPF3
                MOVE SPACE                  TO CA-PSS2
                GO TO B3000-SEND-MAPS
           WHEN EIBAID  = DFHPF4
            AND CA-PSS2 = SPACE
*#DDA#      AND GESPERRT
                MOVE S-ZL-SPERR-SB         TO W-INARB-SB
                MOVE S-ZL-SPERR-TERMID     TO W-INARB-TERMID
                MOVE W-INARB               TO CA-FE-MELDUNG
                GO TO U1200-FEHLERTEXT-SENDEN
           WHEN EIBAID  = DFHPF4
            AND CA-PSS2 = SPACE
            AND CA-ZUAEN = 'J'
                MOVE 'A'                    TO CA-PSS2
                GO TO B3000-SEND-MAPS
           WHEN EIBAID  = DFHPF4
            AND CA-PSS2 = SPACE
                MOVE 9013                   TO W-RC
                GO TO U1000-FEHLER
           WHEN EIBAID  = DFHPF12
            AND CA-PSS2 = SPACE
            AND GESPERRT
                MOVE S-ZL-SPERR-SB         TO W-INARB-SB
                MOVE S-ZL-SPERR-TERMID     TO W-INARB-TERMID
                MOVE W-INARB               TO CA-FE-MELDUNG
                GO TO U1200-FEHLERTEXT-SENDEN
           WHEN EIBAID  = DFHPF12
            AND CA-PSS2 = SPACE
            AND CA-ZULOE = 'J'
            AND (EF10MASCH OF CA-EFSATZ NOT = 'M'
             OR (CA-SBSCHL = 'ZD12' OR 'ZD28'))
                MOVE 'L'                    TO CA-PSS2
                GO TO B3000-SEND-MAPS
           WHEN EIBAID  = DFHPF12
            AND CA-PSS2 = SPACE
            AND CA-ZULOE = 'J'
                MOVE W-TEXT-LOESCHEN        TO W-TEXT3-V1
                MOVE W-TEXT3                TO W-MSG1
                GO TO B3000-SEND-MAPS
           WHEN EIBAID  = DFHPF12
            AND CA-PSS2 = SPACE
                MOVE 9013                   TO W-RC
                GO TO U1000-FEHLER
           WHEN EIBAID = DFHENTER
            AND CA-PSS2 = 'L'
                PERFORM B1000-VERARBEITEN
                GO TO B3000-SEND-MAPS
           WHEN EIBAID = DFHENTER
                GO TO B1500-RECEIVE-MAP
           END-EVALUATE
           MOVE 3                           TO W-RC.
           GO TO U1000-FEHLER
           .
       F2300-R-STEIG-BERECHNEN-END.
           EXIT.
       U1000-FEHLER.
           MOVE W-RC                   TO CA-FE-NUMMER.
       U1100-FEHLER.
*#27#      MOVE CA-FE-NUMMER    TO  ALD800-RC  OF ALD800-PAR
*#27#      MOVE CA-FE-MELDUNG   TO  ALD800-MSG OF ALD800-PAR
*#27#      MOVE K-PROGNAME      TO  ALD800-DD  OF ALD800-PAR
*#27#      CALL LP-FEHLER USING     ALD800-PAR
*#27#                               SMSGS01A
           .
       U1200-FEHLERTEXT-SENDEN.
*#DDA#D    EXEC CICS ENTER TRACEID (099)
*#DDA#D    END-EXEC
*#DDA#D    DISPLAY 'XZLD58A U1200 W-RC' W-RC ' '
*#DDA#D                    CA-FE-NUMMER ' ' CA-FE-MELDUNG
*#DDA#     MOVE CA-FE-NUMMER    TO  ALD800-RC  OF ALD800-PAR
*#DDA#     MOVE CA-FE-MELDUNG   TO  ALD800-MSG OF ALD800-PAR
*#DDA#     MOVE K-PROGNAME      TO  ALD800-DD  OF ALD800-PAR
*#DDA#     CALL LP-FEHLER USING     ALD800-PAR
*#DDA#                              SMSGS01A
*#DDA#     IF CA-FE-NUMMER = LOW-VALUE
*#DDA#          SET  M-MSG-TYP-INFO     TO TRUE
*#DDA#          MOVE CA-FE-MELDUNG      TO M-MSG
*#DDA#          MOVE 9999               TO M-MSG-NR
*#DDA#     END-IF
*#DDA#D    DISPLAY 'XZLD58A (N)' CA-FE-NUMMER ' ' M-MSG-NR ' ' M-MSG

           IF   W-RC-PLAUSI-FEHLER
                PERFORM V0000-CURSOR-ANF
                   THRU V9999-CURSOR-END
           END-IF
           MOVE CA-EFSATZ-TR           TO ERFASSUNG
           MOVE ZEROES                 TO W-RC
*#13#      GOBACK
           .
       V0000-CURSOR-ANF.
           EVALUATE TRUE
           WHEN CA-RT-SEITE = 20
                GO TO V2000-MAP-20
           WHEN CA-RT-SEITE = 21
                GO TO V2100-MAP-21
           WHEN CA-RT-SEITE = 22
                GO TO V2200-MAP-22
           WHEN CA-RT-SEITE = 23
                GO TO V2300-MAP-23
           WHEN CA-RT-SEITE = 24
                GO TO V2400-MAP-24
           WHEN CA-RT-SEITE = 25
                GO TO V2500-MAP-25
           END-EVALUATE
           .
       UP-F5V2.
           MOVE W-F5V2-N                 TO H-FELD-F0.
           MOVE H-FELD-F0                TO W-F5V2-N.
           .

      *--------------------------------------------------------------*
      *--------------------- ENDE DPRO KZLDT20  ---------------------*
      *--------------------------------------------------------------*

      *    ************************************************************
      *    ************************************************************
