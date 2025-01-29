***********************************************************************
************************************************************************
* Subroutine PPPFT2ZS
*
* in Projekt: ZAPB
*
************************************************************************
* Allgemeines
************************************************************************
* Beschreibung:
*
* Subroutine zur Erstellung der Anordnungsbestände, der Sammelanordnung
* für neue Auszahlung "BKT" ----> Elster FT
* ----------------------------------------------------------------------
* -------------------  A U S Z A H L U N G E N  ------------------------
* ----------------------------------------------------------------------
*
* Die Subroutine wird aufgerufen vom Auswahlprogramm ZAPAZ2ZP, daß vom
* ZAP Auswahlpunkt 1 => "reguläre zentrale Auszahlung starten" aufge-
* rufen wird
*
**----------------------------------------------------------------------
* Zuständigkeit, Ansprechpartner, Vertreter
*
* Hainzinger
**----------------------------------------------------------------------
* Bemerkungen:
*
************************************************************************
* Schnittstellen
************************************************************************
* Input:
* DB2 ! Work File ! Parameter ! Glob. Var.! Screen ! Stack !
*  X  !           !           !           !        !       !
**----------------------------------------------------------------------
* Output:
* DB2 ! Work File ! Parameter ! Glob.Var. ! Screen ! Stack ! Print File!
*  X  !     X     !           !           !        !       !     X     !
************************************************************************
* Verwendung:
************************************************************************
* Aufruf:
*
**----------------------------------------------------------------------
* Parameter:                                           Typ   Read/Write
*
**----------------------------------------------------------------------
* Programmreturn:  KEINER
************************************************************************
* Modulkonstruktion
************************************************************************
* Variablen:
*
**----------------------------------------------------------------------
* Programmablauf
*
**----------------------------------------------------------------------
* Fehlerroutine/Fehlertransaktion
*
************************************************************************
* Historie:
************************************************************************
* Version ! Datum     ! User ! Grund/Art der Änderung
**---------------------------------------------------------------------
* 2/01    ! Mai  2005 ! MLHI ! Neuprogrammierung Elster FT
* 3       ! Okt. 2009 / MLHI / Aufnahme EWB in Auszahlung
* 3/01    ! Nov. 2009 ! MLHI ! Anpassung an DZP 2009-Auszahlung
**---------------------------------------------------------------------
************************************************************************
************************************************************************
* Datendefinition
************************************************************************
DEFINE DATA
PARAMETER USING ZAPAZ2-A
LOCAL     USING #BDAT04A
***LOCAL     USING PFAEN--L     ALT
LOCAL     USING PFAEN06L
LOCAL     USING AUFRE--L
LOCAL     USING PPPFT2-L
LOCAL     USING PPPFT2DL
LOCAL     USING PPPFT2CL
LOCAL     USING ZAPPRF-L
LOCAL     USING #DATPRUA /* DATUMS-Prüfung
LOCAL
1 #N2               (N2)
1 #BESCHART         (I2)
1 #BESCHART_IND     (I2)
1 #N_BETRNR         (N10)
1 #DB2_BKZ          (A15)
1 #AUFRECH_IN_PFAEN (I2)
1 #SONST_IN_PFAEN   (I2)
1 #SUM_AUFRECH      (P7,2)
1 #AN_LANDWIRT      (P7,2)
1 #KEINE_AUFRECH    (I2)
** Feld zur Ermittlung des VOLSER-Namens
1 #PROJ_JJ_BEZ      (A7)
** Übergabeparameter für Subroutine C_SATZ_SCHREIBEN
1 #U_PFAEND         (I2)
1 #U_NAME           (A30)
1 #L_NAME           (A30)
1 #U_BLZ            (P8)
1 #U_KONTO          (A10)
1 #U_BANKNAME       (A27)
** Felder zur Doppelanzeige für teilfinanzierte Massnahmen wärend der
** Übergangszeit (für den Datenträgerbegleitzettel
1 #H_FELD           (P10,6)
1 #SUM_EURO_IN_DM   (P10,2)
1 #H_DAT_8          (A8)
1 #N_DAT_8          (N8)
** Felder für Modulation
1 #U                (I2)
1 #V                (I2)
1 #W                (I2)
1 #X                (I2)
1 #Y                (I2)
1 #MAX_HHL          (I2)
1 #MLKG             (I2)
*
1 #HHL_BEZUG        (I2)
*
1 #F12B_BETRAG     (P10,2)
1 #SATZ2_ANZ       (P6)
1 #SATZ2_BETRAG    (P10,2)
*
1 #NEUE_BANDKENN   (I2)
1 #N_NEUE_BANDKENN (N4)
1 REDEFINE #N_NEUE_BANDKENN
2 #A_NEUE_BANDKENN (A4)
1 #NUM             (I2)
1 #TREFF_BANDKENN  (I2)
*
1 #MOSA_TYP        (A2)
1 #PROJEKT_UR      (A3)
1 #DB2_MOSA        (A2)
1 #SUM_MOSA        (P7,2)
1 #SUM_MOSA_S      (P7,2)
1 #SUM_MOSA_M      (P7,2)
1 #SUM_MO          (P7,2)
1 #SUM_SA          (P7,2)
1 #ANZ_MOSA        (I2)
1 #SUMME_MOSA      (P7,2)
1 #SUM_BN_MOSA     (P7,2)
1 #GES_BN_MOSA     (P7,2)
1 #HHL_SA          (I2)
1 #BUND_SA         (A10)
1 #A10             (A10)
1 #SATZZR_BETRAG   (P10,2)
1 #GRUPPE          (I2)
1 #GRUPPE_ZAEHL    (I2)
1 #SATZANZ         (P6)
1 #BEZUG_ZK        (P6)
1 #ZR_HILF         (A35)
1 REDEFINE #ZR_HILF
2 #ZR_KENNZ_E_A    (A1)
2 #ZR_HAUSHALT     (N10)
2 #ZR_OBJEKT       (N10)
2 #ZR_BETRAG       (A14)
1 #BAND_AUSZ       (P10,2)
1 #POS_BETRAG      (P10,2)
1 #SUMME_E_FUER_9  (P10,2)
1 #BUCH_BAND_KENN  (A8)
1 #ANZ_MAX_AUSZ    (I2)
1 #ANZ_MAX_BUCHSATZ(I2)
1 #WERT1           (A1)
1 #WERT2           (A1)
****************************************
* Hilfsfelder zum Aufaddieren für E-Satz
****************************************
1 #H_SUM_CSATZ          (P7,0)
1 #H_SUM_KONTO          (P15,0)
1 #H_SUM_BLZ            (P15,0)
1 #H_SUMME_IN_EURO_BAND (P11,2)
1 #N_ERWEIT             (P2)
1 #NEU_ERWEIT           (P2)
1 #GESAMTSATZLAENGE     (P4)
1 #H_BETRAG_EURO        (P11,2)
1 #INSGESAMT            (P11,2)
*
1 #GES_ZAHL             (N6)
1 #GES                  (P10,2)
1 #N1_MINUS             (P10,2)
1 #N1                   (P10,2)
1 #SUMME_SA             (P10,2)
1 #SUMME_MO             (P10,2)
1 #A1                   (A14)
1 #A2                   (A7)
1 #A61                  (A61)
1 #K_DATUM              (A10)
*
1 #H107_STRICH          (A107)
1 #H131_TEXT            (A131)
1 #H18                  (A18)
1 #H20                  (A20)
1 #H37                  (A37)
1 #H32                  (A32)
1 #H70                  (A70)
1 #A13                  (A13)
1 #A12                  (A12)
1 #A16                  (A16)
1 #AUS_WERTE_V13        (A13)
1 #BN_COUNTER           (P6,0)
1 #L_BLZ                (N8)
1 #H_BLZ                (A8)
1 #KENNUNG              (A4)
1 #ZZ                   (I2)
1 #AUFSTELLUNG          (A16/1:4)
1 #AUFTEIL_ZAEHL        (I2)
1 #AUFTEIL_OBJ          (A8/1:60)
1 #AUFTEIL_BETR         (P11,2/1:60)
1 #AUFTEIL_ZF           (P6/1:60)
1 #EINZEL_BETRAG        (P10,2)
1 #EINZEL_ZAEHL         (I2)
*
1 #NAME_BUCH_DATEI      (A40)
1 #NAME_BAND_DATEI      (A40)
*
1 #SA_OBJ_KONTO         (A8)
1 #SA_BUND_STELLE       (A10)
*
1 #VAIF_HHL             (I2)
*
1 #AUFRE_ZZ             (I2)
*
1 #ABTRET_ZZ            (I2)
*
1 #RESTBETRAG_NACH_AUFRE (P9,2)
*
1 #A_BLZ            (N9)
1 #A_KONTO          (A10)
1 #A_BANKNAME       (A27)
1 #ANZ_BIW_JAHR     (I4)
1 #COUNT-PFAEN      (I4)
1 #RC_A             (I4)
*
1 #P                (I2)
1 #HHL_BEZ_TAB      (I2/1:3)
*
1 #IDENT_MJSAGRUND  (I4)
1 #SUM_MJSA         (P7,2)
*
1 #KASSE_A8         (A8)
* Zähler für zweifaches Ausdrucken des F13 !!!!!
1 #F13_COUNT        (I2)
END-DEFINE
*
DEFINE SUBROUTINE PPPFT2ZS
*
**-----------------------------------------------------------------
* Ausfuer-Datum auf Gültigkeit prüfen, sonst Abbruch
**-----------------------------------------------------------------
#FORM := 5
CALLNAT '#DATPRUN'
         #P_BEREICH1.#AUSFUER_DAT #DATUM_AUS #FORM #FC #FM
IF #FC ^= 0
  #N_ERROR_NR := *ERROR-NR
*
  *ERROR-TA := 'FOEERR-P'
  #PGM      := *PROGRAM
  COMPRESS 'Ungültiges Ausführungsdatum'
      INTO #CODE
  CALLNAT 'FOEERR-N' #PGM FOES.BETRNR #CODE
  ESCAPE ROUTINE
END-IF
*
RESET #SUM_EURO_IN_DM
*
************************************************************************
* Löschen benötigter Felder
************************************************************************
RESET #A_STRUKTUR #C_STRUKTUR #E_STRUKTUR
RESET #1_STRUKTUR #2_STRUKTUR #9_STRUKTUR
RESET #ZK_STRUKTUR #ZR_STRUKTUR #ZL_STRUKTUR
RESET #C_ERWEIT_TAB(*)
RESET #BEZUG
RESET #LFD_NR #LFD_NR_ZL
RESET #BEZUG_ZK
RESET #BN_ZAEHL
RESET #PR_ABRECH
RESET #BN_STRUK(*) #HHL_STRUK(*) #HHL_STRUK_SWAP #HHL_NEG_STRUK(*)
RESET #MOSA_STRUK(*,*,*)
RESET #SUM_ABRECH #SUM_BAY_ABRECH
RESET #PRUEF_STRUK
RESET #SUMME_51000
RESET #SUMME_54400
RESET #SUMME_53100
RESET #SUMME_E_FUER_9
RESET #H_SUM_CSATZ
RESET #H_SUM_KONTO
RESET #H_SUM_BLZ
RESET #H_SUMME_IN_EURO_BAND
RESET #GES #GES_ZAHL
RESET #BN_COUNTER
RESET #AUFTEIL_ZAEHL
RESET #AUFTEIL_OBJ(*)
RESET #AUFTEIL_BETR(*)
RESET #AUFTEIL_ZF(*)
************************************************************************
* Ermittlung des Timestamps bei Programmstart
************************************************************************
CALLNAT '#TS----N' #PR_TS_B
************************************************************************
* Übertragen Parameter in DB2- und Prüfvariablen
************************************************************************
#PR_MODUS    := #P_BEREICH1.#MODUS
#PR_JAHR     := #P_BEREICH1.#JAHR
#PR_PROJEKT  := #P_BEREICH1.#PROJEKT
#PR_ZAP_DAT  := #P_BEREICH1.#ZAP_DAT
#PR_STEPNAME := 'AUSZAHLUNGSBAND'
#DB2_PROJEKT := #P_BEREICH1.#PROJEKT
#DB2_JAHR    := #P_BEREICH1.#JAHR
#DB2_ZAPDAT  := #P_BEREICH1.#ZAP_DAT
*
DECIDE ON FIRST VALUE OF #STAT_NUM
  VALUE 991,992
    #DB2_STATUS_1 := 991
    #DB2_STATUS_2 := 992
  VALUE 995
    #DB2_STATUS_1 := 995
    #DB2_STATUS_2 := 995
  VALUE 999
    #DB2_STATUS_1 := 999
    #DB2_STATUS_2 := 999
  NONE
    #DB2_STATUS_1 := 995
    #DB2_STATUS_2 := 995
END-DECIDE
#DB2_ZAP_STAT := #ZAP_STAT_NUM
*
IF #P_BEREICH1.#MODUS = 'PROD'
  #WERT1 := 'N'
  #WERT2 := 'N'
ELSE
  #WERT1 := 'N'
  #WERT2 := 'J'
END-IF
************************************************************************
* Ermittlung der Projekt-Daten
************************************************************************
RESET PROJ
SELECT * INTO VIEW PROJ FROM ZAP-PROJ
    WHERE PROJEKT  = #P_BEREICH1.#PROJEKT
END-SELECT
*
DECIDE ON FIRST VALUE OF MEHRJAEHRIG
   VALUE 'J'
      RESET #MIN_JAHR
            #MAX_JAHR
      SELECT SINGLE MIN(JAHR),MAX(JAHR)
        INTO #MIN_JAHR,#MAX_JAHR
        FROM ZAP-FOES
        WHERE PROJEKT = #DB2_PROJEKT
          AND ZAP_DAT = #DB2_ZAPDAT
          AND STATUS  = #DB2_ZAP_STAT
      END-SELECT
*
      #DB2_JAHR_V  := #MIN_JAHR
      #DB2_JAHR_B  := #MAX_JAHR
   NONE
      #DB2_JAHR_V  := #DB2_JAHR
      #DB2_JAHR_B  := #DB2_JAHR
END-DECIDE
*
************************************************************************
* Ermittlung, ob Aufrechnungen vorhanden sind
************************************************************************
RESET #AUFRE_ZZ
SELECT COUNT(*) INTO #AUFRE_ZZ FROM ZAP-AUFRE
 WHERE AUSZ_PROJ    = #DB2_PROJEKT
   AND AUFR_BETRAG  > 0
   AND IST_GESTELLT IN (#WERT1,#WERT2)
END-SELECT
*
************************************************************************
* Ermittlung der Masken-Daten der dazugehörigen Daten
************************************************************************
RESET FOESZ
SELECT SINGLE * INTO VIEW FOESZ FROM ZAP-FOESZ
    WHERE PROJEKT  = #DB2_PROJEKT
    AND JAHR     = #DB2_JAHR
    AND ZAP_DAT  = #DB2_ZAPDAT
    AND AUSZ_ART = #P_BEREICH1.#TYP
END-SELECT
*
************************************************************************
* Ermittlung der Buchungsdateikennung aus ZAP-FOESZ
************************************************************************
RESET #ANZ_MAX_AUSZ #ANZ_MAX_BUCHSATZ
SELECT SINGLE MAX(TAG_MAX_AUSZ),MAX(TAG_MAX_BUCHSATZ)
INTO #ANZ_MAX_AUSZ,#ANZ_MAX_BUCHSATZ
 FROM ZAP-FOESZ T1
 WHERE ZAP_DAT          = #DB2_ZAPDAT
   AND AUSZ_ART         IN ('Z','E')
   AND TAG_MAX_AUSZ     IS NOT NULL
   AND TAG_MAX_BUCHSATZ IS NOT NULL
END-SELECT
*
#ANZ_MAX_AUSZ     := #ANZ_MAX_AUSZ     + 1
*
MOVE EDITED #ANZ_MAX_AUSZ (EM=99)
         TO #H2
*
************************************************************************
* Umwandlung des ZAP-Datums von TT.MM.JJJJ in TTMMJJ für A-Satz
************************************************************************
#H_FI := 5
#H_FO := 3
RESET #H_DATUM
CALLNAT '#DATDREN' #P_BEREICH1.#ZAP_DAT #H_DATUM #H_FI #H_FO
#H_DAT_BAND := SUBSTR(#H_DATUM,1,6)
*
#H_FI := 5
#H_FO := 4
RESET #H_DATUM
CALLNAT '#DATDREN' #P_BEREICH1.#ZAP_DAT #H_DATUM #H_FI #H_FO
#H_DAT_6 := SUBSTR(#H_DATUM,1,6)
*
COMPRESS #H_DAT_6 #H2 INTO #BUCH_BAND_KENN LEAVING NO
*
#WAEHRUNG := 'Euro'
*
#H3_N := FOESZ.LFD_ZAHL
************************************************************************
* Zusammenbau des Magnetkassetten-Namens "PPPJJA"
************************************************************************
DECIDE ON FIRST VALUE OF JAHR_UNSPEZ
   VALUE 'J'
      COMPRESS #PROJEKT 'JJ' #UNTERNUMMER 'A'
          INTO #PROJ_JJ_BEZ LEAVING NO
   NONE
      COMPRESS #PROJEKT #J_JJ #UNTERNUMMER 'A'
          INTO #PROJ_JJ_BEZ LEAVING NO
END-DECIDE
*
************************************************************************
************************************************************************
* Zusammenbau der Belegnummer "TTMMJ 001"
************************************************************************
COMPRESS #V_TT #V_MM #V_J INTO #H5 LEAVING NO
MOVE EDITED #H3_N (EM=999) TO #H3
COMPRESS #H5 #H3 INTO #H_BELEGNR
************************************************************************
* Übetragung des Auszahlungsanlasses
************************************************************************
COMPRESS FOESZ.BUCH_TEXT1 INTO #H_ANLASS LEAVING NO
************************************************************************
* Ermittlung, ob "Rückruf vorbehalten" aus 2. Buchungstext
************************************************************************
#RUECKRUF := FALSE
IF FOESZ.BUCH_TEXT2 > ' '
  EXAMINE FOESZ.BUCH_TEXT2 AND TRANSLATE INTO UPPER CASE
  EXAMINE FULL FOESZ.BUCH_TEXT2 FOR 'RUECK' GIVING POSITION #J
  IF #J > 0
    #RUECKRUF := TRUE
  END-IF
  EXAMINE FULL FOESZ.BUCH_TEXT2 FOR 'RÜCK' GIVING POSITION #J
  IF #J > 0
    #RUECKRUF := TRUE
  END-IF
  EXAMINE FULL FOESZ.BUCH_TEXT2 FOR 'RüCK' GIVING POSITION #J
  IF #J > 0
    #RUECKRUF := TRUE
  END-IF
END-IF
*
RESET #NAME_UND_JAHR
COMPRESS PROJ.TEXT '-' #P_BEREICH1.#JAHR INTO #NAME_UND_JAHR
EXAMINE PROJ.M_TEXT AND TRANSLATE INTO UPPER CASE
#LES_STELLE := ZAHL_STELLE
************************************************************************
* Einlesen zuständige Kasse (nach Zahlstelle)
* BKT  => Bundeskasse Trier
* BKM  => Bundeskasse München
************************************************************************
RESET KASSE
SELECT SINGLE * INTO VIEW KASSE FROM ZAP-KASSE
    WHERE KENNUNG     = #LES_STELLE
    AND AUSZAHL_ART = 'R'
END-SELECT
EXAMINE KASSE.AUSZAHL_STELLE AND TRANSLATE INTO UPPER CASE
**SEPARATE KASSE.AUSZAHL_STELLE INTO #H30_1 #H30_2 WITH DELIMITER ' '
************************************************************************
* Ermittlung der gültigen Bandkennung
* BKT  => 3220 - 3259
* BKM  => 1110 - 1119
************************************************************************
IF #P_BEREICH1.#MODUS = 'PROD'
  UPDATE ZAP-FOESZ
  SET BANDKENN = 0
    WHERE PROJEKT = #DB2_PROJEKT
    AND JAHR      = #DB2_JAHR
    AND ZAP_DAT   = #DB2_ZAPDAT
    AND AUSZ_ART  = #P_BEREICH1.#TYP
*
  END OF TRANSACTION
END-IF
*
DECIDE ON FIRST VALUE OF ZAHL_STELLE
   VALUE 'BKT'
      RESET #NEUE_BANDKENN
      FOR #NUM 3220 3259
        RESET #TREFF_BANDKENN
        SELECT SINGLE COUNT(*)
          INTO #TREFF_BANDKENN FROM ZAP-FOESZ T1
         WHERE T1.AUSZ_ART IN ('Z','E')
           AND T1.STATUS BETWEEN 1 AND 8
           AND T1.BANDKENN = #NUM
        END-SELECT
        IF #TREFF_BANDKENN = 0
           ESCAPE BOTTOM
        END-IF
      END-FOR
      IF #NUM < 3260 AND #TREFF_BANDKENN = 0
         #NEUE_BANDKENN := #NUM
      END-IF
   VALUE 'BKM'
      RESET #NEUE_BANDKENN
      FOR #NUM 1110 1119
        RESET #TREFF_BANDKENN
        SELECT SINGLE COUNT(*)
          INTO #TREFF_BANDKENN FROM ZAP-FOESZ T1
         WHERE T1.AUSZ_ART IN ('Z','E')
           AND T1.STATUS BETWEEN 1 AND 8
           AND T1.BANDKENN = #NUM
        END-SELECT
        IF #TREFF_BANDKENN = 0
           ESCAPE BOTTOM
        END-IF
      END-FOR
      IF #NUM < 1120 AND #TREFF_BANDKENN = 0
         #NEUE_BANDKENN := #NUM
      END-IF
   NONE
      IGNORE
END-DECIDE
************************************************************************
* Umwandlung des ZAP-Datums von TT.MM.JJJJ in TTMMJJ für A-Satz
************************************************************************
#H_FI := 5
#H_FO := 4
RESET #H_DATUM
CALLNAT '#DATDREN' #P_BEREICH1.#ZAP_DAT #H_DATUM #H_FI #H_FO
#H_DAT_6 := SUBSTR(#H_DATUM,1,6)
************************************************************************
* Erstellen des A-Satzes
************************************************************************
#A_STRUKTUR.#A_SATZLAENGE   := '0128'
#A_STRUKTUR.#A_SA           := 'A'
#A_STRUKTUR.#A_KZ           := 'GK'
#A_STRUKTUR.#A_FELD_5       := '00000000'
#A_STRUKTUR.#A_BEWIRT       := '03063302'
*
#A_STRUKTUR.#A_LEER         := '  '
#A_STRUKTUR.#A_NAME_ZAHLER  := KASSE.AUSZAHL_STELLE
#A_STRUKTUR.#A_DATUM        := #H_DAT_6
#A_STRUKTUR.#A_LFD_DATNR    := #NEUE_BANDKENN
#A_STRUKTUR.#A_KENNZ_BUCH   := #BUCH_BAND_KENN
*
************************************************************************
* END_DAT um einen Tag verringern
************************************************************************
#H_FI := 5
#H_FO := 2
RESET #H_DATUM
CALLNAT '#DATDREN' #P_BEREICH1.#AUSFUER_DAT #H_DATUM #H_FI #H_FO
#H_DAT_8 := SUBSTR(#H_DATUM,1,8)
#A_DATUM_AUSF               := #H_DAT_8
*
#A_STRUKTUR.#A_KENNZ_BETRAG := '1'
*
WRITE WORK FILE 2 #A_STRUKTUR
*
************************************************************************
* Erstellen des 1-Satzes
************************************************************************
#1_STRUKTUR.#1_SK  := 1
#1_HHJJJJ           := #HH_JAHR
#N_NEUE_BANDKENN    := #NEUE_BANDKENN
#1_BAND_KZ          := #BUCH_BAND_KENN
#1_BEWIRT           := 3063302
*
RESET #E8
MOVE EDITED #1_BEWIRT  (EM=99999999) TO #E8
#1_BEWIRT   := #E8_N
************************************************************************
* Umwandlung des Ende-Datums von TT.MM.JJJJ in TTMMJJ für 1-Satz
************************************************************************
#H_FI := 5
#H_FO := 4
RESET #H_DATUM
CALLNAT '#DATDREN' #P_BEREICH1.#AUSFUER_DAT #H_DATUM #H_FI #H_FO
#H_DAT_6 := SUBSTR(#H_DATUM,1,6)
#1_AUS_DAT := #H_DAT_6
*
************************************************************************
* Umwandlung des ZAP-Datums von TT.MM.JJJJ in TTMMJJ für 1-Satz
************************************************************************
#H_FI := 5
#H_FO := 4
RESET #H_DATUM
CALLNAT '#DATDREN' #P_BEREICH1.#ZAP_DAT #H_DATUM #H_FI #H_FO
#H_DAT_6 := SUBSTR(#H_DATUM,1,6)
#1_ERST_DAT := #H_DAT_6
#1_KENNZ_WAEHR := 'E'
#1_NUMMER_BANKDATEI := #A_NEUE_BANDKENN
*
IF #F13Z_ART = 'L'
   #1_FUNKTION := 'L'
ELSE
   #1_FUNKTION := 'N'
END-IF
*
#1_EINLIEFERNUMMER := 0
RESET #E8
MOVE EDITED #1_EINLIEFERNUMMER (EM=99999999) TO #E8
#1_EINLIEFERNUMMER := #E8_N
*
#1_KENNZ_ZAHLART := 'GK'
*
WRITE WORK FILE 3 VARIABLE #1_STRUKTUR
*
#H_FI := 5
#H_FO := 1
RESET #H_DATUM
CALLNAT '#DATDREN' #P_BEREICH1.#ZAP_DAT #H_DATUM #H_FI #H_FO
#H_DAT_8 := SUBSTR(#H_DATUM,1,8)
*
PERFORM KONTROLL_BERICHT_KOPF
*
PERFORM ZAHLLISTE_KOPF
*
************************************************************************
* Ermittlung des ersten, zur Auszahlung anstehenden Betriebs
* ==> Gruppenwechsel
************************************************************************
RESET #ERSTENS
*
************************************************************************
* Löschen der Variablen zum zählen der Zahllistensätze
************************************************************************
RESET #SATZANZ
************************************************************************
* Lesen der Auszahlungsdaten und Schreiben der "C"-Sätze
************************************************************************
RESET FOES
SELECT *
    INTO VIEW FOES
    FROM ZAP-FOES
    WHERE PROJEKT = #DB2_PROJEKT
**  AND BETRNR    < 9000000000
    AND BETRNR    < 9999999999
    AND JAHR      BETWEEN #DB2_JAHR_V AND #DB2_JAHR_B
    AND ZAP_DAT   = #DB2_ZAPDAT
    AND STATUS    = #DB2_ZAP_STAT
    ORDER BY BETRNR,ABRNUM,HHL
    WITH HOLD
  /********************************************************************
  /* erstes Lesen: Aufbereiten Grunddaten für Betriebsnummernwechsel
  /********************************************************************
  DECIDE ON FIRST VALUE OF #ERSTENS
     VALUE 0
        #ERSTENS    := 1
        #VGL_BETRNR := FOES.BETRNR
        #VGL_JAHR   := FOES.JAHR
        #VGL_LFD    := FOES.HHL
        #VGL_ABRNUM := FOES.ABRNUM
     NONE
        IGNORE
  END-DECIDE
*
  IF FOES.BETRNR ^= #VGL_BETRNR
**  IF #SUM_ABRECH > 0
      RESET ABRJJ_SATZ
      SELECT SINGLE * INTO VIEW ABRJJ_SATZ FROM FOE-ABRJJ
          WHERE PROJEKT = #DB2_PROJEKT
          AND BETRNR  = #VGL_BETRNR
          AND JAHR    = #VGL_JAHR
          AND STATUS  BETWEEN #DB2_STATUS_1 AND #DB2_STATUS_2
          AND ABRNUM  = #VGL_ABRNUM
          AND ZENTRAL > 0
          AND LFD_NR  IN ( 0 , 1 )
      END-SELECT
      IF (ABRJJ_SATZ.NAME     = ' '
       OR ABRJJ_SATZ.BLZ      = 0
       OR ABRJJ_SATZ.KONTO    = ' '
       OR ABRJJ_SATZ.BANKNAME = ' ')
      AND ABRJJ_SATZ.ABRECHNUNG > 0
        WRITE '=' #VGL_BETRNR 'fehlende ABRJJ-Daten !!'
        IF #MODUS = 'PROD'
           #CODE        := 'fehlende ABRJJ-Daten'
           #PR_CON_CODE := 10106
           *ERROR-NR    := 9999
        END-IF
      END-IF
*
    IF #SUM_ABRECH > 0
      PERFORM SONDERFAELLE_AUS_PFAEN
*
    ELSE
      #L_NAME        := ABRJJ_SATZ.NAME
      #U_KONTO       := ABRJJ_SATZ.KONTO
      #U_BLZ         := ABRJJ_SATZ.BLZ
      #H_BETRAG_EURO := (#SUM_ABRECH - #GES_BN_MOSA)
    END-IF
    /*****************************************************************
    /* Aufaddieren bzw. -summieren der Einzelbetriebswerte
    /*****************************************************************
    CALLNAT '#ZAPPR1N'
      #PR_ABRECH #PR_BETR #PR_BETR_PM #PR_BETR_0 #PR_BETR_KL
      #PR_AUSZ   #PR_KLEIN
*
    RESET #HH_Z #HH_D #NIX_ADDIEREN
    FOR #I 1 100
      IF #BN_ANZ(#I) > 0
        ADD 1 TO #HH_Z
        IF #BN_AUSZ(#I) = 0
          ADD 1 TO #HH_D
        END-IF
      END-IF
    END-FOR
    IF #HH_Z = #HH_D
      #NIX_ADDIEREN := 1
    END-IF
*
    IF (#SUM_ABRECH = 0 AND #NIX_ADDIEREN = 0) OR #SUM_ABRECH > 0
      IF #SUM_ABRECH > 0
        RESET #SUM_MOSA_S
        SELECT SINGLE SUM(KUERZ_BETRAG_HHL) INTO #SUM_MOSA_S
          FROM ZAP-ZABUHHL
         WHERE PROJEKT  = #DB2_PROJEKT
           AND JAHR     = #DB2_JAHR
           AND BETRNR   = #VGL_BETRNR
           AND ABRNUM   = #VGL_ABRNUM
        END-SELECT
*
        RESET #SUM_MOSA_M
        SELECT SINGLE SUM(KUERZ_BETRAG_HHL) INTO #SUM_MOSA_M
          FROM ZAP-MOSA2
         WHERE PROJEKT  = #DB2_PROJEKT
           AND JAHR     = #DB2_JAHR
           AND BETRNR   = #VGL_BETRNR
           AND ABRNUM   = #VGL_ABRNUM
           AND MOSA_TYP = 'MO'
        END-SELECT
        IF #SUM_ABRECH - #SUM_MOSA_S - #SUM_MOSA_M > 0
*
          RESET #I #K #L
          RESET #V #W
*
          ADD 1 TO #BEZUG_ZK
          ADD 1 TO #BN_COUNTER
*
          PERFORM ZAHLLISTE_ZEILE1
*
          RESET #ZR_STRUKTUR
          RESET #EINZEL_BETRAG
          RESET #EINZEL_ZAEHL
          #GRUPPE := 1
          #GRUPPE_ZAEHL := 1
          FOR #I 1 100
             IF #BN_NR(#I) > 0
                RESET #ZR_HILF
                RESET #EINZEL_BETRAG
*
                IF #EINZEL_ZAEHL = 0
                  #H_BLZ := #L_BLZ
                  #EINZEL_ZAEHL := 1
                ELSE
                  RESET #H_BLZ
                END-IF
*
                DECIDE ON FIRST VALUE OF #PROJEKT
                VALUES 'ZDI' , 'ZMV' , 'EMV' , 'EWB'
                  RESET #VAIF_HHL
                  SELECT HHL INTO #VAIF_HHL FROM FOE-AIFPR
                   WHERE PROJEKT = #DB2_PROJEKT
                     AND JAHR    = #DB2_JAHR
                     AND DAYS(INS_TS) BETWEEN DAYS(#DB2_ZAPDAT) - 10
                                          AND DAYS(#DB2_ZAPDAT)
* *                      AND DATE(INS_TS) = #DB2_ZAPDAT
                  END-SELECT
*
                  RESET TITEL
                  SELECT SINGLE * INTO VIEW TITEL FROM ZAP-TITEL
                   WHERE LFD_NR      = #VAIF_HHL
                     AND VON_DATUM   <= #DB2_ZAPDAT
                     AND (#DB2_ZAPDAT <= BIS_DATUM
                       OR BIS_DATUM IS NULL)
                     AND (PROJEKT    = #DB2_PROJEKT
                       OR PROJEKT    = ' ')
                  END-SELECT
                NONE
                  RESET TITEL
                  SELECT SINGLE * INTO VIEW TITEL FROM ZAP-TITEL
                   WHERE LFD_NR      = #BN_NR(#I)
                     AND VON_DATUM   <= #DB2_ZAPDAT
                     AND (#DB2_ZAPDAT <= BIS_DATUM
                       OR BIS_DATUM IS NULL)
                     AND (PROJEKT    = #DB2_PROJEKT
                       OR PROJEKT    = ' ')
                  END-SELECT
                END-DECIDE
*
                IF BUND_STELLE > ' '
                   #ZR_HAUSHALT := VAL(BUND_STELLE)
                END-IF
*
                IF OBJ_KONTO > ' '
                   #ZR_OBJEKT := VAL(OBJ_KONTO)
                   MOVE EDITED #ZR_OBJEKT (EM=9999999999)
                     TO #E10
                   #ZR_OBJEKT := #E10_N
                END-IF
*
                #DB2_MOSA := ' '
                RESET #GES_BN_MOSA
                RESET #SUM_SA
                SELECT SINGLE SUM(KUERZ_BETRAG_HHL) INTO #SUM_SA
                  FROM ZAP-ZABUHHL
                 WHERE PROJEKT  = #DB2_PROJEKT
                   AND JAHR     = #DB2_JAHR
                   AND BETRNR   = #VGL_BETRNR
                   AND ABRNUM   = #VGL_ABRNUM
                   AND KUERZ_HHL = #BN_NR(#I)
                END-SELECT
*
                #DB2_MOSA := ' '
                RESET #SUM_MO
                SELECT SINGLE SUM(KUERZ_BETRAG_HHL) INTO #SUM_MO
                  FROM ZAP-MOSA2
                 WHERE PROJEKT  = #DB2_PROJEKT
                   AND JAHR     = #DB2_JAHR
                   AND BETRNR   = #VGL_BETRNR
                   AND ABRNUM   = #VGL_ABRNUM
                   AND HHL      = #BN_NR(#I)
                   AND MOSA_TYP = 'MO'
                END-SELECT
*
                IF #BN_AUSZ(#I) < 0
                   #KENNUNG      :=  'VERR'
                   #ZR_KENNZ_E_A :=  'E'
                   #SATZZR_BETRAG :=  #BN_AUSZ(#I) * -1
                   #EINZEL_BETRAG := #SATZZR_BETRAG
*
                   PERFORM ZAHLLISTE_EINZELDATEN
*
                   #SUMME_E_FUER_9 := #SUMME_E_FUER_9 + #SATZZR_BETRAG
                ELSE
                   #KENNUNG      :=  ' '
                   #ZR_KENNZ_E_A :=  'A'
                   #SATZZR_BETRAG    :=  #BN_AUSZ(#I)
                   #EINZEL_BETRAG := #SATZZR_BETRAG
*
                   PERFORM ZAHLLISTE_EINZELDATEN
*
                END-IF
                #E14  := #SATZZR_BETRAG * 100
                MOVE EDITED #E14 (EM=99999999999999)
                  TO #ZR_BETRAG
*
                #ZR_UNTERSTRUK(#GRUPPE_ZAEHL) := #ZR_HILF
*
                IF #GRUPPE_ZAEHL = 4
                   PERFORM SATZ-ZR_SCHREIBEN
                   #GRUPPE_ZAEHL := 0
                   ADD 1 TO #GRUPPE
                   ADD 1 TO #SATZANZ
                END-IF
*
                ADD 1 TO #GRUPPE_ZAEHL
*
                IF #SUM_SA ^= 0
*
* nicht das zugehörige normale Objektkonto, sondern das Sanktions-Obje  kt
* konto angeben
*
                   RESET #SA_OBJ_KONTO #SA_BUND_STELLE
                   SELECT SINGLE OBJ_KONTO,BUND_STELLE
                     INTO #SA_OBJ_KONTO,#SA_BUND_STELLE
                     FROM ZAP-TITEL
                    WHERE VON_DATUM   <= #DB2_ZAPDAT
                      AND (#DB2_ZAPDAT <= BIS_DATUM
                        OR BIS_DATUM IS NULL)
                      AND (PROJEKT    = #DB2_PROJEKT
                      AND KONTO_ART  = 'S')
                   END-SELECT
*
                   IF #SA_BUND_STELLE > ' '
                      #ZR_HAUSHALT := VAL(#SA_BUND_STELLE)
                      BUND_STELLE := #SA_BUND_STELLE
                   END-IF
*
                   IF #SA_OBJ_KONTO > ' '
                      #ZR_OBJEKT := VAL(#SA_OBJ_KONTO)
                      MOVE EDITED #ZR_OBJEKT (EM=9999999999)
                        TO #E10
                      #ZR_OBJEKT := #E10_N
                      OBJ_KONTO := #SA_OBJ_KONTO
                   END-IF
*
                   #KENNUNG      :=  'SANK'
                   #ZR_KENNZ_E_A :=  'E'
                   #SUMME_E_FUER_9 := #SUMME_E_FUER_9 + #SUM_SA
                   #EINZEL_BETRAG  := #SUM_SA
                   #SUMME_SA  := #SUMME_SA + #SUM_SA
                   #E14  := #SUM_SA * 100
                   MOVE EDITED #E14 (EM=99999999999999)
                     TO #ZR_BETRAG
*
                   #ZR_UNTERSTRUK(#GRUPPE_ZAEHL) := #ZR_HILF
*
                   RESET #H_BLZ
                   PERFORM ZAHLLISTE_EINZELDATEN
*
                   IF #GRUPPE_ZAEHL = 4
                      PERFORM SATZ-ZR_SCHREIBEN
                      #GRUPPE_ZAEHL := 0
                      ADD 1 TO #GRUPPE
                      ADD 1 TO #SATZANZ
                   END-IF
*
                   ADD 1 TO #GRUPPE_ZAEHL
                END-IF
*
                IF #SUM_MO ^= 0
                   #KENNUNG      :=  'MODU'
                   #ZR_KENNZ_E_A :=  'E'
                   #SUMME_E_FUER_9 := #SUMME_E_FUER_9 + #SUM_MO
                   #EINZEL_BETRAG  := #SUM_MO
                   #SUMME_MO  := #SUMME_MO + #SUM_MO
                   #E14  := #SUM_MO * 100
                   MOVE EDITED #E14 (EM=99999999999999)
                     TO #ZR_BETRAG
*
                   RESET #H_BLZ
                   PERFORM ZAHLLISTE_EINZELDATEN
*
                   #ZR_UNTERSTRUK(#GRUPPE_ZAEHL) := #ZR_HILF
*
                   IF #GRUPPE_ZAEHL = 4
                      PERFORM SATZ-ZR_SCHREIBEN
                      #GRUPPE_ZAEHL := 0
                      ADD 1 TO #GRUPPE
                      ADD 1 TO #SATZANZ
                   END-IF
                   ADD 1 TO #GRUPPE_ZAEHL
                END-IF
             END-IF
          END-FOR
          #GRUPPE_ZAEHL := #GRUPPE_ZAEHL -1
*
          IF #GRUPPE_ZAEHL > 0
             PERFORM SATZ-ZR_SCHREIBEN
             ADD 1 TO #SATZANZ
          END-IF
        END-IF
      END-IF
*
      FOR #I 1 100
         IF #BN_NR(#I) > 0
            FOR #K 1 100
               IF #BN_NR(#I) = #HHL_NR(#K)
                  ESCAPE BOTTOM
               END-IF
            END-FOR
            IF #K > 100
               FOR #J 100 1 -1
                  IF #HHL_NR(#J) > 0
                     ESCAPE BOTTOM
                  END-IF
               END-FOR
               #L := #J + 1
*
               RESET #BUND
               DECIDE ON FIRST VALUE OF #PROJEKT
               VALUES 'ZDI' , 'ZMV' , 'EMV' , 'EWB'
                 RESET #VAIF_HHL
                 SELECT HHL INTO #VAIF_HHL FROM FOE-AIFPR
                  WHERE PROJEKT = #DB2_PROJEKT
                    AND JAHR    = #DB2_JAHR
                    AND DAYS(INS_TS) BETWEEN DAYS(#DB2_ZAPDAT) - 10
                                         AND DAYS(#DB2_ZAPDAT)
* *                     AND DATE(INS_TS) = #DB2_ZAPDAT
                 END-SELECT
*
                 SELECT SINGLE BUND_STELLE INTO #BUND FROM ZAP-TITEL
                  WHERE LFD_NR      = #VAIF_HHL
                    AND VON_DATUM   <= #DB2_ZAPDAT
                    AND (#DB2_ZAPDAT <= BIS_DATUM
                      OR BIS_DATUM IS NULL)
                    AND (PROJEKT    = #DB2_PROJEKT
                      OR PROJEKT    = ' ')
                 END-SELECT
               NONE
                 SELECT SINGLE BUND_STELLE INTO #BUND FROM ZAP-TITEL
                  WHERE LFD_NR      = #BN_NR(#I)
                    AND VON_DATUM   <= #DB2_ZAPDAT
                    AND (#DB2_ZAPDAT <= BIS_DATUM
                      OR BIS_DATUM IS NULL)
                    AND (PROJEKT    = #DB2_PROJEKT
                      OR PROJEKT    = ' ')
                 END-SELECT
               END-DECIDE
*
               IF #BUND > ' '
                  #HHL_BUND(#L)  := #BUND
               END-IF
*
               #HHL_NR(#L)       := #BN_NR(#I)
               IF #NIX_ADDIEREN = 0
                  #HHL_ANZ(#L)   :=
                  #HHL_ANZ(#L)    + #BN_ANZ(#I)
               END-IF
               #HHL_AUSZ(#L)     :=
               #HHL_AUSZ(#L)      + #BN_AUSZ(#I)
            ELSE
               IF #NIX_ADDIEREN = 0
                  #HHL_ANZ(#K)   :=
                  #HHL_ANZ(#K)    + #BN_ANZ(#I)
               END-IF
               #HHL_AUSZ(#K)     :=
               #HHL_AUSZ(#K)      + #BN_AUSZ(#I)
            END-IF
*
         END-IF
      END-FOR
    END-IF
    /*****************************************************************
    /* Löschen Betriebssnummen-Variablen
    /*****************************************************************
    RESET #BN_STRUK(*)
    RESET #SUM_ABRECH #SUM_BAY_ABRECH #PR_ABRECH
    #VGL_LFD    := FOES.HHL
    #VGL_ABRNUM := FOES.ABRNUM
    #VGL_JAHR   := FOES.JAHR
    #VGL_BETRNR := FOES.BETRNR
  END-IF
  /*********************************************************************
  /* Commit-Point nach 500 Betrieben setzen
  /*********************************************************************
  #BN_ZAEHL := #BN_ZAEHL + 1
  IF #BN_ZAEHL = 100
    WRITE #VGL_BETRNR
    COMMIT
    RESET #BN_ZAEHL
  END-IF
*
  #SUM_ABRECH       := #SUM_ABRECH     + FOES.ABRECH
  #PR_ABRECH        := #PR_ABRECH      + FOES.ABRECH
  #SUM_BAY_ABRECH   := #SUM_BAY_ABRECH + FOES.BAY_ABRECH
*
  IF FOES.HHL > 0
      /*****************************************************************
      /* Aufaddieren entfällt bei HHL = 0 und Abrechnung = 0
      /*****************************************************************
      IF FOES.HHL > 0 AND FOES.ABRECH ^= 0
         FOR #J 100 1 -1
            IF #BN_NR(#J) > 0
               ESCAPE BOTTOM
            END-IF
         END-FOR
         #K := #J
         DECIDE ON FIRST VALUE OF #PROJEKT
           VALUES 'ZDI' , 'ZMV' , 'EMV' , 'EWB'
             RESET #VAIF_HHL
             SELECT HHL INTO #VAIF_HHL FROM FOE-AIFPR
              WHERE PROJEKT = #DB2_PROJEKT
                AND JAHR    = #DB2_JAHR
                AND DAYS(INS_TS) BETWEEN DAYS(#DB2_ZAPDAT) - 10
                                     AND DAYS(#DB2_ZAPDAT)
* *                 AND DATE(INS_TS) = #DB2_ZAPDAT
             END-SELECT
*
             FOR #J 1 100
                IF #VAIF_HHL = #BN_NR(#J)
                   ESCAPE BOTTOM
                END-IF
             END-FOR
             IF #J > 100
                ADD 1 TO #K
                #BN_NR(#K)   := #VAIF_HHL
                #BN_ANZ(#K)  :=
                #BN_ANZ(#K)  + 1
                #BN_AUSZ(#K) :=
                #BN_AUSZ(#K) + FOES.ABRECH
             ELSE
                #BN_AUSZ(#J) :=
                #BN_AUSZ(#J) + FOES.ABRECH
             END-IF
           NONE
             FOR #J 1 100
                IF FOES.HHL = #BN_NR(#J)
                   ESCAPE BOTTOM
                END-IF
             END-FOR
             IF #J > 100
                ADD 1 TO #K
                #BN_NR(#K)   := FOES.HHL
                #BN_ANZ(#K)  :=
                #BN_ANZ(#K)  + 1
                #BN_AUSZ(#K) :=
                #BN_AUSZ(#K) + FOES.ABRECH
             ELSE
                #BN_ANZ(#J)  :=
                #BN_ANZ(#J)  + 1
                #BN_AUSZ(#J) :=
                #BN_AUSZ(#J) + FOES.ABRECH
             END-IF
         END-DECIDE
         /**************************************************
         /* ADDIEREN NUR MEHRJAHRESSANKTIONEN
         /**************************************************
         RESET #U #V #W #X #IDENT_MJSAGRUND #PROJEKT_UR #SUM_BN_MOSA
         SELECT IDENT_MJSAGRUND,SUM(KUERZ_BETRAG_HHL)
           INTO #IDENT_MJSAGRUND,#SUM_BN_MOSA
           FROM ZAP-ZABUHHL
          WHERE PROJEKT   = #DB2_PROJEKT
            AND JAHR      = FOES.JAHR
            AND BETRNR    = FOES.BETRNR
            AND ABRNUM    = FOES.ABRNUM
            AND KUERZ_HHL = FOES.HHL
          GROUP BY IDENT_MJSAGRUND
*
           RESET #PROJEKT_UR
           SELECT PROJEKT INTO #PROJEKT_UR
             FROM ZAP-MJSAGRUND
            WHERE IDENT = #IDENT_MJSAGRUND
           END-SELECT
*
           #U := 1
           DECIDE ON FIRST VALUE OF #PROJEKT_UR
             VALUE 'KPR'
               #X := 1
             VALUE 'RIP'
               #X := 2
             VALUE 'SFP'
               #X := 3
             VALUE 'MIP'
               #X := 4
             VALUE 'DZP'
               #X := 5
             VALUE 'STK'
               #X := 6
             VALUE 'ZBB'
               #X := 7
             VALUE 'AGZ'
               #X := 8
             VALUE 'KUA'
               #X := 9
             NONE
               WRITE 'falsches Projekt'
           END-DECIDE
*
           IF #SUM_BN_MOSA > 0 AND #U > 0
              FOR #V 10 1 -1
                 IF #MOSA_NR(#U,#V,#X) > 0
                    ESCAPE BOTTOM
                 END-IF
              END-FOR
              #W := #V
              FOR #V 1 10
                 IF FOES.HHL = #MOSA_NR(#U,#V,#X)
                    ESCAPE BOTTOM
                 END-IF
              END-FOR
              IF #V > 10
                 ADD 1 TO #W
                 #MOSA_NR(#U,#W,#X)   := FOES.HHL
                 #MOSA_ANZ(#U,#W,#X)  :=
                 #MOSA_ANZ(#U,#W,#X)  + 1
                 #MOSA_AUSZ(#U,#W,#X) :=
                 #MOSA_AUSZ(#U,#W,#X) + #SUM_BN_MOSA
              ELSE
                 #MOSA_ANZ(#U,#V,#X)  :=
                 #MOSA_ANZ(#U,#V,#X)  + 1
                 #MOSA_AUSZ(#U,#V,#X) :=
                 #MOSA_AUSZ(#U,#V,#X) + #SUM_BN_MOSA
              END-IF
            END-IF
         END-SELECT
         /**************************************************
         /* NUR MODULATIONEN
         /**************************************************
         SELECT SUM(KUERZ_BETRAG_HHL)
           INTO #SUM_BN_MOSA
           FROM ZAP-MOSA2
          WHERE PROJEKT  = #DB2_PROJEKT
            AND JAHR     = FOES.JAHR
            AND BETRNR   = FOES.BETRNR
            AND ABRNUM   = FOES.ABRNUM
            AND HHL      = FOES.HHL
            AND MOSA_TYP = 'MO'
*
           #U := 2
           #X := 1
           IF #SUM_BN_MOSA > 0
             FOR #V 10 1 -1
               IF #MOSA_NR(#U,#V,#X) > 0
                 ESCAPE BOTTOM
                END-IF
             END-FOR
             #W := #V
             FOR #V 1 10
               IF FOES.HHL = #MOSA_NR(#U,#V,#X)
                 ESCAPE BOTTOM
               END-IF
             END-FOR
             IF #V > 10
               ADD 1 TO #W
               #MOSA_NR(#U,#W,#X)   := FOES.HHL
               #MOSA_ANZ(#U,#W,#X)  :=
               #MOSA_ANZ(#U,#W,#X)  + 1
               #MOSA_AUSZ(#U,#W,#X) :=
               #MOSA_AUSZ(#U,#W,#X) + #SUM_BN_MOSA
             ELSE
               #MOSA_ANZ(#U,#V,#X)  :=
               #MOSA_ANZ(#U,#V,#X)  + 1
               #MOSA_AUSZ(#U,#V,#X) :=
               #MOSA_AUSZ(#U,#V,#X) + #SUM_BN_MOSA
             END-IF
           END-IF
         END-SELECT
     END-IF
  END-IF
END-SELECT
** IF #SUM_ABRECH > 0
   #VGL_BETRNR := FOES.BETRNR
   #VGL_JAHR   := FOES.JAHR
   #VGL_ABRNUM := FOES.ABRNUM
*
   RESET ABRJJ_SATZ
   SELECT SINGLE * INTO VIEW ABRJJ_SATZ FROM FOE-ABRJJ
     WHERE PROJEKT = #DB2_PROJEKT
       AND BETRNR  = #VGL_BETRNR
       AND JAHR    = #VGL_JAHR
       AND STATUS  BETWEEN #DB2_STATUS_1 AND #DB2_STATUS_2
       AND ABRNUM  = #VGL_ABRNUM
       AND ZENTRAL > 0
       AND LFD_NR  IN ( 0 , 1 )
   END-SELECT
   IF (ABRJJ_SATZ.NAME     = ' '
    OR ABRJJ_SATZ.BLZ      = 0
    OR ABRJJ_SATZ.KONTO    = ' '
    OR ABRJJ_SATZ.BANKNAME = ' ')
   AND ABRJJ_SATZ.ABRECHNUNG > 0
     WRITE '=' #VGL_BETRNR 'fehlende ABRJJ-Daten !!'
     IF #MODUS = 'PROD'
        #CODE        := 'fehlende ABRJJ-Daten'
        #PR_CON_CODE := 10106
        *ERROR-NR    := 9999
     END-IF
   END-IF
*
IF #SUM_ABRECH > 0
   PERFORM SONDERFAELLE_AUS_PFAEN
*
ELSE
  #L_NAME        := ABRJJ_SATZ.NAME
  #U_KONTO       := ABRJJ_SATZ.KONTO
  #U_BLZ         := ABRJJ_SATZ.BLZ
  #H_BETRAG_EURO := (#SUM_ABRECH - #GES_BN_MOSA)
END-IF
************************************************************************
* Aufaddieren bzw. -summieren der Einzelbetriebswerte
************************************************************************
CALLNAT '#ZAPPR1N'
  #PR_ABRECH #PR_BETR #PR_BETR_PM #PR_BETR_0 #PR_BETR_KL
  #PR_AUSZ   #PR_KLEIN
*
RESET #HH_Z #HH_D #NIX_ADDIEREN
FOR #I 1 100
  IF #BN_ANZ(#I) > 0
    ADD 1 TO #HH_Z
    IF #BN_AUSZ(#I) = 0
      ADD 1 TO #HH_D
    END-IF
  END-IF
END-FOR
IF #HH_Z = #HH_D
  #NIX_ADDIEREN := 1
END-IF
************************************************************************
* !! KLEINBETRAGSREGELUNG WIRD FÜR ZAP AB SOFORT AUFGEHOBEN !!  *
************************************************************************
* IF (#SUM_ABRECH = 0 AND #NIX_ADDIEREN = 0)
*    OR #SUM_ABRECH >= KLEIN.POS_KLEIN_DM
************************************************************************
IF (#SUM_ABRECH = 0 AND #NIX_ADDIEREN = 0)
OR #SUM_ABRECH > 0
  IF #SUM_ABRECH > 0
    RESET #SUM_MOSA_S
    SELECT SINGLE SUM(KUERZ_BETRAG_HHL) INTO #SUM_MOSA_S
      FROM ZAP-ZABUHHL
     WHERE PROJEKT  = #DB2_PROJEKT
       AND JAHR     = #DB2_JAHR
       AND BETRNR   = #VGL_BETRNR
       AND ABRNUM   = #VGL_ABRNUM
    END-SELECT
*
    RESET #SUM_MOSA_M
    SELECT SINGLE SUM(KUERZ_BETRAG_HHL) INTO #SUM_MOSA_M
      FROM ZAP-MOSA2
     WHERE PROJEKT  = #DB2_PROJEKT
       AND JAHR     = #DB2_JAHR
       AND BETRNR   = #VGL_BETRNR
       AND ABRNUM   = #VGL_ABRNUM
       AND MOSA_TYP = 'MO'
    END-SELECT
    IF #SUM_ABRECH - #SUM_MOSA_S - #SUM_MOSA_M > 0
      RESET #I #K #L
      RESET #V #W
*
      ADD 1 TO #BEZUG_ZK
      ADD 1 TO #BN_COUNTER
*
      PERFORM ZAHLLISTE_ZEILE1
*
      RESET #ZR_STRUKTUR
      RESET #EINZEL_BETRAG
      RESET #EINZEL_ZAEHL
      #GRUPPE := 1
      #GRUPPE_ZAEHL := 1
      FOR #I 1 100
         IF #BN_NR(#I) > 0
            RESET #ZR_HILF
            RESET #EINZEL_BETRAG
*
            IF #EINZEL_ZAEHL = 0
              #H_BLZ := #L_BLZ
              #EINZEL_ZAEHL := 1
            ELSE
              RESET #H_BLZ
            END-IF
            DECIDE ON FIRST VALUE OF #PROJEKT
            VALUES 'ZDI' , 'ZMV' , 'EMV' , 'EWB'
              RESET #VAIF_HHL
              SELECT HHL INTO #VAIF_HHL FROM FOE-AIFPR
               WHERE PROJEKT = #DB2_PROJEKT
                 AND JAHR    = #DB2_JAHR
                 AND DAYS(INS_TS) BETWEEN DAYS(#DB2_ZAPDAT) - 10
                                      AND DAYS(#DB2_ZAPDAT)
* *                  AND DATE(INS_TS) = #DB2_ZAPDAT
              END-SELECT
*
              RESET TITEL
              SELECT SINGLE * INTO VIEW TITEL FROM ZAP-TITEL
               WHERE LFD_NR      = #VAIF_HHL
                 AND VON_DATUM   <= #DB2_ZAPDAT
                 AND (#DB2_ZAPDAT <= BIS_DATUM
                   OR BIS_DATUM IS NULL)
                 AND (PROJEKT    = #DB2_PROJEKT
                   OR PROJEKT    = ' ')
              END-SELECT
            NONE
              RESET TITEL
              SELECT SINGLE * INTO VIEW TITEL FROM ZAP-TITEL
               WHERE LFD_NR      = #BN_NR(#I)
                 AND VON_DATUM   <= #DB2_ZAPDAT
                 AND (#DB2_ZAPDAT <= BIS_DATUM
                   OR BIS_DATUM IS NULL)
                 AND (PROJEKT    = #DB2_PROJEKT
                   OR PROJEKT    = ' ')
              END-SELECT
            END-DECIDE
*
            IF BUND_STELLE > ' '
               #ZR_HAUSHALT := VAL(BUND_STELLE)
            END-IF
*
            IF OBJ_KONTO > ' '
               #ZR_OBJEKT := VAL(OBJ_KONTO)
               MOVE EDITED #ZR_OBJEKT (EM=9999999999)
                 TO #E10
               #ZR_OBJEKT := #E10_N
            END-IF
*
** ##   #BAND_AUSZ := #BN_AUSZ(#I)
*
            #DB2_MOSA := ' '
            RESET #GES_BN_MOSA
            RESET #SUM_SA
            SELECT SINGLE SUM(KUERZ_BETRAG_HHL) INTO #SUM_SA
              FROM ZAP-ZABUHHL
             WHERE PROJEKT   = #DB2_PROJEKT
               AND JAHR      = #DB2_JAHR
               AND BETRNR    = #VGL_BETRNR
               AND ABRNUM    = #VGL_ABRNUM
               AND KUERZ_HHL = #BN_NR(#I)
            END-SELECT
*
** ##   IF #SUM_MOSA > 0
** ##      #BAND_AUSZ := #BAND_AUSZ - #SUM_MOSA
** ##   END-IF
*
*
            RESET #SUM_MO
            SELECT SINGLE SUM(KUERZ_BETRAG_HHL) INTO #SUM_MO
              FROM ZAP-MOSA2
             WHERE PROJEKT  = #DB2_PROJEKT
               AND JAHR     = #DB2_JAHR
               AND BETRNR   = #VGL_BETRNR
               AND ABRNUM   = #VGL_ABRNUM
               AND HHL      = #BN_NR(#I)
               AND MOSA_TYP = 'MO'
            END-SELECT
*
** ##   IF #SUM_MOSA > 0
** ##          #BAND_AUSZ := #BAND_AUSZ - #SUM_MOSA
** ##   END-IF
*
            IF #BN_AUSZ(#I) < 0
               #KENNUNG      :=  'VERR'
               #ZR_KENNZ_E_A :=  'E'
               #SATZZR_BETRAG    :=  #BN_AUSZ(#I) * -1
               #EINZEL_BETRAG := #SATZZR_BETRAG
*
               PERFORM ZAHLLISTE_EINZELDATEN
*
               #SUMME_E_FUER_9 := #SUMME_E_FUER_9 + #SATZZR_BETRAG
            ELSE
               #KENNUNG      :=  ' '
               #ZR_KENNZ_E_A :=  'A'
               #SATZZR_BETRAG    :=  #BN_AUSZ(#I)
               #EINZEL_BETRAG := #SATZZR_BETRAG
*
               PERFORM ZAHLLISTE_EINZELDATEN
*
            END-IF
            #E14  := #SATZZR_BETRAG * 100
            MOVE EDITED #E14 (EM=99999999999999)
              TO #ZR_BETRAG
*
            #ZR_UNTERSTRUK(#GRUPPE_ZAEHL) := #ZR_HILF
*
            IF #GRUPPE_ZAEHL = 4
               PERFORM SATZ-ZR_SCHREIBEN
               #GRUPPE_ZAEHL := 0
               ADD 1 TO #GRUPPE
               ADD 1 TO #SATZANZ
            END-IF
*
            ADD 1 TO #GRUPPE_ZAEHL
*
            IF #SUM_SA ^= 0
*
* nicht das zugehörige normale Objektkonto, sondern das Sanktions-Obje  kt
* konto angeben
*
               RESET #SA_OBJ_KONTO #SA_BUND_STELLE
               SELECT SINGLE OBJ_KONTO,BUND_STELLE
                 INTO #SA_OBJ_KONTO,#SA_BUND_STELLE
                 FROM ZAP-TITEL
                WHERE VON_DATUM   <= #DB2_ZAPDAT
                  AND (#DB2_ZAPDAT <= BIS_DATUM
                    OR BIS_DATUM IS NULL)
                  AND (PROJEKT    = #DB2_PROJEKT
                  AND KONTO_ART  = 'S')
               END-SELECT
*
               IF #SA_BUND_STELLE > ' '
                  #ZR_HAUSHALT := VAL(#SA_BUND_STELLE)
                  BUND_STELLE := #SA_BUND_STELLE
               END-IF
*
               IF #SA_OBJ_KONTO > ' '
                  #ZR_OBJEKT := VAL(#SA_OBJ_KONTO)
                  MOVE EDITED #ZR_OBJEKT (EM=9999999999)
                    TO #E10
                  #ZR_OBJEKT := #E10_N
                  OBJ_KONTO := #SA_OBJ_KONTO
               END-IF
*
               #KENNUNG      :=  'SANK'
               #ZR_KENNZ_E_A :=  'E'
               #SUMME_E_FUER_9 := #SUMME_E_FUER_9 + #SUM_SA
               #EINZEL_BETRAG  := #SUM_SA
*
               #E14  := #SUM_SA * 100
               MOVE EDITED #E14 (EM=99999999999999)
                 TO #ZR_BETRAG
*
               RESET #H_BLZ
               PERFORM ZAHLLISTE_EINZELDATEN
*
               #ZR_UNTERSTRUK(#GRUPPE_ZAEHL) := #ZR_HILF
*
               IF #GRUPPE_ZAEHL = 4
                  PERFORM SATZ-ZR_SCHREIBEN
                  #GRUPPE_ZAEHL := 0
                  ADD 1 TO #GRUPPE
                  ADD 1 TO #SATZANZ
               END-IF
*
               ADD 1 TO #GRUPPE_ZAEHL
            END-IF
*
            IF #SUM_MO ^= 0
               #KENNUNG      :=  'MODU'
               #ZR_KENNZ_E_A :=  'E'
               #SUMME_E_FUER_9 := #SUMME_E_FUER_9 + #SUM_MO
               #EINZEL_BETRAG  := #SUM_MO
*
               #E14  := #SUM_MO * 100
               MOVE EDITED #E14 (EM=99999999999999)
                 TO #ZR_BETRAG
*
               RESET #H_BLZ
               PERFORM ZAHLLISTE_EINZELDATEN
*
               #ZR_UNTERSTRUK(#GRUPPE_ZAEHL) := #ZR_HILF
*
               IF #GRUPPE_ZAEHL = 4
                  PERFORM SATZ-ZR_SCHREIBEN
                  #GRUPPE_ZAEHL := 0
                  ADD 1 TO #GRUPPE
                  ADD 1 TO #SATZANZ
               END-IF
*
               ADD 1 TO #GRUPPE_ZAEHL
            END-IF
         END-IF
      END-FOR
      #GRUPPE_ZAEHL := #GRUPPE_ZAEHL -1
*
      IF #GRUPPE_ZAEHL > 0
         PERFORM SATZ-ZR_SCHREIBEN
         ADD 1 TO #SATZANZ
      END-IF
    END-IF
  END-IF
*
  FOR #I 1 100
     IF #BN_NR(#I) > 0
        FOR #K 1 100
           IF #BN_NR(#I) = #HHL_NR(#K)
              ESCAPE BOTTOM
           END-IF
        END-FOR
        IF #K > 100
           FOR #J 100 1 -1
              IF #HHL_NR(#J) > 0
                 ESCAPE BOTTOM
              END-IF
           END-FOR
           #L := #J + 1
*
           RESET #BUND
           DECIDE ON FIRST VALUE OF #PROJEKT
           VALUES 'ZDI' , 'ZMV' , 'EMV' , 'EWB'
             RESET #VAIF_HHL
             SELECT HHL INTO #VAIF_HHL FROM FOE-AIFPR
              WHERE PROJEKT = #DB2_PROJEKT
                AND JAHR    = #DB2_JAHR
                AND DAYS(INS_TS) BETWEEN DAYS(#DB2_ZAPDAT) - 10
                                     AND DAYS(#DB2_ZAPDAT)
* *                 AND DATE(INS_TS) = #DB2_ZAPDAT
             END-SELECT
*
             SELECT SINGLE BUND_STELLE INTO #BUND FROM ZAP-TITEL
              WHERE LFD_NR      = #VAIF_HHL
                AND VON_DATUM   <= #DB2_ZAPDAT
                AND (#DB2_ZAPDAT <= BIS_DATUM
                  OR BIS_DATUM IS NULL)
                AND (PROJEKT    = #DB2_PROJEKT
                  OR PROJEKT    = ' ')
             END-SELECT
           NONE
             SELECT SINGLE BUND_STELLE INTO #BUND FROM ZAP-TITEL
              WHERE LFD_NR      = #BN_NR(#I)
                AND VON_DATUM   <= #DB2_ZAPDAT
                AND (#DB2_ZAPDAT <= BIS_DATUM
                  OR BIS_DATUM IS NULL)
                AND (PROJEKT    = #DB2_PROJEKT
                  OR PROJEKT    = ' ')
             END-SELECT
           END-DECIDE
*
           IF #BUND > ' '
              #HHL_BUND(#L)  := #BUND
           END-IF
*
           #HHL_NR(#L)       := #BN_NR(#I)
           IF #NIX_ADDIEREN = 0
              #HHL_ANZ(#L)   :=
              #HHL_ANZ(#L)    + #BN_ANZ(#I)
           END-IF
           #HHL_AUSZ(#L)     :=
           #HHL_AUSZ(#L)      + #BN_AUSZ(#I)
        ELSE
           IF #NIX_ADDIEREN = 0
              #HHL_ANZ(#K)   :=
              #HHL_ANZ(#K)    + #BN_ANZ(#I)
           END-IF
           #HHL_AUSZ(#K)     :=
           #HHL_AUSZ(#K)      + #BN_AUSZ(#I)
        END-IF
     END-IF
  END-FOR
END-IF
*
RESET #BN_STRUK(*)
************************************************************************
* Löschen Betriebssnummen-Variablen
************************************************************************
RESET #SUM_ABRECH #SUM_BAY_ABRECH #PR_ABRECH
************************************************************************
* Schreiben des E-Satzes
************************************************************************
#E_STRUKTUR.#E_SATZLAENGE := '0128'
#E_STRUKTUR.#E_SA         := 'E'
#E_STRUKTUR.#E_SUM_BETRAG := '0000000000000'
*
MOVE EDITED #H_SUM_CSATZ  (EM=9999999)
  TO #E_STRUKTUR.#E_SUM_CSATZ
#E_STRUKTUR.#E_FELD_4 := '00'
#E_STRUKTUR.#E_FELD_5 := '00'
MOVE EDITED #H_SUM_KONTO (EM=999999999999999)
  TO #E_STRUKTUR.#E_SUM_KONTO
MOVE EDITED #H_SUM_BLZ   (EM=999999999999999)
  TO #E_STRUKTUR.#E_SUM_BLZ
#N13  := #H_SUMME_IN_EURO_BAND * 100
MOVE EDITED #N13 (EM=9999999999999)
  TO #E_STRUKTUR.#E_SUMME_IN_EURO_BAND
*
WRITE WORK FILE 2 #E_STRUKTUR
FORMAT(1) PS=91 LS=132
*
CLOSE WORK FILE 4
CLOSE WORK FILE 5
*
***=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*==*
*     Ausdruck von F13Z und Schreiben Buchungssätze
***=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*==*
**====================================================================
** Verarbeitung Bundekassen Trier und München
**====================================================================
RESET #K #SUM_OBJ #SUM_OBJ_VERR  #SUM_OBJ_PLUS1
*
FOR #U 1 2
   FOR #I 1 10
      FOR #X 1 9
         IF #MOSA_AUSZ(#U,#I,#X) > 0
            FOR #J 1 100
               IF #MOSA_NR(#U,#I,#X) = #HHL_NR(#J)
                  #HHL_AUSZ(#J) := #HHL_AUSZ(#J)
                                 - #MOSA_AUSZ(#U,#I,#X)
               END-IF
            END-FOR
            ADD (#MOSA_AUSZ(#U,#I,#X) * -1) TO #SUM_BETRAG_VERR
            ADD (#MOSA_AUSZ(#U,#I,#X) * -1) TO #SUM_BETRAG_VERR
            IF #U = 1
               DECIDE ON FIRST VALUE OF #X
                  VALUE 1
                     RESET #HHL_SA #BUND_SA
                     SELECT SINGLE LFD_NR,BUND_STELLE
                       INTO #HHL_SA, #BUND_SA
                       FROM ZAP-TITEL
                      WHERE  VON_DATUM   <= #PR_ZAP_DAT
                        AND (#PR_ZAP_DAT <= BIS_DATUM
                          OR BIS_DATUM IS NULL)
                        AND (PROJEKT    = 'KPR'
                         AND KONTO_ART  = 'S')
                     END-SELECT
                  VALUE 2
                     RESET #HHL_SA #BUND_SA
                     SELECT SINGLE LFD_NR,BUND_STELLE
                       INTO #HHL_SA,#BUND_SA
                       FROM ZAP-TITEL
                      WHERE  VON_DATUM   <= #PR_ZAP_DAT
                        AND (#PR_ZAP_DAT <= BIS_DATUM
                          OR BIS_DATUM IS NULL)
                        AND (PROJEKT    = 'RIP'
                         AND KONTO_ART  = 'S')
                     END-SELECT
                  VALUE 3
                     RESET #HHL_SA #BUND_SA
                     SELECT SINGLE LFD_NR,BUND_STELLE
                       INTO #HHL_SA, #BUND_SA
                       FROM ZAP-TITEL
                      WHERE  VON_DATUM   <= #PR_ZAP_DAT
                        AND (#PR_ZAP_DAT <= BIS_DATUM
                          OR BIS_DATUM IS NULL)
                        AND (PROJEKT    = 'SFP'
                         AND KONTO_ART  = 'S')
                     END-SELECT
                  VALUE 4
                     RESET #HHL_SA #BUND_SA
                     SELECT SINGLE LFD_NR,BUND_STELLE
                       INTO #HHL_SA, #BUND_SA
                       FROM ZAP-TITEL
                      WHERE  VON_DATUM   <= #PR_ZAP_DAT
                        AND (#PR_ZAP_DAT <= BIS_DATUM
                          OR BIS_DATUM IS NULL)
                        AND (PROJEKT    = 'MIP'
                         AND KONTO_ART  = 'S')
                     END-SELECT
                  VALUE 5
                     RESET #HHL_SA #BUND_SA
                     SELECT SINGLE LFD_NR,BUND_STELLE
                       INTO #HHL_SA, #BUND_SA
                       FROM ZAP-TITEL
                      WHERE  VON_DATUM   <= #PR_ZAP_DAT
                        AND (#PR_ZAP_DAT <= BIS_DATUM
                          OR BIS_DATUM IS NULL)
                        AND (PROJEKT    = 'DZP'
                         AND KONTO_ART  = 'S')
                     END-SELECT
                  VALUE 6
                     RESET #HHL_SA #BUND_SA
                     SELECT SINGLE LFD_NR,BUND_STELLE
                       INTO #HHL_SA, #BUND_SA
                       FROM ZAP-TITEL
                      WHERE  VON_DATUM   <= #PR_ZAP_DAT
                        AND (#PR_ZAP_DAT <= BIS_DATUM
                          OR BIS_DATUM IS NULL)
                        AND (PROJEKT    = 'STK'
                         AND KONTO_ART  = 'S')
                     END-SELECT
                  VALUE 7
                     RESET #HHL_SA #BUND_SA
                     SELECT SINGLE LFD_NR,BUND_STELLE
                       INTO #HHL_SA, #BUND_SA
                       FROM ZAP-TITEL
                      WHERE  VON_DATUM   <= #PR_ZAP_DAT
                        AND (#PR_ZAP_DAT <= BIS_DATUM
                          OR BIS_DATUM IS NULL)
                        AND (PROJEKT    = 'ZBB'
                         AND KONTO_ART  = 'S')
                     END-SELECT
                  VALUE 8
                     RESET #HHL_SA #BUND_SA
                     SELECT SINGLE LFD_NR,BUND_STELLE
                       INTO #HHL_SA, #BUND_SA
                       FROM ZAP-TITEL
                      WHERE  VON_DATUM   <= #PR_ZAP_DAT
                        AND (#PR_ZAP_DAT <= BIS_DATUM
                          OR BIS_DATUM IS NULL)
                        AND (PROJEKT    = 'AGZ'
                         AND KONTO_ART  = 'S')
                     END-SELECT
                  VALUE 9
                     RESET #HHL_SA #BUND_SA
                     SELECT SINGLE LFD_NR,BUND_STELLE
                       INTO #HHL_SA, #BUND_SA
                       FROM ZAP-TITEL
                      WHERE  VON_DATUM   <= #PR_ZAP_DAT
                        AND (#PR_ZAP_DAT <= BIS_DATUM
                          OR BIS_DATUM IS NULL)
                        AND (PROJEKT    = 'KUA'
                         AND KONTO_ART  = 'S')
                     END-SELECT
                  NONE
                     WRITE 'falsches Projekt'
               END-DECIDE
               #NEG_BUND(#X) := #BUND_SA
               #NEG_NR(#X)   := #HHL_SA
               #NEG_ANZ(#X) := #NEG_ANZ(#X)  + #MOSA_ANZ(#U,#I,#X)
               #NEG_AUSZ(#X) :=
               #NEG_AUSZ(#X) + ( #MOSA_AUSZ(#U,#I,#X) * -1 )
            ELSE
**             #K := 5
               #K := 10
               #NEG_BUND(#K) := '1090683028'
               #NEG_NR(#K)   := 599
               #NEG_ANZ(#K) := #NEG_ANZ(#K)  + #MOSA_ANZ(#U,#I,#X)
               #NEG_AUSZ(#K) :=
               #NEG_AUSZ(#K) + ( #MOSA_AUSZ(#U,#I,#X) * -1 )
            END-IF
         END-IF
      END-FOR
   END-FOR
END-FOR
#K := 9
**#K := 5
*
FOR #I 1 50
   IF #HHL_AUSZ(#I) < 0
      ADD #HHL_AUSZ(#I) TO #SUM_BETRAG_VERR
      ADD 1 TO #K
      #NEG_BUND(#K) := #HHL_BUND(#I)
      #NEG_NR(#K)   := #HHL_NR(#I)
      #NEG_ANZ(#K)  := #HHL_ANZ(#I)
      #NEG_AUSZ(#K) := #HHL_AUSZ(#I)
   END-IF
END-FOR
IF #SUM_BETRAG_VERR < 0
   #SUM_BETRAG_VERR := #SUM_BETRAG_VERR * -1
END-IF
FOR #I 1 100
   IF #HHL_AUSZ(#I) < 0
      RESET #HHL_BEZUG #HHL_BEZ_TAB(*) #P
      SELECT HHL_BEZUG INTO #HHL_BEZUG
        FROM ZAP-BZHHL
       WHERE PROJEKT = #PROJEKT
         AND JAHR    = #JAHR
         AND HHL     = #HHL_NR(#I)
       ORDER BY HHL_BEZUG
*
         #P := #P + 1
         #HHL_BEZ_TAB(#P) := #HHL_BEZUG
*
      END-SELECT
*       SELECT SINGLE MIN(HHL_BEZUG) INTO #HHL_BEZUG
*         FROM ZAP-BZHHL
*        WHERE PROJEKT = #PROJEKT
*          AND JAHR    = #JAHR
*          AND HHL     = #HHL_NR(#I)
*       END-SELECT
      IF #HHL_BEZ_TAB(1) > 0
*       IF #HHL_BEZUG > 0
        RESET #MAX_NR
        FOR #J 1 100
          IF #HHL_AUSZ(#J) > 0 AND
             #HHL_NR(#J) = #HHL_BEZ_TAB(1)
*             (#HHL_NR(#J) = #HHL_BEZ_TAB(1) OR
*              #HHL_NR(#J) = #HHL_BEZ_TAB(2) OR
*              #HHL_NR(#J) = #HHL_BEZ_TAB(3))
*           IF #HHL_AUSZ(#J) > 0 AND #HHL_NR(#J) = #HHL_BEZUG
            #MAX_NR   := #J
            ESCAPE BOTTOM
          END-IF
        END-FOR
*
        IF #MAX_NR = 0
          FOR #J 1 100
            IF #HHL_AUSZ(#J) > 0 AND
               #HHL_NR(#J) = #HHL_BEZ_TAB(2)
              #MAX_NR   := #J
              ESCAPE BOTTOM
            END-IF
          END-FOR
        END-IF
*
        IF #MAX_NR = 0
          FOR #J 1 100
            IF #HHL_AUSZ(#J) > 0 AND
               #HHL_NR(#J) = #HHL_BEZ_TAB(3)
              #MAX_NR   := #J
              ESCAPE BOTTOM
            END-IF
          END-FOR
        END-IF
*
        #HHL_BUND(#I)      := #HHL_BUND(#MAX_NR)
        #HHL_NR(#I)        := #HHL_NR(#MAX_NR)
        #HHL_AUSZ(#MAX_NR) := #HHL_AUSZ(#MAX_NR) + #HHL_AUSZ(#I)
      ELSE
        RESET #MAX_AUSZ #MAX_NR
        FOR #J 1 100
          IF #HHL_AUSZ(#J) > 0
            IF #HHL_AUSZ(#J) > #MAX_AUSZ
              #MAX_AUSZ := #HHL_AUSZ(#J)
              #MAX_NR   := #J
            END-IF
          END-IF
        END-FOR
        IF #MAX_AUSZ > (#HHL_AUSZ(#I) * -1)
           #HHL_BUND(#I)      := #HHL_BUND(#MAX_NR)
           #HHL_NR(#I)        := #HHL_NR(#MAX_NR)
           #HHL_AUSZ(#MAX_NR) := #HHL_AUSZ(#MAX_NR) + #HHL_AUSZ(#I)
        END-IF
      END-IF
   END-IF
END-FOR
*
#SUM_BETRAG_POS  := #SUMME_IN_EURO + #SUM_BETRAG_VERR
*
FOR #S0 100 1 -1
   IF #HHL_NR(#S0) > 0
      ESCAPE BOTTOM
   END-IF
END-FOR
**++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
** UMSORTIEREN FüR AUSGABE
**++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
FOR #S1 = 1 TO #S0
  #S4 := #S1 + 1
  FOR #S2 = #S4 TO #S0
    /*------------------------------------------------------------
    /* ASCENDING: <  , DESCENDING >
    /*------------------------------------------------------------
    IF #HHL_NR(#S2) >= #HHL_NR(#S1)
       MOVE #HHL_NR(#S2)   TO #S_HHL_NR    /* RETTE WERT(#S2)
       MOVE #HHL_BUND(#S2) TO #S_HHL_BUND
       MOVE #HHL_ANZ(#S2)  TO #S_HHL_ANZ
       MOVE #HHL_AUSZ(#S2) TO #S_HHL_AUSZ
       #S5 := #S2 - 1
       FOR #S3 = #S5 TO #S1 STEP -1      /* SCHIEB I BIS J-1 HOCH
          MOVE #HHL_NR(#S3)   TO #HHL_NR(#S3 + 1)
          MOVE #HHL_BUND(#S3) TO #HHL_BUND(#S3 + 1)
          MOVE #HHL_ANZ(#S3)  TO #HHL_ANZ(#S3 + 1)
          MOVE #HHL_AUSZ(#S3) TO #HHL_AUSZ(#S3 + 1)
       END-FOR
       MOVE #S_HHL_NR   TO #HHL_NR(#S1)/*ALTEN WERT(#S2) AUF #S1
       MOVE #S_HHL_BUND TO #HHL_BUND(#S1)
       MOVE #S_HHL_ANZ  TO #HHL_ANZ(#S1)
       MOVE #S_HHL_AUSZ TO #HHL_AUSZ(#S1)
    END-IF
  END-FOR
END-FOR
FOR #S1 = 1 TO 100
   IF #HHL_NR(#S1) > 0 AND #HHL_AUSZ(#S1) < 0
      #S3 := #S1+1
      FOR #S2 = #S3 TO 100
         IF #HHL_NR(#S2) = #HHL_NR(#S1) AND #HHL_AUSZ(#S2) < 0
            #HHL_ANZ(#S1)  := #HHL_ANZ(#S1)  + #HHL_ANZ(#S2)
            #HHL_AUSZ(#S1) := #HHL_AUSZ(#S1) + #HHL_AUSZ(#S2)
            RESET #HHL_BUND(#S2) #HHL_NR(#S2)
                  #HHL_ANZ(#S2)  #HHL_AUSZ(#S2)
         END-IF
      END-FOR
   END-IF
END-FOR
FOR #I 1 100
   IF #HHL_AUSZ(#I) > 0
      ADD 1 TO #SUM_OBJ
   END-IF
   IF #HHL_AUSZ(#I) < 0
      ADD 1 TO #SUM_OBJ_VERR
   END-IF
   IF #I < 11
      IF #MOSA_AUSZ(*,#I,*) > 0
         ADD 1 TO #SUM_OBJ_VERR
      END-IF
   END-IF
END-FOR
**----------------------------------------------------------------
** Schreiben der Sammelanordnung
**----------------------------------------------------------------
FORMAT(1) PS=80 LS=90
*
* zweifaches Ausdrucken des F13 !!!!!
*
FOR #F13_COUNT 1 2
NEWPAGE (1)
WRITE (1) NOTITLE
          /
          5T  'BAYERISCHES STAATSMINISTERIUM'
          38T 'BBBB    BB  BBB  BBBBBB'
          67T 'Eingang Kasse'
          /
          5T  'FUER ERNAEHRUNG, LANDWIRT-'
          38T 'B      B B B   B     B'
          67T 'Datum Uhrzeit'
          /
          5T  'SCHAFT UND FORSTEN'
          38T 'BBB   B  B     B    B'
          /
          38T 'B        B   BBB   B'
          /
          38T 'B        B     B  B'
*
#H_FI := 5
#H_FO := 6
RESET #H_DATUM
CALLNAT '#DATDREN' #P_BEREICH1.#ZAP_DAT #H_DATUM #H_FI #H_FO
*
WRITE (1) 5T
          'AN DIE'
          38T 'B        B B   B  B'
          /
          5T
          #A_NAME_ZAHLER
          38T 'B        B  BBB  BBBBBBB'
*
WRITE (1) //
          8T
          'Datenblatt zur elektronischen Einreichung von'
          'Zahlungsgsdateien'
WRITE (1) 8T
          'Buchungs- und Zahlungsdatei  F 13  übermittelt am'
          #H_DATUM
WRITE (1) /
          8T
          'Haushaltsjahr'
           #HH_JAHR
         42T
          'Bewirtschafternummer ' '03063302'
*
WRITE (1) /
          8T
          'Kennzeichen der F13-Buchungsdatei'
          42T
          #BUCH_BAND_KENN
*
WRITE (1) 8T
          'Kennzeichen der F13-Zahlungsdatei'
          42T
          #A_NEUE_BANDKENN
*
WRITE (1) 8T
          'Kennzeichen Dateiart'
          42T
          #A_KZ
*
IF #F13Z_ART = 'N'
   WRITE (1) /
          8T  'N E U A U F N A H M E'
ELSE
   WRITE (1) /
          8T  'ACHTUNG: D E A K T I V I E R U N G'
END-IF
*
*
MOVE EDITED KASSE.AUSZAHL_BLZ (EM=99999999)
  TO #KASSE_A8
*
WRITE (1) /
          8T
          'Bankleitzahl des Einreichers'
          42T
          #KASSE_A8
*
MOVE LEFT JUSTIFIED KASSE.AUSZAHL_KONTO
  TO #A10
*
WRITE (1) 8T
          'Kontonummer des Einreichers'
          42T
          #A10
*
WRITE (1) /
          8T
          'Erstellungsdatum'
          42T
          #P_BEREICH1.#ZAP_DAT
*
WRITE (1) 8T
          'Ausführungsdatum'
          42T
          #P_BEREICH1.#AUSFUER_DAT
* *
IF #F13_COUNT = 1
* *
RESET #TO_COUNT
DECIDE ON FIRST VALUE OF #SUM_BETRAG_VERR
   VALUE 0
      #TO_COUNT := 1
   NONE
      #TO_COUNT := 2
END-DECIDE
*
RESET #P7
RESET #P11_2_B
RESET #P11_2_G
RESET #H10_1
*
FOR #POS_NEG 1 #TO_COUNT
*
   DECIDE ON FIRST VALUE OF #POS_NEG
      VALUE 1
         /*-----------------------------------------------------
         /* Schreiben der 2-Sätze - Positiv
         /*-----------------------------------------------------
         FOR #I 1 100
            RESET #SATZ2_ANZ #SATZ2_BETRAG
            IF #HHL_NR(#I) > 0 AND #HHL_AUSZ(#I) ^= 0
               DECIDE ON FIRST VALUE OF #PROJEKT
                 VALUES 'ZDI' , 'ZMV' , 'EMV' , 'EWB'
                   RESET #VAIF_HHL
                   SELECT HHL INTO #VAIF_HHL FROM FOE-AIFPR
                    WHERE PROJEKT = #DB2_PROJEKT
                      AND JAHR    = #DB2_JAHR
                      AND DAYS(INS_TS) BETWEEN DAYS(#DB2_ZAPDAT) - 10
                                           AND DAYS(#DB2_ZAPDAT)
* *                       AND DATE(INS_TS) = #DB2_ZAPDAT
                   END-SELECT
*
                   RESET TITEL
                   SELECT SINGLE * INTO VIEW TITEL FROM ZAP-TITEL
                    WHERE LFD_NR      = #VAIF_HHL
                      AND VON_DATUM   <= #DB2_ZAPDAT
                      AND (#DB2_ZAPDAT <= BIS_DATUM
                        OR BIS_DATUM IS NULL)
                      AND (PROJEKT    = #DB2_PROJEKT
                        OR PROJEKT    = ' ')
                   END-SELECT
                 NONE
                   RESET TITEL
                   SELECT SINGLE * INTO VIEW TITEL FROM ZAP-TITEL
                    WHERE LFD_NR      = #HHL_NR(#I)
                      AND VON_DATUM   <= #DB2_ZAPDAT
                      AND (#DB2_ZAPDAT <= BIS_DATUM
                        OR BIS_DATUM IS NULL)
                      AND (PROJEKT    = #DB2_PROJEKT
                        OR PROJEKT    = ' ')
                   END-SELECT
               END-DECIDE
*
               IF BUND_STELLE > ' '
                  ADD 1 TO #ANZ_MAX_BUCHSATZ
                  ADD 1 TO #P7
                  #PRINT_AW := '+'
                  RESET #SUMME_MOSA
                  RESET #ANZ_MOSA
                  IF #HHL_AUSZ(#I) > 0
                    FOR #V 1 2
                       FOR #W 1 10
                          FOR #X 1 9
                             IF #MOSA_NR(#V,#W,#X) = #HHL_NR(#I)
                               #SATZ2_ANZ  := #MOSA_ANZ(#V,#W,#X)
                               #SATZ2_BETRAG := #MOSA_AUSZ(#V,#W,#X)
                               #P11_2_G := #P11_2_G +
                                           #MOSA_AUSZ(#V,#W,#X)
                               IF #SATZ2_BETRAG ^= 0
*
                                 #SATZ2_BETRAG := #SATZ2_BETRAG * -1
                                 PERFORM   SATZ-2_SCHREIBEN
                                 ADD 1 TO #ANZ_MAX_BUCHSATZ
                                 ADD 1 TO #P7
                               END-IF
                             END-IF
                          END-FOR
                       END-FOR
                    END-FOR
                  END-IF
*
                  #SATZ2_ANZ    := #HHL_ANZ(#I)
                  #SATZ2_BETRAG := #HHL_AUSZ(#I)
                  #P11_2_G      := #P11_2_G + #HHL_AUSZ(#I)
                  PERFORM SATZ-2_SCHREIBEN
               END-IF
            END-IF
         END-FOR
      VALUE 2
         /*-----------------------------------------------------
         /* Schreiben der 2-Sätze - Negativ
         /*-----------------------------------------------------
         FOR #I 1 20
            IF #NEG_NR(#I) > 0 AND #NEG_AUSZ(#I) ^= 0
               RESET TITEL
               SELECT * INTO VIEW TITEL FROM ZAP-TITEL
                WHERE LFD_NR        = #NEG_NR(#I)
                  AND VON_DATUM    <= #DB2_ZAPDAT
                  AND (#DB2_ZAPDAT <= BIS_DATUM
                    OR BIS_DATUM IS NULL)
                  AND (PROJEKT    = #DB2_PROJEKT
                    OR PROJEKT    = 'KPR'
                    OR PROJEKT    = 'SFP'
                    OR PROJEKT    = 'RIP'
                    OR PROJEKT    = 'MIP'
                    OR PROJEKT    = 'STK'
                    OR PROJEKT    = 'ZBB'
                    OR PROJEKT    = 'AGZ'
                    OR PROJEKT    = 'KUA'
                    OR PROJEKT    = ' ')
*
               END-SELECT
               IF BUND_STELLE > ' '
                  ADD 1 TO #ANZ_MAX_BUCHSATZ
                  ADD 1 TO #P7
                  #PRINT_AW := '-'
                  #SATZ2_ANZ    := #NEG_ANZ(#I)
                  #SATZ2_BETRAG := #NEG_AUSZ(#I)
                  #P11_2_G      := #P11_2_G + #HHL_AUSZ(#I)
                  PERFORM SATZ-2_SCHREIBEN
               END-IF
            END-IF
         END-FOR
      NONE
         IGNORE
   END-DECIDE
END-FOR
* *
END-IF
* *
MOVE EDITED #H_SUM_CSATZ  (EM=ZZZZZ9)
  TO #H6
*
WRITE (1) /
          8T
          'Anzahl Zahlungsdatensätze (C)'
          46T
          #H6
*
#E6 := #P7 + 2
MOVE EDITED #E6 (EM=ZZZZZ9) TO #H6
*
WRITE (1) 8T
          'Anzahl Buchungsdatensätze (C)'
          46T
          #H6
*
MOVE EDITED #H_SUMME_IN_EURO_BAND  (EM=**ZZZ.ZZZ.ZZZ.ZZ9,99**)
  TO #H22
*
WRITE (1) /
          8T
          'Gesamtbetrag der Zahlungssätze'
          42T
          #H22
*
MOVE EDITED #SUMME_51000  (EM=**ZZZ.ZZZ.ZZZ.ZZ9,99**)
  TO #H22
WRITE (1) 8T
          '-  davon Auszahlungen'
          42T
          #H22
*
WRITE (1) 8T
          '-  davon Erstattungen'
*
MOVE EDITED #SUMME_53100           (EM=**ZZZ.ZZZ.ZZZ.ZZ9,99**)
  TO #H22
*
WRITE (1) 8T
          'Summe der Verrechnungen - E -'
          42T
          #H22
*
MOVE EDITED #SUMME_54400           (EM=**ZZZ.ZZZ.ZZZ.ZZ9,99**)
  TO #H22
*
WRITE (1) 8T
          'Summe der Verrechnungen - A -'
          42T
          #H22
*
WRITE (1) /
          8T
          'Kontrollsumme der Bankleitzahlen'
          46T
          #H_SUM_BLZ
WRITE (1) 8T
          'Kontrollsumme der Kontonummern'
          46T
          #H_SUM_KONTO
WRITE (1) /
          5T
          'Die hier in summarische Form dargestellten'
         55T
          'Die Zahlungs-/ und Buchungsdatei'
          /
          5T
          'Einzelzahlungen sind einzeln entsprechend'
         55T
          'wurden mit dokumentierten,'
          /
          5T
          'den Angaben der übermittelten Zahlungsdatei'
         55T
          'freigegebenen und gültigen'
          /
          5T
          'auszuführen und summarisch über die in der'
         55T
          'Programmen erstellt'
          /
          5T
          'Buchungsdatei enthaltenen Daten zu buchen.'
         55T
          '(Versions-Nr. 02.01).'
          /
          5T
          'Die Zahlliste wurde erstellt und ent-'
         55T
          'Bei der Verarbeitung und'
          /
          5T
          'sprechend der Aufbewahrungsbestimmungen zum'
         55T
          'Übermittlung der Daten traten'
          /
          5T
          'Zwecke der Rechnungsprüfung archiviert'
         55T
          'keine Störungen auf.'
*
WRITE (1) /
          31T
          'Sachlich richtig'
         55T
          'Im Auftrag'
*
WRITE (1) /
          31T
          '________________'
*
WRITE (1)
          33T
          'Unterschrift'
          55T
          #P_BEREICH1.#ZAP_DAT
          67T
          '______________________'
*
WRITE (1)
          5T
          'Im Auftrag'
         55T
          'Datum, Unterschrift'
WRITE (1)
         55T
          '(Beauftragter im Bereich DV)'
*
WRITE (1) /
          5T
          '__________________________________________'
*
WRITE (1)
          5T
          'Datum, Unterschrift des Anordnungsbefugten'
*
COMPRESS '=================================================='
         '=========================='
    INTO #H76 LEAVING NO
*
WRITE (1)
          5T
          #H76
*
WRITE (1)
          5T
          'Vermerk der Bundeskasse'
*
WRITE (1) /
          5T
          'Die o.a. Datei wurde freigeschaltet. Es traten'
         52T
          'keine Fehler auf. Die Anordnungs-'
          /
          5T
          'befugnis wurde geprüft und liegt vor.'
#H42 := '__________________________________________'
*
WRITE (1) /
          41T
          #H42
*
#H42 := 'Datum, Unterschrift des Kassenbediensteten'
*
WRITE (1)
          41T
          #H42
* *
END-FOR
* *
*
IF #P_BEREICH1.#MODUS = 'PROD'
  UPDATE ZAP-FOESZ
  SET BANDKENN         = #NEUE_BANDKENN ,
      ANZ_BETR         = #H_SUM_CSATZ  ,
      SUM_AUSZ         = #H_SUMME_IN_EURO_BAND ,
      SUM_EU           = #H_SUMME_IN_EURO_BAND ,
      SUM_BAY          = 0 ,
      SUM_LA           = 0 ,
      SUM_BU           = 0 ,
      SUM_SO           = 0 ,
      TAG_MAX_AUSZ     = #ANZ_MAX_AUSZ ,
      TAG_MAX_BUCHSATZ = #ANZ_MAX_BUCHSATZ
    WHERE PROJEKT = #DB2_PROJEKT
    AND JAHR      = #DB2_JAHR
    AND ZAP_DAT   = #DB2_ZAPDAT
    AND AUSZ_ART  = #P_BEREICH1.#TYP
END-IF
*
********************************************************************
* Kopieren der ZR-Sätze in die Buchungsdatei
********************************************************************
READ WORK FILE 4 RECORD #ZR_STRUKTUR
  WRITE WORK FILE 3 VARIABLE #ZR_STRUKTUR
END-WORK
*******************************************************************
* Kopieren der ZL-Sätze in die Buchungsdatei
*******************************************************************
**READ WORK FILE 5 RECORD #ZL_STRUKTUR
**  WRITE WORK FILE 3 VARIABLE #ZL_STRUKTUR
**END-WORK
************************************************************************
* Schreiben des 9-Satzes
************************************************************************
#9_STRUKTUR.#9_SK        := 9
#9_HHJJJJ   := #HH_JAHR
#N_NEUE_BANDKENN := #NEUE_BANDKENN
#9_BAND_KZ  := #BUCH_BAND_KENN
#9_BEWIRT   := 3063302
*
RESET #E8
MOVE EDITED #9_BEWIRT  (EM=99999999) TO #E8
#9_BEWIRT        := #E8_N
#E5              := #P7 + 2
MOVE EDITED #E5 (EM=99999)
  TO #9_FAELLE_OHNE_Z
*
#E14 := (#SUMME_51000 + #SUMME_53100 + #SUMME_54400) * 100
MOVE EDITED #E14 (EM=99999999999999) TO #9_GESAMT
*
#E14  := #H_SUMME_IN_EURO_BAND * 100
MOVE EDITED #E14 (EM=99999999999999)
  TO #9_SUMME_KENNUNG_E
*
#9_SUMME_ERSTATTUNG := '00000000000000'
*
#9_VOR_BLZ   := '00000'
MOVE EDITED #H_SUM_BLZ            (EM=999999999999999)
         TO #9_PRF_BLZ
#9_VOR_KONTO := '00000'
MOVE EDITED #H_SUM_KONTO          (EM=999999999999999)
         TO #9_PRF_KONTO
*
#E10_NN          := #H_SUM_CSATZ
MOVE EDITED #E10_NN (EM=9999999999)
  TO #9_ANZ_ZAHLSAETZE
*
#E14 := #SUMME_53100 * 100
MOVE EDITED #E14 (EM=99999999999999) TO #9_SUMME_VERRECHNUNG_E
*
#E14 := #SUMME_54400 * 100
MOVE EDITED #E14 (EM=99999999999999) TO #9_SUMME_VERRECHNUNG_A
*
#E10_NN := #SATZANZ
MOVE EDITED #E10_NN (EM=9999999999)
  TO #9_ANZ_ZAHLLISTSAETZE
*
WRITE WORK FILE 3 VARIABLE #9_STRUKTUR
*
PERFORM KONTROLL_BERICHT_FUSS
*
PERFORM ZAHLLISTE_FUSS
*
**--------------------------------------------------------------------
**
** Subroutine zur Verarbeitung der Sonderfälle aus ZAP-PFAEN
**
**--------------------------------------------------------------------
DEFINE SUBROUTINE SONDERFAELLE_AUS_PFAEN
   #DB2_MOSA := ' '
   RESET #GES_BN_MOSA
   RESET #SUM_MJSA
   SELECT SINGLE SUM(KUERZ_BETRAG_HHL) INTO #SUM_MJSA
     FROM ZAP-ZABUHHL
    WHERE PROJEKT  = #DB2_PROJEKT
      AND JAHR     = #DB2_JAHR
      AND BETRNR   = #VGL_BETRNR
      AND ABRNUM   = #VGL_ABRNUM
   END-SELECT
*
   IF #SUM_MJSA > 0
      #GES_BN_MOSA := #GES_BN_MOSA + #SUM_MJSA
*
      #H_SUMME_IN_EURO_BAND := #H_SUMME_IN_EURO_BAND - #SUM_MJSA
   END-IF
*
   #DB2_MOSA := ' '
   RESET #SUM_MOSA
   SELECT SINGLE SUM(KUERZ_BETRAG_HHL) INTO #SUM_MOSA
     FROM ZAP-MOSA2
    WHERE PROJEKT  = #DB2_PROJEKT
      AND JAHR     = #DB2_JAHR
      AND BETRNR   = #VGL_BETRNR
      AND ABRNUM   = #VGL_ABRNUM
      AND MOSA_TYP = 'MO'
   END-SELECT
*
   IF #SUM_MOSA > 0
      #GES_BN_MOSA := #GES_BN_MOSA + #SUM_MOSA
*
      #H_SUMME_IN_EURO_BAND := #H_SUMME_IN_EURO_BAND - #SUM_MOSA
   END-IF
   #DB2_MOSA := ' '
*
   #U_PFAEND   := ABRJJ_SATZ.PFAEND
   #U_NAME     := ABRJJ_SATZ.NAME
   #U_BLZ      := ABRJJ_SATZ.BLZ
   #U_KONTO    := ABRJJ_SATZ.KONTO
   #U_BANKNAME := ABRJJ_SATZ.BANKNAME
   #L_NAME     := ABRJJ_SATZ.NAME
   #L_BLZ      := ABRJJ_SATZ.BLZ
*
   IF ABRJJ_SATZ.PFAEND > 0
      RESET #PF_STRUK
      IF #P_BEREICH1.#PROJEKT = 'OSA'
         #PF_PROJEKT  := 'KPR'
      ELSE
         #PF_PROJEKT  := #P_BEREICH1.#PROJEKT
      END-IF
      #PF_BETRNR   := #VGL_BETRNR
      #PF_JAHR     := #DB2_JAHR
      #PF_STICHTAG := #P_BEREICH1.#ZAP_DAT
      CALLNAT '#PFAE06N' #PF_STRUK
      /**************************************************************
      /* Aufrechnung, Pfändung, Vollstreckung oder Abtretung ?
      /**************************************************************
      DECIDE ON FIRST VALUE OF #PF_RC
         /***********************************************************
         /* keine Aufrechnung/Pfändung/Vollstreckung/Abtretung gef.
         /***********************************************************
         VALUE 0
            #U_NAME     := ABRJJ_SATZ.NAME
            #U_BLZ      := ABRJJ_SATZ.BLZ
            #L_BLZ      := ABRJJ_SATZ.BLZ
            #U_KONTO    := ABRJJ_SATZ.KONTO
            #U_BANKNAME := ABRJJ_SATZ.BANKNAME
**       VALUE 6
**          #U_NAME     := ABRJJ_SATZ.NAME
**          #U_BLZ      := ABRJJ_SATZ.BLZ
**          #L_BLZ      := ABRJJ_SATZ.BLZ
**          #U_KONTO    := ABRJJ_SATZ.KONTO
**          #U_BANKNAME := ABRJJ_SATZ.BANKNAME
         /***********************************************************
         /* Aufrechnung/Pfändung/Vollstreckung/Abtretung gefunden
         /***********************************************************
         NONE
            #U_NAME     := #PF_NAME
            #U_BLZ      := #PF_BLZ
            #L_BLZ      := #PF_BLZ
            #U_KONTO    := #PF_KONTO
            #U_BANKNAME := #PF_BANKNAME
      END-DECIDE
   END-IF
*
   RESET #AUFRECH_IN_PFAEN
         #SUM_AUFRECH
         #AN_LANDWIRT
         #KEINE_AUFRECH
*
   /*****************************************************************
   /* Gibt es eine aktive Aufrechung ?
   /*****************************************************************
   SELECT SINGLE COUNT(*) INTO #AUFRECH_IN_PFAEN FROM ZAP-PFAEN
    WHERE BETRNR = #VGL_BETRNR
      AND PROJEKT = #DB2_PROJEKT
      AND BESCHART   =  1
      AND (STORNDAT > #P_BEREICH1.#ZAP_DAT
        OR STORNDAT IS NULL)
      AND ( REGNR LIKE '0801%' OR REGNR LIKE '9130%')
   END-SELECT
*
   DECIDE ON FIRST VALUE OF #AUFRECH_IN_PFAEN
      /**************************************************************
      /* keine aktive Aufrechung gefunden - Satz schreiben
      /**************************************************************
      VALUE 0
         RESET #C_STRUKTUR
         RESET #DB2_BKZ
*
         #C_STRUKTUR.#C_KENNZ_BETRAG    := '1'
         #H_BETRAG_EURO                 := (#SUM_ABRECH - #GES_BN_MOSA)
         #N11  := #H_BETRAG_EURO * 100
         MOVE EDITED #N11 (EM=99999999999)
           TO #C_STRUKTUR.#C_BETRAG_IN_EURO
         #C_STRUKTUR.#C_BETRAG          := '00000000000'
*
         IF #H_BETRAG_EURO > 0
            PERFORM C_SATZ_SCHREIBEN
         END-IF
*
         ADD #SUM_ABRECH TO #SUMME_IN_EURO
         ADD #SUM_ABRECH TO #H_SUMME_IN_EURO_BAND
      /**************************************************************
      /* aktive Aufrechung gefunden
      /* - Nachlesen der Aufrechnungsdatenb aus ZAP-AUFRE
      /* - Nachlesen des Buchungskennzeichens aus der ZAP-KAS
      /**************************************************************
      NONE
         RESET AUFRE
         SELECT * INTO VIEW AUFRE FROM ZAP-AUFRE
          WHERE AUSZ_PROJ    = #DB2_PROJEKT
            AND BETRNR       = #VGL_BETRNR
            AND AUSZ_JAHR    = #VGL_JAHR
            AND AUSZ_ABRNUM  = #VGL_ABRNUM
            AND AUFR_BETRAG  > 0
            AND IST_GESTELLT IN (#WERT1,#WERT2)
          ORDER BY INS_TS ASC
         WITH HOLD
*
            IF NO
               #KEINE_AUFRECH := 1
               WRITE 'Keine Aufrechnung bei' #VGL_BETRNR
*
               #C_STRUKTUR.#C_KENNZ_BETRAG    := '1'
               #H_BETRAG_EURO :=
                  (#SUM_ABRECH - #GES_BN_MOSA)
               #N11  := #H_BETRAG_EURO * 100
               MOVE EDITED #N11 (EM=99999999999)
                 TO #C_STRUKTUR.#C_BETRAG_IN_EURO
               #C_STRUKTUR.#C_BETRAG          := '00000000000'
*
               /*****************************************************
               /* Gibt es sonst was in der ZAP-PFAEN ?
               /*****************************************************
               RESET #SONST_IN_PFAEN
               SELECT SINGLE COUNT(*) INTO #SONST_IN_PFAEN
                 FROM ZAP-PFAEN
                WHERE BETRNR  = #VGL_BETRNR
                  AND PROJEKT = #DB2_PROJEKT
                  AND BESCHART NOT IN ( 1 , 6 )
                  AND (STORNDAT > #P_BEREICH1.#ZAP_DAT
                    OR STORNDAT IS NULL)
               END-SELECT
*
               DECIDE ON FIRST VALUE OF #SONST_IN_PFAEN
                  VALUE 0
                     #U_PFAEND := 0
*
                     RESET #DB2_BKZ
**##
** ##                PERFORM BETRIEB_LESEN
** ##                #U_NAME             := #BETRDAT.#NAME
** ##                #U_PFAEND           := 0
** ##
                  NONE
                     RESET #U_PFAEND
                     SELECT SINGLE MIN(BESCHART) INTO #U_PFAEND
                       FROM ZAP-PFAEN
                      WHERE BETRNR = #VGL_BETRNR
                        AND PROJEKT = #DB2_PROJEKT
                        AND BESCHART NOT IN ( 1 , 6 )
                        AND (STORNDAT > #P_BEREICH1.#ZAP_DAT
                          OR STORNDAT IS NULL)
                     END-SELECT
               END-DECIDE
               /*****************************************************
               /* UPDATE FOE.ABRJJ when keine Aufrechnung
               /*****************************************************
               IF #MODUS = 'PROD'
                  UPDATE FOE-ABRJJ
                     SET PFAEND   = #U_PFAEND ,
                         BLZ      = #U_BLZ ,
                         KONTO    = #U_KONTO ,
                         BANKNAME = #U_BANKNAME
                   WHERE PROJEKT = #DB2_PROJEKT
                     AND BETRNR  = #VGL_BETRNR
                     AND JAHR    = #VGL_JAHR
                     AND STATUS  BETWEEN #DB2_STATUS_1
                                     AND #DB2_STATUS_2
                     AND ABRNUM  = #VGL_ABRNUM
                     AND ZENTRAL > 0
               END-IF
*
               IF #H_BETRAG_EURO > 0
                  PERFORM C_SATZ_SCHREIBEN
               END-IF
*
               ESCAPE BOTTOM
            END-NOREC
*
            RESET #DB2_BKZ
            SELECT SINGLE DISTINCT BKZ INTO #DB2_BKZ
              FROM ZAP-KAS
             WHERE PROJEKT      = AUFRE.PROJEKT
               AND BETRNR       = AUFRE.BETRNR
               AND JAHR         = AUFRE.JAHR
               AND ABRNUM       = AUFRE.ABRNUM
            END-SELECT
*
            RESET #C_STRUKTUR
*
            #SUM_AUFRECH     := #SUM_AUFRECH + AUFRE.AUFR_BETRAG
            #AN_LANDWIRT     := AUFRE.AUSZ_REST
*
            #C_STRUKTUR.#C_KENNZ_BETRAG    := '1'
            #H_BETRAG_EURO                 := AUFRE.AUFR_BETRAG
            #N11  := #H_BETRAG_EURO * 100
            MOVE EDITED #N11 (EM=99999999999)
              TO #C_STRUKTUR.#C_BETRAG_IN_EURO
            #C_STRUKTUR.#C_BETRAG          := '00000000000'
*
*
            IF SUBSTR(#DB2_BKZ,1,4) = '9130'
               #U_NAME      := 'BUNDESKASSE TRIER'
               #U_BLZ       := 59000000
               #L_BLZ       := 59000000
               #U_KONTO     := '0059001020'
               #U_BANKNAME  := 'DEUTSCHE BUNDESBANK TRIER'
            ELSE
               #U_NAME      := 'STAATSOBERK.BAYERN IN LANDSHUT'
               #U_BLZ       := 70050000
               #L_BLZ       := 70050000
               #U_KONTO     := '0001190315'
               #U_BANKNAME  := 'BAYER. LANDESBANK MUENCHEN'
            END-IF
*
            IF #H_BETRAG_EURO > 0
*
* blanko-ZR-Satz schreiben, damit Zahl der ZR = Zahl der C
*
               RESET #MAX_HHL
               SELECT T1.HHL INTO #MAX_HHL FROM ZAP-FOES T1
                WHERE T1.PROJEKT = #DB2_PROJEKT
                  AND T1.BETRNR  = #VGL_BETRNR
                  AND T1.JAHR    = #VGL_JAHR
                  AND T1.ABRNUM  = #VGL_ABRNUM
                  AND T1.ABRECH =
                  (SELECT MAX(T2.ABRECH) FROM ZAP-FOES T2
                   WHERE T1.PROJEKT = T2.PROJEKT
                     AND T1.BETRNR  = T2.BETRNR
                     AND T1.JAHR = T2.JAHR
                     AND T1.ABRNUM = T2.ABRNUM)
               END-SELECT
               RESET #ZR_STRUKTUR
               RESET #EINZEL_BETRAG
               RESET #EINZEL_ZAEHL
               #GRUPPE := 1
               #GRUPPE_ZAEHL := 1
               RESET #ZR_HILF
*
               DECIDE ON FIRST VALUE OF #PROJEKT
               VALUES 'ZDI' , 'ZMV' , 'EMV' , 'EWB'
                 RESET #VAIF_HHL
                 SELECT HHL INTO #VAIF_HHL FROM FOE-AIFPR
                  WHERE PROJEKT = #DB2_PROJEKT
                    AND JAHR    = #DB2_JAHR
                    AND DAYS(INS_TS) BETWEEN DAYS(#DB2_ZAPDAT) - 10
                                         AND DAYS(#DB2_ZAPDAT)
* *                     AND DATE(INS_TS) = #DB2_ZAPDAT
                 END-SELECT
*
                 RESET TITEL
                 SELECT SINGLE * INTO VIEW TITEL FROM ZAP-TITEL
                  WHERE LFD_NR      = #VAIF_HHL
                    AND VON_DATUM   <= #DB2_ZAPDAT
                    AND (#DB2_ZAPDAT <= BIS_DATUM
                      OR BIS_DATUM IS NULL)
                    AND (PROJEKT    = #DB2_PROJEKT
                      OR PROJEKT    = ' ')
                 END-SELECT
               NONE
                 RESET TITEL
                 SELECT SINGLE * INTO VIEW TITEL FROM ZAP-TITEL
                  WHERE LFD_NR      = #MAX_HHL
                    AND VON_DATUM   <= #DB2_ZAPDAT
                    AND (#DB2_ZAPDAT <= BIS_DATUM
                      OR BIS_DATUM IS NULL)
                    AND (PROJEKT    = #DB2_PROJEKT
                      OR PROJEKT    = ' ')
                 END-SELECT
               END-DECIDE
*
               IF BUND_STELLE > ' '
                  #ZR_HAUSHALT := VAL(BUND_STELLE)
               END-IF
*
               IF OBJ_KONTO > ' '
                  #ZR_OBJEKT := VAL(OBJ_KONTO)
                  MOVE EDITED #ZR_OBJEKT (EM=9999999999)
                    TO #E10
                  #ZR_OBJEKT := #E10_N
               END-IF
*
               #KENNUNG      :=  ' '
               #ZR_KENNZ_E_A :=  'A'
*** ###        #SATZZR_BETRAG :=  AUFRE.AUFR_BETRAG
               #SATZZR_BETRAG :=  0
               #EINZEL_BETRAG :=  0
*** ###        #EINZEL_BETRAG :=  #SATZZR_BETRAG
*
*** ###        #RESTBETRAG_NACH_AUFRE := AUFRE.AUSZ_REST
*
               #E14  := #SATZZR_BETRAG * 100
               MOVE EDITED #E14 (EM=99999999999999)
                 TO #ZR_BETRAG
*
               #ZR_UNTERSTRUK(#GRUPPE_ZAEHL) := #ZR_HILF
*
               ADD 1 TO #BEZUG_ZK
               PERFORM SATZ-ZR_SCHREIBEN
               ADD 1 TO #SATZANZ
*
               PERFORM C_SATZ_SCHREIBEN
            END-IF
         END-SELECT
*
         /***********************************************************
         /* Ist noch eine Restauszahlungs übrig ?
         /* - Nachlesen des Namens und der Bankverbindung des Antrag-
         /*   stellers
         /***********************************************************
         IF #KEINE_AUFRECH = 0 AND
            #SUM_ABRECH - #GES_BN_MOSA - #SUM_AUFRECH > 0
*
            RESET #C_STRUKTUR
            RESET #DB2_BKZ
            #U_PFAEND := 0
*
            /*****************************************************
            /* Gibt es sonst was in der ZAP-PFAEN ?
            /*****************************************************
            RESET #SONST_IN_PFAEN
            SELECT SINGLE COUNT(*) INTO #SONST_IN_PFAEN
              FROM ZAP-PFAEN
             WHERE BETRNR = #VGL_BETRNR
               AND PROJEKT = #DB2_PROJEKT
               AND BESCHART NOT IN ( 1 , 6 )
               AND (STORNDAT > #P_BEREICH1.#ZAP_DAT
                 OR STORNDAT IS NULL)
            END-SELECT
*
            IF #SONST_IN_PFAEN > 0
               RESET #U_PFAEND
               SELECT SINGLE MIN(BESCHART) INTO #U_PFAEND
                 FROM ZAP-PFAEN
                WHERE BETRNR = #VGL_BETRNR
                  AND PROJEKT = #DB2_PROJEKT
                  AND BESCHART NOT IN ( 1 , 6 )
                  AND (STORNDAT > #P_BEREICH1.#ZAP_DAT
                    OR STORNDAT IS NULL)
               END-SELECT
*
               #U_NAME      := 'STAATSOBERK.BAYERN IN LANDSHUT'
               #U_BLZ       := 70050000
               #L_BLZ       := 70050000
               /*********************************************
               /* neues Konto für Staatsoberkasse ab 30.01.08
               /* Anforderung 2457
               /*********************************************
               /* #U_KONTO     := '0001190315'
               #U_KONTO     := '4501190315'
               #U_BANKNAME  := 'BAYER. LANDESBANK MUENCHEN'
            ELSE
  WRITE 'es gibt eine Restzahlung für' #VGL_BETRNR '=' #AN_LANDWIRT
               #U_NAME             := ABRJJ_SATZ.NAME
** ##
               PERFORM BETRIEB_LESEN
** ##
            END-IF
*
            #C_STRUKTUR.#C_KENNZ_BETRAG    := '1'
            #H_BETRAG_EURO                 := #AN_LANDWIRT
            #N11  := #H_BETRAG_EURO * 100
            MOVE EDITED #N11 (EM=99999999999)
              TO #C_STRUKTUR.#C_BETRAG_IN_EURO
            #C_STRUKTUR.#C_BETRAG          := '00000000000'
*
*
            IF #H_BETRAG_EURO > 0
               PERFORM C_SATZ_SCHREIBEN
            END-IF
         END-IF
         IF #KEINE_AUFRECH = 0 AND
            #SUM_ABRECH - #GES_BN_MOSA - #SUM_AUFRECH = 0
            #BEZUG_ZK := #BEZUG_ZK - 1
            #SATZANZ  := #SATZANZ  - 1
         END-IF
*
         ADD #SUM_ABRECH TO #SUMME_IN_EURO
         ADD #SUM_ABRECH TO #H_SUMME_IN_EURO_BAND
   END-DECIDE
END-SUBROUTINE
**--------------------------------------------------------------------
**
** Subroutine zum Schreiben der C-Sätze und Aufaddieren für E-Satz
**
**--------------------------------------------------------------------
DEFINE SUBROUTINE C_SATZ_SCHREIBEN
   RESET #C_ERWEIT
   RESET #N_ERWEIT
   RESET #NEU_ERWEIT
   RESET #C_ERWEIT_TAB(*)
   #C_STRUKTUR.#C_SA        := 'C'
   #C_STRUKTUR.#C_FELD_3    := '00000000'
   #C_STRUKTUR.#C_FELD_6    := '0000000000000'
*
   PERFORM KONTO_PRUEFUNG
*
   /**************************************************************
   /* Füllen der Hilfssummenfelder für E-Satz
   /**************************************************************
*
   ADD #KONTO TO #H_SUM_KONTO
   ADD #U_BLZ TO #H_SUM_BLZ
*
   MOVE EDITED #U_BLZ (EM=99999999)
     TO #C_STRUKTUR.#C_BLZ_EMPFG
   MOVE EDITED #KONTO (EM=9999999999)
     TO #C_STRUKTUR.#C_KONTO_EMPFG
*
   #C_STRUKTUR.#C_TEXT_SCHL   := 51000
*
   #AUS_BETRNR_N10          := #VGL_BETRNR
   COMPRESS #LLL #GGG #NNN INTO #AUS_BETRNR_A12
*
   MOVE EDITED KASSE.AUSZAHL_BLZ (EM=99999999)
     TO #C_STRUKTUR.#C_BLZ_KASSE
*
   #C_STRUKTUR.#C_KONTO_KASSE := KASSE.AUSZAHL_KONTO
*
   #C_STRUKTUR.#C_NAME_KASSE := KASSE.AUSZAHL_STELLE
*
   /********************************************************************
   /* Bei Aufrechungen wird nur der Name in das Empfängerfeld gesetzt
   /********************************************************************
   IF SUBSTR(#U_NAME,1,11) = 'STAATSOBERK' OR
      SUBSTR(#U_NAME,1,11) = 'BUNDESKASSE'
      #H_NAME      := #U_NAME
      EXAMINE #H_NAME     AND TRANSLATE INTO UPPER CASE
*
      FOR #N_E 1 3
        EXAMINE #H_NAME     FOR #K_UMLAUT_TAB(#N_E)
          AND REPLACE WITH #G_UMLAUT_TAB(#N_E)
      END-FOR
*
      COMPRESS #H_NAME
          INTO #C_STRUKTUR.#C_EMPFAENGER
   /********************************************************************
   /* Sonst Anschrift, so lange Platz reicht !!
   /********************************************************************
   ELSE
      #H_NAME      := ABRJJ_SATZ.NAME
      #H_STRASSE   := ABRJJ_SATZ.STRASSE
      #H_ORTSTEIL  := ABRJJ_SATZ.ORTSTEIL
      #H_ORT       := ABRJJ_SATZ.ORT
      EXAMINE #H_NAME     AND TRANSLATE INTO UPPER CASE
      EXAMINE #H_STRASSE  AND TRANSLATE INTO UPPER CASE
      EXAMINE #H_ORTSTEIL AND TRANSLATE INTO UPPER CASE
      EXAMINE #H_ORT      AND TRANSLATE INTO UPPER CASE
*
      FOR #N_E 1 3
        EXAMINE #H_NAME     FOR #K_UMLAUT_TAB(#N_E)
          AND REPLACE WITH #G_UMLAUT_TAB(#N_E)
        EXAMINE #H_STRASSE  FOR #K_UMLAUT_TAB(#N_E)
          AND REPLACE WITH #G_UMLAUT_TAB(#N_E)
        EXAMINE #H_ORTSTEIL FOR #K_UMLAUT_TAB(#N_E)
          AND REPLACE WITH #G_UMLAUT_TAB(#N_E)
        EXAMINE #H_ORT      FOR #K_UMLAUT_TAB(#N_E)
          AND REPLACE WITH #G_UMLAUT_TAB(#N_E)
      END-FOR
*
      IF #H_STRASSE > ' '
        COMPRESS #H_NAME ',' #H_STRASSE ',' #H_ORT
            INTO #C_STRUKTUR.#C_EMPFAENGER
      END-IF
      IF #H_STRASSE = ' ' AND #H_ORTSTEIL > ' '
        COMPRESS #H_NAME ',' #H_ORTSTEIL ',' #H_ORT
            INTO #C_STRUKTUR.#C_EMPFAENGER
      END-IF
      IF #H_STRASSE = ' ' AND #H_ORTSTEIL = ' '
        COMPRESS #H_NAME ',' #H_ORT
            INTO #C_STRUKTUR.#C_EMPFAENGER
      END-IF
   END-IF
*
   FOR #N_E 1 27
      FOR #N_T 1 50
         IF #C_EMPF_TAB(#N_E) = #ERLAUBT_TAB(#N_T)
            ESCAPE BOTTOM
         END-IF
      END-FOR
      IF #N_T > 50
         #C_EMPF_TAB(#N_E) := ' '
      END-IF
   END-FOR
*
   DECIDE ON FIRST VALUE OF #PROJEKT
   VALUES 'ZDI' , 'ZMV' , 'EMV' , 'EWB'
     RESET #VAIF_HHL
     SELECT HHL INTO #VAIF_HHL FROM FOE-AIFPR
      WHERE PROJEKT = #DB2_PROJEKT
        AND JAHR    = #DB2_JAHR
        AND DAYS(INS_TS) BETWEEN DAYS(#DB2_ZAPDAT) - 10
                             AND DAYS(#DB2_ZAPDAT)
* *         AND DATE(INS_TS) = #DB2_ZAPDAT
     END-SELECT
*
     RESET TITEL
     SELECT SINGLE * INTO VIEW TITEL FROM ZAP-TITEL
      WHERE LFD_NR      = #VAIF_HHL
        AND VON_DATUM   <= #DB2_ZAPDAT
        AND (#DB2_ZAPDAT <= BIS_DATUM
          OR BIS_DATUM IS NULL)
        AND (PROJEKT    = #DB2_PROJEKT
          OR PROJEKT    = ' ')
     END-SELECT
   NONE
     RESET TITEL
     SELECT SINGLE * INTO VIEW TITEL FROM ZAP-TITEL
      WHERE LFD_NR      = #VGL_LFD
        AND VON_DATUM   <= #DB2_ZAPDAT
        AND (#DB2_ZAPDAT <= BIS_DATUM
          OR BIS_DATUM IS NULL)
        AND (PROJEKT    = #DB2_PROJEKT
          OR PROJEKT    = ' ')
     END-SELECT
   END-DECIDE
*
   #DR_HAUSHALT := BUND_STELLE
   COMPRESS #V1 #V2 #V3 INTO #H9 LEAVING NO
   /**************************************************************
   /* Ermittlung der aktuellen Auszahlungsnummer im Jahr
   /**************************************************************
   EXAMINE PROJ.K_TEXT  AND TRANSLATE INTO UPPER CASE
*
   FOR #N_E 1 3
     EXAMINE PROJ.K_TEXT  FOR #K_UMLAUT_TAB(#N_E)
       AND REPLACE WITH #G_UMLAUT_TAB(#N_E)
   END-FOR
*
   #N2 := FOESZ.LFD_ZAHL
   DECIDE ON FIRST VALUE OF #U_PFAEND
      VALUE 1
         COMPRESS 'BKZ:' #DB2_BKZ
             INTO #H_ZWECK
      VALUE 2 , 3 , 4 , 7 , 9
         COMPRESS #DB2_PROJEKT'/' #AUS_BETRNR_N10 '/' #JAHR '/'
                  #N2 '/' 'PFAE' INTO #H_ZWECK
         LEAVING NO
      NONE
         COMPRESS #DB2_PROJEKT'/' #AUS_BETRNR_N10 '/' #JAHR '/'
                  #N2 INTO #H_ZWECK
         LEAVING NO
   END-DECIDE
*
   #C_STRUKTUR.#C_ZWECK    := #H_ZWECK
   #N_ERWEIT               := 1
   #C_STRUKTUR.#C_ERWEIT   := '01'
   ADD 1 TO #LFD_NR
   ADD 1 TO #LFD_NR_ZL
*
   /*******************************************************************
   /* Schreiben des ZL-Satzes
   /*******************************************************************
   PERFORM SATZ-ZL_SCHREIBEN
*
   MOVE EDITED #LFD_NR (EM=999999) TO #H6
   COMPRESS 'A03063302' #V_TT #V_MM #H6
       INTO #H19 LEAVING NO;
   COMPRESS 'BUKATR' #H19
       INTO #C_STRUKTUR.#C_NAME_KASSE
*
   COMPRESS 'A0633020' #H9 #V_TT #V_MM #H6
       INTO #C_ERW_TEXT1 LEAVING NO
*
   #N_ERWEIT := 2
   #C_ERW_ART1 := '02'
   #C_ERWEIT := '02'
*
   /*******************************************************************
   /* Übertragung des Verwendungszwecks in das Erweiterungsfeld
   /*******************************************************************
   DECIDE ON FIRST VALUE OF #U_PFAEND
      VALUE 1
         COMPRESS 'AUFRECHNUNG BN' #AUS_BETRNR_A12
          INTO #C_ERW_TEXT2
      VALUE 2
         COMPRESS 'PFAENDUNG  BN' #AUS_BETRNR_A12
          INTO #C_ERW_TEXT2
      VALUE 3
         COMPRESS 'VERW.VOLL. BN' #AUS_BETRNR_A12
          INTO #C_ERW_TEXT2
      VALUE 4
         COMPRESS 'ABTRETUNG  BN' #AUS_BETRNR_A12
          INTO #C_ERW_TEXT2
      VALUE 5
         COMPRESS 'INSOLVENZ  BN' #AUS_BETRNR_A12
          INTO #C_ERW_TEXT2
      VALUE 9
         COMPRESS 'SONSTIGES  BN' #AUS_BETRNR_A12
          INTO #C_ERW_TEXT2
      NONE
         COMPRESS 'BETRIEBSNR.' #AUS_BETRNR_A12
          INTO #C_ERW_TEXT2
   END-DECIDE
   #C_ERW_ART2 := '02'
*
   #H_PROJ_NAME := SUBSTR(PROJ.M_TEXT,1,22)
   EXAMINE #H_PROJ_NAME AND TRANSLATE INTO UPPER CASE
*
   FOR #N_E 1 3
      EXAMINE #H_PROJ_NAME FOR #K_UMLAUT_TAB(#N_E)
          AND REPLACE WITH #G_UMLAUT_TAB(#N_E)
   END-FOR
*
*
   #N_ERWEIT := #N_ERWEIT + 1
   MOVE EDITED #N_ERWEIT (EM=99) TO #C_ERWEIT
   IF #N_ERWEIT <= 2
      COMPRESS #H_PROJ_NAME #JAHR
          INTO #C_ERW_TEXT2
      #C_ERW_ART2 := '02'
   ELSE
      #NEU_ERWEIT := #N_ERWEIT - 2
      COMPRESS #H_PROJ_NAME #JAHR
          INTO #C_ERWEIT_TEXT(#NEU_ERWEIT)
              #C_ERWEIT_ART(#NEU_ERWEIT) := '02'
   END-IF
*
   IF #RUECKRUF = TRUE
     #N_ERWEIT := #N_ERWEIT + 1
     MOVE EDITED #N_ERWEIT (EM=99) TO #C_ERWEIT
     IF #N_ERWEIT <= 2
        #C_ERW_TEXT2 := 'RUECKRUF VORBEHALTEN'
        #C_ERW_ART2 := '02'
     ELSE
        #NEU_ERWEIT := #N_ERWEIT - 2
        #C_ERWEIT_TEXT(#NEU_ERWEIT) := 'RUECKRUF VORBEHALTEN'
        #C_ERWEIT_ART(#NEU_ERWEIT) := '02'
     END-IF
   END-IF
*
   /*******************************************************************
   /* Bei Aufrechnung: Erhöhen des Erweiterungszählers um 1 und Füllen
   /*               des Verwendungszwecks mit Buchungskennzeichen
   /*******************************************************************
   IF #DB2_BKZ > ' '
     #N_ERWEIT := #N_ERWEIT + 1
     MOVE EDITED #N_ERWEIT (EM=99) TO #C_ERWEIT
     IF #N_ERWEIT <= 2
        COMPRESS 'BKZ:' #DB2_BKZ
            INTO #C_ERW_TEXT2
        #C_ERW_ART2 := '02'
     ELSE
        #NEU_ERWEIT := #N_ERWEIT - 2
        COMPRESS 'BKZ:' #DB2_BKZ
            INTO #C_ERWEIT_TEXT(#NEU_ERWEIT)
        #C_ERWEIT_ART(#NEU_ERWEIT) := '02'
     END-IF
   END-IF
*
   RESET #GESAMTSATZLAENGE
   #GESAMTSATZLAENGE := (#N_ERWEIT * 29) + 187
   MOVE EDITED #GESAMTSATZLAENGE (EM=9999)
     TO #C_STRUKTUR.#C_SATZLAENGE
*
** ## IF #NEU_ERWEIT > 0
** ##    #GESAMTSATZLAENGE := 256 + (29 * #NEU_ERWEIT)
** ##    MOVE EDITED #GESAMTSATZLAENGE (EM=9999)
** ##      TO #C_STRUKTUR.#C_SATZLAENGE
** ## ELSE
** ##    #GESAMTSATZLAENGE := 256
** ##    MOVE EDITED #GESAMTSATZLAENGE (EM=9999)
** ##      TO #C_STRUKTUR.#C_SATZLAENGE
** ## END-IF
*
   RESET #C_TAB(*)
   FOR #E 1 #NEU_ERWEIT
     COMPRESS #C_ERWEIT_ART(#E) #C_ERWEIT_TEXT(#E) INTO #C_TAB(#E)
       LEAVING NO
   END-FOR
   /*******************************************************************
   /* Schreiben des C-Satzes abhängig von der Anzahl der Erweiterungs-
   /* teile
   /*******************************************************************
   WRITE WORK FILE 2 #C_STRUKTUR #C_TAB(1:3)
   /**************************************************************
   /* UPDATE des Bezugsfeldes (lfd. Nummer auf Band)
   /* nur bei Neuerstellung - nicht bei nochmal. Erstellung
   /**************************************************************
   IF #DB2_ZAP_STAT = 7
      ADD 1 TO #BEZUG
      IF #P_BEREICH1.#MODUS = 'PROD'
        UPDATE ZAP-FOES
        SET BEZUG   = #BEZUG
          WHERE JAHR  = #VGL_JAHR
          AND PROJEKT = #DB2_PROJEKT
          AND BETRNR  = #VGL_BETRNR
          AND ZAP_DAT = #DB2_ZAPDAT
          AND STATUS  = #DB2_ZAP_STAT
*
        #BN_ZAEHL := #BN_ZAEHL + 1
        IF #BN_ZAEHL = 100
          WRITE #VGL_BETRNR
          COMMIT
          RESET #BN_ZAEHL
        END-IF
      END-IF
   END-IF
   /**************************************************************
   /* Füllen der Hilfssummenfelder für E-Satz
   /* Füllen der 9-Satz-Summenfelder
   /**************************************************************
   ADD 1                          TO #H_SUM_CSATZ
   ADD 1                          TO #SUM_FAELLE
   ADD 1                          TO #SUM_ERWEITEIL(#N_ERWEIT)
END-SUBROUTINE
*
DEFINE SUBROUTINE KONTO_PRUEFUNG
   #KONTO_CH := #U_KONTO
   REPEAT WHILE #KONTO_TAB(10) = ' '
     COMPRESS '0' #KONTO_CH INTO #KONTO_CH LEAVING NO
   END-REPEAT
*
   FOR #I 1 10
     IF #I = 1 AND (#KONTO_TAB(#I) < '0' OR #KONTO_TAB(#I) > '9')
       #KONTO_TAB(#I) := '0'
     END-IF
     IF #KONTO_TAB(#I) = ' '
       #KONTO_TAB(#I) := '0'
     END-IF
     IF #KONTO_TAB(#I) < '0' OR #KONTO_TAB(#I) > '9'
       #J  := #I - 1
       MOVE SUBSTR(#KONTO_CH,1,#J) TO #H10_1
       MOVE SUBSTR(#KONTO_CH,1,#I) TO #H10_2
       COMPRESS '0' #H10_1 INTO #H10_2 LEAVING NO
       MOVE #H10_2 TO SUBSTR(#KONTO_CH,1,#I)
     END-IF
   END-FOR
END-SUBROUTINE
*
DEFINE SUBROUTINE SATZ-2_SCHREIBEN
   RESET #2_STRUKTUR
   #2_SK           := 2
   #2_FILLER_0     := '000'
   #2_FILLER_1     := '0000'
   #2_DIENSTSTELLE := 'BAYSTMELF MUENCHEN'
*
************************************************************************
* Zusammenbau der Belegnummer "TTMMJ 001"
************************************************************************
   COMPRESS #V_TT #V_MM #V_J INTO #H5 LEAVING NO
   #H3_N := #ANZ_MAX_BUCHSATZ
   MOVE EDITED #H3_N (EM=999)
     TO #H3
   COMPRESS #H5 #H3 INTO #H_BELEGNR LEAVING NO
   #2_BELEGNR      := #N_BELEGNR
*
   IF #PRINT_AW = '+'
      IF #SATZ2_BETRAG  > 0
         #2_VSL   := 51000
         #2_KENNZ := 0
         #SUMME_51000 := #SUMME_51000 + #SATZ2_BETRAG
      ELSE
         #2_VSL := 54400
         #2_KENNZ := 0
         #POS_BETRAG := #SATZ2_BETRAG
         IF #POS_BETRAG < 0
            #POS_BETRAG := #POS_BETRAG * -1
         END-IF
         #SUMME_54400 := #SUMME_54400 + #POS_BETRAG
      END-IF
   ELSE
      #2_VSL   := 53100
      #2_KENNZ := 1
      #POS_BETRAG := #SATZ2_BETRAG
      IF #POS_BETRAG < 0
         #POS_BETRAG := #POS_BETRAG * -1
      END-IF
      #SUMME_53100 := #SUMME_53100 + #POS_BETRAG
   END-IF
*
   #2_BEWIRT   := '03063302'
   #2_HAUSHALT := VAL(BUND_STELLE)
*
   IF OBJ_KONTO > ' '
      #2_OBJEKT   := VAL(OBJ_KONTO)
      MOVE EDITED #2_OBJEKT (EM=9999999999)
        TO #E10
      #2_OBJEKT   := #E10_N
   END-IF
*
   #2_SA100       := '100'
   #2_SA100_FILLER:= '00000000'
*
   IF #PRINT_AW = '+'
      IF #SATZ2_BETRAG < 0
         #SATZ2_BETRAG    :=  #SATZ2_BETRAG * -1
      END-IF
      #E13  := #SATZ2_BETRAG * 100
      MOVE EDITED #E13 (EM=9999999999999)
        TO #2_SA100_BETRAG
   ELSE
      IF #SATZ2_BETRAG < 0
         #SATZ2_BETRAG := (#SATZ2_BETRAG * -1)
      END-IF
      #E13  := #SATZ2_BETRAG * 100
      MOVE EDITED #E13 (EM=9999999999999)
        TO #2_SA100_BETRAG
   END-IF
*
   #2_SAH02                := 'H02'
   COMPRESS #DB2_PROJEKT'/' #JAHR '/' #N2 INTO #2_SAH02_BUCH_TEXT
            LEAVING NO
*
   #2_SAH12                := 'H12'
   #2_SAH12_BUCH_TEXT      := ' '
*
   #2_SA104                := '104'
   #2_SA104_FILLER         := '0000000000000000'
*
   #2_SAH82                := 'H82'
   #2_SAH82_ANZ_ZAHLFAELLE := #SATZ2_ANZ
*
   WRITE WORK FILE 3 VARIABLE #2_STRUKTUR
*
   PERFORM KONTROLL_BERICHT_DATEN
END-SUBROUTINE
*
DEFINE SUBROUTINE SATZ-ZR_SCHREIBEN
   #ZR_SK           := 'Z'
   #ZR_LISTENART    := 'R'
   #ZR_LFD          := #BEZUG_ZK
   #ZR_BEWIRT       := 3063302
*
   RESET #E8
   MOVE EDITED #ZR_BEWIRT  (EM=99999999) TO #E8
   #ZR_BEWIRT   := #E8_N
*
   #ZR_LFD_GRUPPENNR := #GRUPPE
*
   RESET #E2
   MOVE EDITED #ZR_LFD_GRUPPENNR (EM=99) TO #E2
   #ZR_LFD_GRUPPENNR := #E2_N
*
   #ZR_ANZ_ELEMENTE_GRUPPE := #GRUPPE_ZAEHL
*
   WRITE WORK FILE 4 VARIABLE #ZR_STRUKTUR
*
   RESET #ZR_STRUKTUR
END-SUBROUTINE
*
DEFINE SUBROUTINE SATZ-ZL_SCHREIBEN
   RESET #ZL_STRUKTUR
   #ZL_SK           := 'Z'
   #ZL_LISTENART    := 'L'
   RESET #H7
   MOVE EDITED #LFD_NR_ZL (EM=9999999) TO #H7
   #ZL_LFD          := #H7
   #ZL_BEWIRT       := 3063302
*
   RESET #E8
   MOVE EDITED #ZL_BEWIRT  (EM=99999999) TO #E8
   #ZL_BEWIRT   := #E8_N
*
   #ZL_LFD_GRUPPENNR := 0
*
   RESET #E2
   MOVE EDITED #ZL_LFD_GRUPPENNR (EM=99) TO #E2
   #ZL_LFD_GRUPPENNR := #E2_N
*
   #ZL_EMPFAENGER  := #C_STRUKTUR.#C_EMPFAENGER
   #ZL_VOR_BETRAG  := '000'
   #ZL_BETRAG      := #C_STRUKTUR.#C_BETRAG_IN_EURO
   #ZL_TEXT        := #C_STRUKTUR.#C_ZWECK
   #ZL_BLZ_EMPFG   := #C_BLZ_EMPFG
   #ZL_KONTO_EMPFG := #C_KONTO_EMPFG
*
   WRITE WORK FILE 5 VARIABLE #ZL_STRUKTUR
*
END-SUBROUTINE
*
ON ERROR
  UPDATE ZAP-FOESZ
  SET BANDKENN = 0
    WHERE PROJEKT = #DB2_PROJEKT
    AND JAHR      = #DB2_JAHR
    AND ZAP_DAT   = #DB2_ZAPDAT
    AND AUSZ_ART  = #P_BEREICH1.#TYP
*
  END OF TRANSACTION
*
  #N_ERROR_NR := *ERROR-NR
  PERFORM ZAPPR2N_SCHREIBEN
*
  *ERROR-TA := 'FOEERR-P'
  #PGM      := *PROGRAM
  COMPRESS 'Kassette erstellen wegen' #N_ERROR_NR 'in Zeile' *ERROR-LINE
           'abgebrochen' INTO #CODE
  CALLNAT 'FOEERR-N' #PGM FOES.BETRNR #CODE
END-ERROR
*
PERFORM ZAPPR2N_SCHREIBEN
*
DEFINE SUBROUTINE ZAPPR2N_SCHREIBEN
   /*******************************************************************
   /* Pruefprotokoll vervollständigen
   /*******************************************************************
   CALLNAT '#TS----N' #PR_TS_E
   #PR_CON_CODE      := 0
*
   IF #P_BEREICH1.#MODUS = 'PROD'
      CALLNAT '#ZAPPR2N' #PR_MODUS
                         #PR_JAHR
                         #PR_PROJEKT
                         #PR_ZAP_DAT
                         #PR_STEPNAME
                         #PR_TS_B
                         #PR_TS_E
                         #PR_CON_CODE
                         #PR_BETR
                         #PR_BETR_PM
                         #PR_BETR_0
                         #PR_BETR_KL
                         #PR_AUSZ
                         #PR_KLEIN
                         #PR_RET_CODE
   ELSE
      WRITE              #PR_MODUS
                         #PR_JAHR
                         #PR_PROJEKT
                         #PR_ZAP_DAT
                         #PR_STEPNAME
                         #PR_TS_B
                         #PR_TS_E
                         #PR_CON_CODE
                         #PR_BETR
                         #PR_BETR_PM
                         #PR_BETR_0
                         #PR_BETR_KL
                         #PR_AUSZ
                         #PR_KLEIN
                         #PR_RET_CODE
   END-IF
END-SUBROUTINE
*
DEFINE SUBROUTINE BETRIEB_LESEN
   /*****************************************************
   /* Lesen neueste Betriebsdaten aus Betriebsdatenbank
   /*****************************************************
   #N_BETRNR := #VGL_BETRNR
   RESET #STICHTAG
         #BUNDESLAND
         #BETRDAT(*)
         #RET_CODE
*
   CALLNAT '#BDAT04N' #N_BETRNR
                      #STICHTAG
                      #BUNDESLAND
                      #BETRDAT(*)
                      #RET_CODE
                      #WARN_CODE
*
   DECIDE ON FIRST VALUE OF #RET_CODE
      /**************************************************
      /* Betrieb O.K.
      /**************************************************
      VALUE 0
         IGNORE
      /**************************************************
      /* Betrieb nicht vorhanden
      /**************************************************
      VALUE 1
        WRITE '=' #VGL_BETRNR 'Betrieb unbekannt'
**        IF #MODUS = 'PROD'
**           #CODE        := 'Betrieb unbekannt!'
**           #PR_CON_CODE := 10101
**           *ERROR-NR    := 9999
**        END-IF
      NONE
        IGNORE
   END-DECIDE
*
   #U_BLZ              := #BETRDAT.#BLZ
   #L_BLZ              := #BETRDAT.#BLZ
   #U_KONTO            := #BETRDAT.#KONTONR
   #U_BANKNAME         := #BETRDAT.#BANKNAME
*
   RESET #A_BLZ
         #A_KONTO
         #A_BANKNAME
*
   RESET #ANZ_BIW_JAHR #COUNT-PFAEN
   SELECT COUNT(*) INTO #ANZ_BIW_JAHR FROM GAT-BIW06
       WHERE BETRNR = #VGL_BETRNR
       AND JAHR   = #VGL_JAHR
       AND BIW_ERFOLGT <> ' '
       WITH UR
   END-SELECT
   IF #ANZ_BIW_JAHR > 0
     SELECT COUNT(*) INTO #COUNT-PFAEN FROM ZAP-PFAEN
         WHERE BETRNR = #VGL_BETRNR
**         AND PROJEKT = #DB2_PROJEKT
         AND   BESCHART = 6
         AND JAHR   = #VGL_JAHR
         WITH UR
     END-SELECT
     IF #COUNT-PFAEN = 0
       RESET
         #A_KONTO
         #A_BLZ
         #A_BANKNAME
         #RC_A
       PERFORM ZAPVORBS
         #VGL_BETRNR
         #A_KONTO
         #A_BLZ
         #A_BANKNAME
         #RC_A
       IF  #RC_A = 0
         #U_BLZ            := #A_BLZ
         #L_BLZ            := #BETRDAT.#BLZ
         #U_KONTO          := #A_KONTO
         #U_BANKNAME       := #A_BANKNAME
       ELSE
         WRITE '=' #VGL_BETRNR 'Fehler Lesen Vorbesitzer'
         IF #MODUS = 'PROD'
            #CODE        := 'Fehler Lesen Vorbesitzer'
            #PR_CON_CODE := 10105
            *ERROR-NR    := 9999
         END-IF
       END-IF
     END-IF
   END-IF
END-SUBROUTINE
*
DEFINE SUBROUTINE KONTROLL_BERICHT_KOPF
  FORMAT(5) PS=80 LS=90
*
  #H_FI := 5
  #H_FO := 6
  RESET #K_DATUM
*
  CALLNAT '#DATDREN' #P_BEREICH1.#ZAP_DAT #K_DATUM #H_FI #H_FO
  WRITE (5) 5T
            'Kontrollbericht zur elektronischen Einreichung von'
            'Zahlungsgsdateien'
  WRITE (5) /
            5T
            #DB2_PROJEKT
            9T
            '-'
           11T
            FOESZ.BUCH_TEXT1
  WRITE (5) /
            5T
            'Buchungs- und Zahlungsdatei  F 13  übermittelt am'
            #K_DATUM
  WRITE (5) /
            5T
            'Haushaltsjahr'
             #HH_JAHR
*
  COMPRESS #P_BEREICH1.#PROJEKT '.JAHR' #P_BEREICH1.#JAHR '.BUCHEFT.B'
           #H_DAT_BAND
      INTO #NAME_BUCH_DATEI LEAVING NO
*
  COMPRESS #P_BEREICH1.#PROJEKT '.BAND' #P_BEREICH1.#JAHR '.B'
           #H_DAT_BAND
      INTO #NAME_BAND_DATEI LEAVING NO
*
  WRITE (5) /
            5T
            'Kennzeichen der F13-Buchungsdatei'
            42T
            #BUCH_BAND_KENN
            /
            5T
            'Name der F13-Buchungsdatei'
            42T
            #NAME_BUCH_DATEI
*
  WRITE (5) 5T
            'Kennzeichen der F13-Zahlungsdatei'
            42T
            #A_NEUE_BANDKENN
            /
            5T
            'Name der F13-Zahlungsdatei'
            42T
            #NAME_BAND_DATEI
*
  WRITE (5) //
            5T
            'Aufteilung nach Haushaltsstelle/Objektkonto:'
            /
            5T
            '============================================'
*
  WRITE (5) /
            5T
            'Haushaltsstelle / Objektkonto'
           45T
            'Betrag'
           55T
            'VSL'
           60T
            'Anzahl'
*
  #A61 :=
    '-------------------------------------------------------------'
*
  WRITE (5) 5T
            #A61
END-SUBROUTINE
*
DEFINE SUBROUTINE KONTROLL_BERICHT_DATEN
  DECIDE ON FIRST VALUE OF #2_VSL
    VALUES 53100
      #N1       := #SATZ2_BETRAG
      MOVE EDITED #N1           (EM=Z.ZZZ.ZZZ.ZZ9,99)
        TO #A16
    NONE
      #N1       := #SATZ2_BETRAG
      MOVE EDITED #N1           (EM=Z.ZZZ.ZZZ.ZZ9,99)
        TO #A16
  END-DECIDE
*
  MOVE EDITED #2_SAH82_ANZ_ZAHLFAELLE (EM=ZZZ.ZZ9)
    TO #A2
  WRITE (5) /
            5T
            BUND_STELLE
           23T
            OBJ_KONTO
           35T
            #A16
           52T
            #2_VSL
           59T
            #A2
*
   ADD 1 TO #AUFTEIL_ZAEHL
   #AUFTEIL_OBJ(#AUFTEIL_ZAEHL)  := OBJ_KONTO
   #AUFTEIL_BETR(#AUFTEIL_ZAEHL) := #N1
   #AUFTEIL_ZF(#AUFTEIL_ZAEHL)   := #2_SAH82_ANZ_ZAHLFAELLE
*
 #GES_ZAHL := #GES_ZAHL + #2_SAH82_ANZ_ZAHLFAELLE
*
END-SUBROUTINE
*
DEFINE SUBROUTINE KONTROLL_BERICHT_FUSS
*
  #A61 :=
    '-------------------------------------------------------------'
*
  WRITE (5) 5T
            #A61
*
  #GES := #SUMME_51000 + #SUMME_53100 + #SUMME_54400
  MOVE EDITED #GES       (EM=Z.ZZZ.ZZZ.ZZ9,99)
    TO #A16
  MOVE EDITED #GES_ZAHL  (EM=ZZZ.ZZ9)
    TO #A2
*
  WRITE (5) 5T
            'Gesamtsumme:'
           35T
            #A16
           59T
            #A2
*
  RESET #SUMME_MO #SUMME_SA
*
  SELECT SUM(KUERZ_BETRAG_HHL) INTO #SUMME_SA
    FROM ZAP-ZABUHHL
   WHERE PROJEKT = #PROJEKT AND JAHR = #JAHR
     AND DATE(AENDERUNG) = #ZAP_DAT
  END-SELECT
*
  SELECT SUM(KUERZ_BETRAG_HHL) INTO #SUMME_MO
    FROM ZAP-MOSA2
   WHERE PROJEKT = #PROJEKT AND JAHR = #JAHR
     AND DATE(LAST_TS) = #ZAP_DAT
     AND MOSA_TYP = 'MO';
  END-SELECT
*
  MOVE EDITED #SUMME_SA  (EM=Z.ZZZ.ZZZ.ZZ9,99)
    TO #A16
*
  WRITE (5) //
            5T
            'Summe Sanktion:'
           24T
            #A16
*
  MOVE EDITED #SUMME_MO  (EM=Z.ZZZ.ZZZ.ZZ9,99)
    TO #A16
*
  WRITE (5) 5T
            'Summe Modulation:'
           24T
            #A16
*
  MOVE EDITED #H_SUMME_IN_EURO_BAND  (EM=Z.ZZZ.ZZZ.ZZ9,99)
    TO #A16
*
  WRITE (5) 5T
            'Auszahlungsbetrag:'
           24T
            #A16
*
  WRITE (5) 5T
            #A61
*
 #INSGESAMT := #SUMME_SA + #SUMME_MO + #H_SUMME_IN_EURO_BAND
*
  MOVE EDITED #INSGESAMT  (EM=Z.ZZZ.ZZZ.ZZ9,99)
    TO #A16
*
  WRITE (5) 5T
            'Insgesamt:'
           24T
            #A16
*
  #A61 :=
    '============================================================='
*
  WRITE (5) 5T
            #A61
*
  WRITE (5) ///
            5T
            'Bescheinigung nach HKR 8.4 HKR - ADV - Bestimmung:'
            /
            5T
            '--------------------------------------------------'
            /
            5T
            'Das Bayerische Staatsministerium für Ernährung,'
           53T
            'Landwirtschaft und Forsten'
            /
            5T
            'bescheinigt die ordnungsgemäße Verarbeitung der Daten.'
  WRITE (5) //
           20T
            'Für die fachliche Abwicklung sachlich und rechnerisch'
           74T
            'richtig'
            ///
           20T
            'München,'
           29T
            #P_BEREICH1.#ZAP_DAT
  WRITE (5) ////
           20T
            'Für die EDV-Abwicklung sachlich und rechnerisch richtig'
            ///
           20T
            'München,'
           29T
            #P_BEREICH1.#ZAP_DAT
END-SUBROUTINE
*
DEFINE SUBROUTINE ZAHLLISTE_KOPF
  FORMAT(2) PS=56 LS=132
*
COMPRESS '+' '----------'
         '+' '---------------------------------'
         '+' '---------------'
         '+' '-------------'
         '+' '---------------'
         '+' '--------------'
         '+' INTO #H107_STRICH LEAVING NO
*
COMPRESS FOESZ.LFD_ZAHL '. Auszahlung' #P_BEREICH1.#JAHR
INTO #H20
*
EXAMINE FULL PROJ.TEXT FOR ' ' GIVING LENGTH IN #J
   #K := (131 - #J) / 2
   RESET #H131_TEXT
   MOVE PROJ.TEXT TO SUBSTR(#H131_TEXT,#K,#J)
*
WRITE (2) NOTITLE
          47T 'Z a h l l i s t e'
          92T 'Anweisungdatum:'
         108T #P_BEREICH1.#ZAP_DAT
          /
          47T '-----------------'
          /
          /
          #H131_TEXT
          /
          /
          40T #H20
*
*
WRITE (2) /
          #H107_STRICH
          /
          1T  '!'
          3T  'Lfd. Nr.'
          12T '!'
          14T 'Zahlungsempfänger'
          46T '!'
          48T 'Einzelbetrag'
          62T '!'
          64T 'Fallnummer'
          76T '!'
          78T 'Überweisung'
          92T '!'
          94T 'Kontonummer'
         107T '!'
          /
          1T  '!'
          12T '!'
          14T 'Haushaltsstelle'
          46T '!'
          48T 'in Euro/Ct'
          62T '!'
          64T 'd. Bewirt.'
          76T '!'
          78T 'in Euro/Ct'
          92T '!'
          94T 'Bankleitzahl'
         107T '!'
*
END-SUBROUTINE
*
DEFINE SUBROUTINE ZAHLLISTE_ZEILE1
  RESET #AUS_WERTE_V13
  IF #H_BETRAG_EURO > 0
     MOVE EDITED #H_BETRAG_EURO (EM=Z.ZZZ.ZZ9,99)
       TO #AUS_WERTE_V13
  END-IF
*
  WRITE (2) #H107_STRICH
            /
            1T '!'
            3T #BN_COUNTER
           12T '!'
           14T #L_NAME
           46T '!'
           62T '!'
           64T #VGL_BETRNR
           76T '!'
           78T #AUS_WERTE_V13 (AD=R)
           92T '!'
           94T #U_KONTO
          107T '!'
END-SUBROUTINE
*
DEFINE SUBROUTINE ZAHLLISTE_EINZELDATEN
  RESET #AUS_WERTE_V13
  IF #EINZEL_BETRAG > 0
     MOVE EDITED #EINZEL_BETRAG (EM=Z.ZZZ.ZZ9,99)
              TO #AUS_WERTE_V13
  END-IF
*
  IF #KENNUNG > ' '
    COMPRESS OBJ_KONTO '-' #KENNUNG INTO #A13 LEAVING NO
  ELSE
    #A13 := OBJ_KONTO
  END-IF
*
  WRITE (2) 1T '!'
           12T '!'
           14T #A13
           46T '!'
           48T #AUS_WERTE_V13 (AD=R)
           62T '!'
           76T '!'
           92T '!'
           96T #H_BLZ
          107T '!'
END-SUBROUTINE
*
DEFINE SUBROUTINE ZAHLLISTE_FUSS
  WRITE (2) #H107_STRICH
*
  /*********************************************************************
  /* Anzeige Gesamtsumme und Gesamtanzahl Zahlungsempfänger
  /*********************************************************************
  MOVE EDITED #BN_COUNTER (EM=*ZZZZZ9) TO #AUS_WERTE_A7
  COMPRESS 'Gesamt: Zahlungsempfänger **' #AUS_WERTE_A7 '**' INTO  #H37
  LEAVING NO
*
  MOVE EDITED #H_SUMME_IN_EURO_BAND (EM=Z.ZZZ.ZZZ.ZZ9,99)
    TO #H18
  EXAMINE #H18 FOR ' ' AND REPLACE WITH '*'
  COMPRESS 'Endsumme **' #H18 '**' INTO  #H32
  LEAVING NO
*
  COMPRESS #H37 #H32 INTO #H70
*
  FOR #ZZ 1 2
     NEWPAGE (2)
     WRITE (2) NOTITLE
               47T 'Z a h l l i s t e'
               92T 'Anweisungdatum:'
              108T #P_BEREICH1.#ZAP_DAT
               /
               47T '-----------------'
               /
               /
               #H131_TEXT
               /
               /
               40T #H20
*
     WRITE (2) //
               #H70
*
     WRITE (2) //
               'Übersicht der Zahlungen nach Objektkonten'
               //
               'Objektkonto           Summe           Zahlfälle'
               /
               '-----------------------------------------------'
*
     FOR #J 1 60
        IF #AUFTEIL_BETR(#J) ^= 0
           MOVE EDITED #AUFTEIL_BETR(#J) (EM=Z.ZZZ.ZZZ.ZZ9,99)
             TO #A16
           MOVE EDITED #AUFTEIL_ZF(#J) (EM=ZZZZ.ZZZ.ZZ9)
             TO #A12
*
           WRITE (2) 1T #AUFTEIL_OBJ(#J)
                    13T #A16   (AD=R)
                    36T #A12   (AD=R)
        END-IF
     END-FOR
*
     WRITE (2) '-----------------------------------------------'
*
     MOVE EDITED #GES       (EM=Z.ZZZ.ZZZ.ZZ9,99)
       TO #A16
     MOVE EDITED #GES_ZAHL  (EM=ZZZ.ZZ9)
       TO #A12
*
     WRITE (2) 1T 'Gesamt:'
              13T #A16   (AD=R)
              36T #A12   (AD=R)
*
     WRITE (2) '==============================================='
*
     IF #PROJEKT = 'KPR' OR #PROJEKT = 'SFP' OR
        #PROJEKT = 'RIP' OR #PROJEKT = 'MIP' OR
        #PROJEKT = 'DZP' OR #PROJEKT = 'STK'
*
        RESET #SUMME_MO #SUMME_SA
*
        SELECT SUM(KUERZ_BETRAG_HHL) INTO #SUMME_SA
          FROM ZAP-ZABUHHL
         WHERE PROJEKT = #PROJEKT AND JAHR = #JAHR
           AND DATE(AENDERUNG) = #ZAP_DAT
        END-SELECT
*
        SELECT SUM(KUERZ_BETRAG_HHL) INTO #SUMME_MO
          FROM ZAP-MOSA2
         WHERE PROJEKT = #PROJEKT AND JAHR = #JAHR
           AND DATE(LAST_TS) = #ZAP_DAT
           AND MOSA_TYP = 'MO';
        END-SELECT
*
        RESET #AUFSTELLUNG(*)
        MOVE EDITED #H_SUMME_IN_EURO_BAND (EM=Z.ZZZ.ZZZ.ZZ9,99)
             TO #AUFSTELLUNG(1)
        MOVE EDITED #SUMME_MO  (EM=Z.ZZZ.ZZZ.ZZ9,99)
             TO #AUFSTELLUNG(2)
        MOVE EDITED #SUMME_SA  (EM=Z.ZZZ.ZZZ.ZZ9,99)
             TO #AUFSTELLUNG(3)
        #INSGESAMT := #H_SUMME_IN_EURO_BAND + #SUMME_SA + #SUMME_MO
        MOVE EDITED #INSGESAMT (EM=Z.ZZZ.ZZZ.ZZ9,99)
             TO #AUFSTELLUNG(4)
*
        WRITE (2) ///
                  1T 'Aufteilung'
                  /
                  1T '=========='
                  /
                  1T 'Auszahlungsbetrag....:'
                  24T #AUFSTELLUNG(1)
                  /
                  1T 'Modulationsmittel....:'
                  24T #AUFSTELLUNG(2)
                  /
                  1T 'Sanktion.............:'
                  24T #AUFSTELLUNG(3)
                  /
                  1T 'Gesamtbetrag.........:'
                  24T #AUFSTELLUNG(4)
     END-IF
  END-FOR
*
END-SUBROUTINE
**----------------------------------------------------------------------
* Nachspann für Druckersteuerung auf Netzwerkdrucker ausgeben
**----------------------------------------------------------------------
#SYS_TIME := *TIME
*
WRITE '=' FOESZ.USERID
*
* #TITLE := 'ZAP - Datenträgerbegleitzettel'
* #DATE   := #P_BEREICH1.#ZAP_DAT
* #APPL   := '10.08'
* #USER   := FOESZ.USERID
* #TIME   := SUBSTR(#SYS_TIME,1,5)
* #FCB    := 'ZB12'
* #PRINT  := #PC1182_USER
* #KEEP   := #PC1182_USER
* #LOKAL  := #PC1182_USER
* #NETZ   := #PC1182_USER
* #COPY   := #PC1182_KEINER
* #INFO   := #PC1182_USER
* #BROWSE := #PC1182_USER
* #EDIT   := #PC1182_KEINER
* #DELETE := #PC1182_USER
* #REP    := 1
* CALLNAT '#PC1182N' #PC1182-HEADER #REP #RC
*
#TITLE := 'ZAP - Kontrollbericht'
#DATE   := #P_BEREICH1.#ZAP_DAT
#APPL   := '10.08'
#USER   := FOESZ.USERID
#TIME   := SUBSTR(#SYS_TIME,1,5)
#FCB    := 'ZB12'
#PRINT  := #PC1182_USER
#KEEP   := #PC1182_USER
#LOKAL  := #PC1182_USER
#NETZ   := #PC1182_USER
#COPY   := #PC1182_USER
#INFO   := #PC1182_USER
#BROWSE := #PC1182_USER
#EDIT   := #PC1182_KEINER
#DELETE := #PC1182_USER
#REP    := 5
CALLNAT '#PC1182N' #PC1182-HEADER #REP #RC
*
#TITLE := 'ZAP - Zahlliste'
#DATE   := #P_BEREICH1.#ZAP_DAT
#APPL   := '10.08'
#USER   := FOESZ.USERID
#TIME   := SUBSTR(#SYS_TIME,1,5)
#FCB    := 'ZB12'
#PRINT  := #PC1182_USER
#KEEP   := #PC1182_USER
#LOKAL  := #PC1182_USER
#NETZ   := #PC1182_USER
#COPY   := #PC1182_USER
#INFO   := #PC1182_USER
#BROWSE := #PC1182_USER
#EDIT   := #PC1182_KEINER
#DELETE := #PC1182_USER
#REP    := 2
CALLNAT '#PC1182N' #PC1182-HEADER #REP #RC
END OF TRANSACTION
END-SUBROUTINE
END
