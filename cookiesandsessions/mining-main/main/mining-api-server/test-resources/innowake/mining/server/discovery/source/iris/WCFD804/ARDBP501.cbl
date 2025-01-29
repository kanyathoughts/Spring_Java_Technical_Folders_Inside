000100 IDENTIFICATION DIVISION.                                         00010000
000200 PROGRAM-ID.    ARDBP501.                                         00020000
000300*AUTHOR.         MUGESCU.                                         00030000
000400*DATE-WRITTEN.  OKT/1998.                                         00040000
000500************************************************************      00050000
000600***                                                               00060000
000700***                       AA      RRRRRR                          00070000
000800***                      AAAA     RR  RR                          00080000
000900***                     AA  AA    RRRR                            00090000
001000***                    AAAAAAAA   RR  RR                          00100000
001100***                   AA      AA  RR   RR                         00110000
001200***                                                               00120000
001300***  = OZP =                                                      00130000
001400************************************************************      00140000
001500*                                                                 00150000
001600* K U R Z B E S C H R E I B U N G  D E S  P R O G R A M M S       00160000
001700* ---------------------------------------------------------       00170000
001800*                                                                 00180000
001900*  DAS PROGRAMM FUEHRT DIE ZUGRIFFE AUF DIE TABELLE               00190000
002000*  AR_ABRECH_DET AUS.                                             00200000
002100*  BATCH VERARBEITUNG AUSLANDSREISEVERSICHERUNG                   00210000
002200*                                                                 00220000
002300************************************************************      00230000
002400*                                                                 00240000
002500* D A T E N B A N K E N  / D B 2  -  T A B E L L E N              00250000
002600* ----------------------------------------------------------      00260000
002700*                                                                 00270000
002800* DD-NAME              BEZEICHNUNG                   DB-SYST      00280000
002900* AR_ABRECH_DET        ABRECHNUNG-DETAIL            DB2PVARB      00290000
003000*                                                                 00300000
003100*                                                                 00310000
003200************************************************************      00320000
003300*                                                                 00330000
003400* M A I N T E N A N C E  L O G                                    00340000
003500*-----------------------------------------------------------      00350000
003600*                                                                 00360000
003700* AENDERUNGSDATUM : 03.08.2000                                    00370000
003800*                                                                 00380000
003900* GEANDERT VON    : GFP/AL4 - HEIDI WELTER                        00390000
004000*                                                                 00400000
004100* AENDERUNGSGRUND : F¸R DAS PR‰MIENSOLL M¸SSEN DIE PERSONENBEZOGE-00410000
004200*                   NEN PAUSCH-TARIFE (Z.B. A5/R5...ALSO ALLE MIT 00420000
004300*                   2. STELLE >= 5) EBENFALLS UNTER 'INDIVI-      00420100
004400*                   DUELL' UND NICHT - WIE BISHER FALSCH - UNTER  00420200
004500*                   PAUSCHAL SALDIERT/AUSGEWIESEN WERDEN          00420300
004600*                   (S. A. U31-VERARB-TAB)                        00420400
004700*                   FERNER MUSS DIE EINE KENNZEICHNUNG DER        00420500
004800*                   PERSONENBEZOGENEN PAUSCH-TARIFE ERFOLGEN      00420600
004900*                   (PTAR-ART)                                    00420700
005000*                                                                 00420800
005100* AENDERUNGS-KZ   : V1.02                                         00430000
005200*                                                                 00440000
005300* VERSION         : 1.02 (GEM. ENDEVOR)                           00450000
005400*                                                                 00460000
005500*-----------------------------------------------------------      00470000
005600*                                                                 00480000
005700* AENDERUNGSDATUM :                                               00490000
005800*                                                                 00500000
005900* GEANDERT VON    :                                               00510000
006000*                                                                 00520000
006100* AENDERUNGSGRUND :                                               00530000
006200*                                                                 00540000
006300* AENDERUNGS-KZ   :                                               00550000
006400*                                                                 00560000
006500* VERSION         :                                               00570000
006600*                                                                 00580000
006700*---------------------------------------------------------------* 00590000
006800* 19.10.2012 ! H. WELTER    ! PROJ. 44903 - UNISEX              * 00600000
006900*            !              ! BER¸CKSICHTIGUNG DER NEUEN TARIFE:* 00610000
007000*            !              ! PAUSCHAL: AE/IE/IF/RE/RF/EG/RH    * 00620000
007100*            !              ! INDIVID.: AA/IA/IB/RA/RB/RC/ED    * 00630000
007200*---------------------------------------------------------------* 00631000
007300* 11.09.2018 ! H. WELTER    ! KOL-ALM#986: MANTELVERTRAGS-/JAHRE- 00632000
007400*            !              ! PAUSCHALEN, WELCHE PRO MONAT UNTER* 00633000
007500*            !              ! RECH_MM_BETR ABGELEGT WURDEN, SIND* 00634000
007600*            !              ! AUCH UNTER P1/P2 ZU BER¸CKSICHTIGEN 00635000
007700*---------------------------------------------------------------* 00640000
007800*            !              !                                   * 00650000
007900*---------------------------------------------------------------* 00650100
008000*                                                                 00650200
008100/                                                                 00650300
008200 ENVIRONMENT DIVISION.                                            00650400
008300 DATA DIVISION.                                                   00660000
008400/                                                                 00670000
008500 WORKING-STORAGE SECTION.                                         00680000
008600*                                                                 00690000
008700 01  ZW-BEGIN-WS.                                                 00700000
008800     03 FILLER          PIC X(051) VALUE                          00701000
008900     'BEGIN  OF  WORKING STORAGE SECTION A R V - ARDBP501'.       00710000
009000* ----------------------------------------------------------- *   00720000
009100*  LITERALS                                                   *   00730000
009200* ----------------------------------------------------------- *   00740000
009300 01  ZW-LITERALS.                                                 00750000
009400     03 ZW-LITERAL-THIS-PROG        PIC X(8)  VALUE 'ARDBP501'.   00760000
009500                                                                  00770000
009600* ----------------------------------------------------------- *   00780000
009700*  MESSAGES                                                   *   00790000
009800* ----------------------------------------------------------- *   00800000
009900 01  ZW-MESSAGE-DEFINITIONS.                                      00810000
010000     03 ZW-MSG0001                      PIC X(30)  VALUE          00820000
010100        '0001H                        '.                          00830000
010200     03 ZW-MSG0002                      PIC X(30)  VALUE          00840000
010300        '0002F                        '.                          00850000
010400     03 ZW-MSG0003                      PIC X(30)  VALUE          00860000
010500        '0002F                        '.                          00870000
010600/                                                                 00880000
010700* ----------------------------------------------------------- *   00890000
010800*   WORK AREA                                                 *   00900000
010900* ----------------------------------------------------------- *   00910000
011000 01  ZW-VARIABLEN.                                                00920000
011100   02  ZW-KOMM-KZ                       PIC X(02).                00930000
011200       88  FEHLER                       VALUE 'F'.                00940000
011300   02  ZW-TRANSFER-KZ                   PIC X(2).                 00950000
011400       88  HV-NACH-PARAM                VALUE 'HP'.               00960000
011500       88  PARAM-NACH-HV                VALUE 'PH'.               00970000
011600*==> #986                                                         00970100
011700   02  ZW-UP92-DATENFELDER.                                       00971000
011800     05    ZW-FREQ                     PIC  9(01).                00972000
011900     05    ZW-SUMMAND                  PIC  9(02).                00973000
012000     05    ZW-MULTIPLIKATOR            PIC  9(02).                00974000
012100     05    ZW-ANZAHL                   PIC  9(02).                00975000
012200     05    ZW-MONATSTABELLE.                                      00979400
012300       10  ZW-MONATSTABELLE-T               OCCURS 12             00979500
012400                                            INDEXED BY IND1.      00979600
012500        15 ZW-TAB-MM                   PIC  9(01).                00979700
012600     05    ZW-BETR.                                               00979800
012700       10  ZW-BETR1                    PIC S9(08)V9(05) COMP.     00979900
012800       10  ZW-BETR2                    PIC S9(08)V9(05) COMP.     00980000
012900     05    MK-LESEN                    PIC  X(01).                00980100
013000        88 UEBERLESEN                      VALUE '0'.             00980200
013100        88 WEITER                          VALUE '1'.             00980300
013200*                                                                 00980400
013300     05    ZW-ABR-JJJJMM-N             PIC  9(06).                00980500
013400     05    ZW-ABR-JJJJMM-R                       REDEFINES        00980600
013500           ZW-ABR-JJJJMM-N.                                       00980700
013600       10  ZW-ABR-JJJJ                 PIC  9(04).                00980800
013700       10  ZW-ABR-MM                   PIC  9(02).                00980900
013800* ----------------------------------------------------------- *   00981000
013900*  SQL-DECLARE-STATEMENTS                                     *   00990000
014000*                                                             *   01000000
014100*  - TABELLEN DEKLARATIONEN                                   *   01010000
014200*  - HOSTVARIABLEN                                            *   01020000
014300*  - INDIKATORVARIABLEN                                       *   01030000
014400*  - CURSOR                                                   *   01040000
014500*                                                             *   01050000
014600* ----------------------------------------------------------- *   01060000
014700           EXEC SQL BEGIN DECLARE SECTION END-EXEC.               01070000
014800/AR_ABRECH_DET                                                    01080000
014900           EXEC SQL INCLUDE ARHVAD0A    END-EXEC.                 01110000
015000/#986 - ZZGL. AR_WERK                                             01111000
015100           EXEC SQL INCLUDE ARDBWE1A    END-EXEC.                 01112000
015200/#986 - ZZGL. AR_ABRECH                                           01113000
015300           EXEC SQL INCLUDE ARDBAB1A    END-EXEC.                 01114000
015400/                                                                 01120000
015500           EXEC SQL                                               01130000
015600            DECLARE C1-AD-ABRECH CURSOR FOR                       01140000
015700            SELECT                                                01150000
015800               TAR_SYM                                            01151000
015900             , ABR_SCHL                                           01160000
016000             , RECH_BETR                                          01170000
016100             , RECH_WHG                                           01180000
016200/*#986 ZZGL. RECH_MM_BETR                                         01180100
016300             , RECH_MM_BETR                                       01181000
016400            FROM AR_ABRECH_DET                                    01190000
016500            WHERE                                                 01200000
016600*<#986          ABR_SCHL = :ABR-SCHL                              01210000
016700                ABR_SCHL = :DCLAR-ABRECH-DET.ABR-SCHL             01211000
016800            ORDER BY ABR_SCHL                                     01220000
016900            END-EXEC.                                             01230000
017000* ----------------------------------------------------------- *   01240000
017100*  SQL-BEREICH:  SQLCA / SQL-COMMUNICATION-AREA               *   01250000
017200* ----------------------------------------------------------- *   01260000
017300 01  ZW-DB2-DATEN.                                                01270000
017400   05  SQLCODE                            PIC S9(04) BINARY.      01270100
017500*                                                                 01270300
017600*#986 - NEUE HOST-VARIABLE                                        01271000
017700   05  ZW-ABRECH-DET-SQLCODE              PIC S9(04) BINARY.      01271100
017800   05  ZW-DB-PAUSCH-MM                    PIC S9(04) BINARY.      01272000
017900   05  ZW-DB-VERTR-MM                     PIC S9(04) BINARY.      01273000
018000*                                                                 01280000
018100 01  SQLSTATE                             PIC X(5).               01290000
018200/                                                                 01300000
018300           EXEC SQL END DECLARE SECTION END-EXEC.                 01310000
018400/                                                                 01320000
018500 LINKAGE SECTION.                                                 01330000
018600* ----------------------------------------------------------- *   01340000
018700*    LINKAGE                                                  *   01350000
018800* ----------------------------------------------------------- *   01360000
018900 01  PARAM-KOMM.                                                  01370000
019000 COPY ARDBPO0A.                                                   01380000
019100/                                                                 01390000
019200 01  PARAM-KEY.                                                   01400000
019300 COPY ARDBKY0A.                                                   01410000
019400/                                                                 01420000
019500 01  PARAM-DATEN-1.                                               01430000
019600     05  PARAM-D1                  OCCURS 10                      01440000
019700        INDEXED BY IND-S1.                                        01450000
019800                                                                  01460000
019900 COPY ARC1AD0A.                                                   01470000
020000/                                                                 01480000
020100 01  PARAM-DATEN-2.                                               01490000
020200     05  PARAM-D2.                                                01500000
020300*                                                                 01520000
020400 COPY ARC1AD1A.                                                   01530000
020500/                                                                 01540000
020600 PROCEDURE DIVISION             USING PARAM-KOMM                  01550000
020700                                      PARAM-KEY                   01560000
020800                                      PARAM-DATEN-2               01570000
020900                                      PARAM-DATEN-1.              01580000
021000*-------------------------------------------------------------*   01590000
021100* STEUERUNG                                                   *   01600000
021200*-------------------------------------------------------------*   01610000
021300 S-STEUER                     SECTION.                            01620000
021400 S-010.                                                           01630000
021500     PERFORM S1-VORLAUF.                                          01640000
021600*                                                                 01650000
021700     IF    FEHLER                                                 01660000
021800           GO TO S-090.                                           01670000
021900*                                                                 01680000
022000     PERFORM S2-DB2-ZUGRIFF.                                      01690000
022100*                                                                 01700000
022200 S-090.                                                           01710000
022300     PERFORM S3-NACHLAUF.                                         01720000
022400*                                                                 01730000
022500 S-EXIT.                                                          01740000
022600     EXIT  PROGRAM.                                               01750000
022700/                                                                 01760000
022800* ----------------------------------------------------------- *   01770000
022900*  INITIALISIERUNGEN DER WORKING STORAGE BEREICHE             *   01780000
023000*                                                             *   01790000
023100* ----------------------------------------------------------- *   01800000
023200 S1-VORLAUF                   SECTION.                            01810000
023300 S1-010.                                                          01820000
023400                                                                  01830000
023500     INITIALIZE  ZW-VARIABLEN.                                    01840000
023600                                                                  01850000
023700     INITIALIZE  ARPO-KOMM-AUSGABE.                               01860000
023800                                                                  01870000
023900     INITIALIZE  ARC1-SABRDET.                                    01871000
024000                                                                  01872000
024100     SET ARPO-RC-OHNE TO TRUE.                                    01880000
024200                                                                  01890000
024300*<#986  MOVE ARKY-PRI-KEY-C (1) TO ABR-SCHL.                      01900000
024400     MOVE ARKY-PRI-KEY-C (1) TO ABR-SCHL OF DCLAR-ABRECH-DET.     01901000
024500                                                                  01910000
024600     IF NOT ARPO-RC-OK                                            01950000
024700        SET FEHLER                     TO TRUE.                   01960000
024800 S1-EXIT.                                                         01970000
024900     EXIT.                                                        01980000
025000/                                                                 01990000
025100* ----------------------------------------------------------- *   02120000
025200* STEUERUNG:       DB-ZUGRIFFE                                *   02130000
025300* ----------------------------------------------------------- *   02140000
025400*                                                                 02150000
025500 S2-DB2-ZUGRIFF              SECTION.                             02160000
025600 S2-020.                                                          02170000
025700     EVALUATE   TRUE                                              02180000
025800*                                                                 02190000
025900     WHEN       ARPO-OP-RETR                                      02200000
026000                PERFORM S22-DBZ-RETRIEVAL                         02210000
026100                IF NOT ARPO-OP-SELECT                             02220000
026200                   GO TO S2-EXIT                                  02230000
026300                END-IF                                            02240000
026400                                                                  02250000
026500     WHEN       ARPO-OP-UPDATE                                    02260000
026600* ACHTUNG DATEN-UMSCHIFTEN NACH HV                                02270000
026700                PERFORM S23-DBZ-UPDATE                            02280000
026800                                                                  02290000
026900                                                                  02300000
027000     WHEN       ARPO-OP-OPEN                                      02310000
027100                PERFORM S24-DBZ-OPEN                              02320000
027200                                                                  02330000
027300     WHEN       ARPO-OP-CLOSE                                     02340000
027400                PERFORM S25-DBZ-CLOSE                             02350000
027500                                                                  02360000
027600     WHEN  OTHER                                                  02370000
027700               SET ARPO-RC-F-OPERAT TO TRUE                       02380000
027800                                                                  02390000
027900     END-EVALUATE.                                                02400000
028000 S2-030.                                                          02410000
028100                                                                  02440000
028200     PERFORM S26-SQLSTATE.                                        02450000
028300                                                                  02460000
028400     IF NOT ARPO-RC-OK                                            02470000
028500        GO TO S2-EXIT.                                            02480000
028600 S2-040.                                                          02490000
028700        EVALUATE  TRUE                                            02500000
028800        WHEN      ARPO-OP-RETR                                    02510000
028900                  SET HV-NACH-PARAM  TO TRUE                      02520000
029000        WHEN      OTHER                                           02540000
029100                  CONTINUE                                        02550000
029200        END-EVALUATE.                                             02560000
029300                                                                  02570000
029400 S2-EXIT.                                                         02580000
029500     EXIT.                                                        02590000
029600/                                                                 02600000
029700* ----------------------------------------------------------- *   02730000
029800* RETRIEVAL-ZUGRIFFE                                          *   02740000
029900* ----------------------------------------------------------- *   02750000
030000 S22-DBZ-RETRIEVAL            SECTION.                            02751000
030100 S22-010.                                                         02760000
030200     EVALUATE   TRUE                                              02770000
030300     WHEN       ARPO-OP-SELECT                                    02780000
030400                PERFORM S221-DBZ-SELECT                           02790000
030500     WHEN       ARPO-OP-READ                                      02800000
030600     WHEN       ARPO-OP-FETCH                                     02810000
030700                PERFORM U1-VERARB-CURSOR                          02820000
030800     WHEN       OTHER                                             02830000
030900                SET ARPO-RC-F-OPERAT TO TRUE                      02840000
031000     END-EVALUATE.                                                02850000
031100 S22-EXIT.                                                        02860000
031200     EXIT.                                                        02870000
031300/                                                                 02880000
031400* ----------------------------------------------------------- *   02900000
031500* ZUGRIFF:   ZEILE    (EINDEUTIG)                             *   02910000
031600*            AR_ABRECH_DET MIT TAR_SYM                        *   02920000
031700* ----------------------------------------------------------- *   02930000
031800 S221-DBZ-SELECT              SECTION.                            02931000
031900 S221-010.                                                        02940000
032000     IF    ARKY-PRI-KEY-C (01) NOT = SPACES                       02950000
032100                 PERFORM S2211-DBZ-SEL                            02960000
032200     ELSE                                                         02970000
032300           SET ARPO-RC-F-SELEKT TO TRUE                           02980000
032400     END-IF.                                                      02990000
032500 S221-EXIT.                                                       03000000
032600      EXIT.                                                       03010000
032700/                                                                 03020000
032800* ------------------------------------------------------------*   03040000
032900* ZUGRIFF:   SELECT MIT RS_SCHL                               *   03050000
033000* ------------------------------------------------------------*   03060000
033100 S2211-DBZ-SEL                SECTION.                            03061000
033200 S2211-010.                                                       03062000
033300*<#986   MOVE ARKY-PRI-KEY-C (01)    TO ABR-SCHL.                 03070000
033400      MOVE ARKY-PRI-KEY-C (01)    TO ABR-SCHL OF DCLAR-ABRECH-DET.03071000
033500*                                                                 03080000
033600      EXEC SQL                                                    03090000
033700           SELECT                                                 03100000
033800                 TAR_SYM                                          03110000
033900               , ABR_SCHL                                         03120000
034000               , TAR_BETR                                         03130000
034100               , TAR_WHG                                          03140000
034200               , RABATT_PROZ                                      03150000
034300               , RABATT_BETR                                      03160000
034400               , RABATT_WHG                                       03170000
034500               , RECH_BETR                                        03180000
034600               , RECH_WHG                                         03190000
034700               , RECH_MM_BETR                                     03200000
034800               , RECH_MM_WHG                                      03210000
034900               , KZ_BEMERKUNG                                     03220000
035000           INTO                                                   03250000
035100                 :TAR-SYM             ,                           03260000
035200*<#986           :ABR-SCHL            ,                           03261000
035300                 :DCLAR-ABRECH-DET.ABR-SCHL ,                     03261100
035400                 :TAR-BETR            ,                           03262000
035500                 :TAR-WHG             ,                           03270000
035600                 :RABATT-PROZ         ,                           03280000
035700                 :RABATT-BETR         ,                           03290000
035800                 :RABATT-WHG          ,                           03300000
035900                 :RECH-BETR           ,                           03310000
036000                 :RECH-WHG            ,                           03320000
036100                 :RECH-MM-BETR        ,                           03330000
036200                 :RECH-MM-WHG         ,                           03340000
036300                 :KZ-BEMERKUNG                                    03350000
036400          FROM   AR_ABRECH_DET                                    03400000
036500          WHERE  TAR_SYM  = :TAR-SYM                              03410000
036600      END-EXEC.                                                   03420000
036700 S2211-EXIT.                                                      03430000
036800      EXIT.                                                       03440000
036900* ---------------------------------------------------------- *    03450000
037000* STEUERUNG:  UPDATE-ZUGRIFFE                                *    03460000
037100* ---------------------------------------------------------- *    03470000
037200 S23-DBZ-UPDATE               SECTION.                            03480000
037300 S23-010.                                                         03490000
037400     EVALUATE  TRUE                                               03500000
037500     WHEN      ARPO-OP-INSERT                                     03510000
037600               PERFORM S231-DBZ-INSERT                            03520000
037700     WHEN      ARPO-OP-UPDATE                                     03530000
037800               PERFORM S232-DBZ-UPDATE                            03540000
037900     WHEN      ARPO-OP-DELETE                                     03550000
038000               PERFORM S233-DBZ-DELETE                            03560000
038100     WHEN      OTHER                                              03570000
038200               SET ARPO-RC-F-OPERAT TO TRUE                       03580000
038300     END-EVALUATE.                                                03590000
038400 S23-EXIT.                                                        03600000
038500      EXIT.                                                       03610000
038600* ---------------------------------------------------------- *    03620000
038700* ZUGRIFF: INSERT                                            *    03630000
038800* ---------------------------------------------------------- *    03640000
038900 S231-DBZ-INSERT              SECTION.                            03650000
039000 S231-010.                                                        03651000
039100      EXEC SQL                                                    03660000
039200           INSERT                                                 03670000
039300             INTO  AR_ABRECH_DET                                  03680000
039400                (                                                 03690000
039500                 TAR_SYM                                          03700000
039600               , ABR_SCHL                                         03710000
039700               , TAR_BETR                                         03720000
039800               , TAR_WHG                                          03730000
039900               , RABATT_PROZ                                      03740000
040000               , RABATT_BETR                                      03750000
040100               , RABATT_WHG                                       03760000
040200               , RECH_BETR                                        03770000
040300               , RECH_WHG                                         03780000
040400               , RECH_MM_BETR                                     03790000
040500               , RECH_MM_WHG                                      03800000
040600               , KZ_BEMERKUNG                                     03810000
040700                )                                                 03840000
040800           VALUES                                                 03850000
040900                (                                                 03860000
041000                 :TAR-SYM             ,                           03880000
041100*#986            :ABR-SCHL            ,                           03890000
041200                 :DCLAR-ABRECH-DET.ABR-SCHL ,                     03891000
041300                 :TAR-BETR            ,                           03900000
041400                 :TAR-WHG             ,                           03910000
041500                 :RABATT-PROZ         ,                           03920000
041600                 :RABATT-BETR         ,                           03930000
041700                 :RABATT-WHG          ,                           03940000
041800                 :RECH-BETR           ,                           03950000
041900                 :RECH-WHG            ,                           03960000
042000                 :RECH-MM-BETR        ,                           03970000
042100                 :RECH-MM-WHG         ,                           03980000
042200                 :KZ-BEMERKUNG                                    03990000
042300                )                                                 04010000
042400      END-EXEC.                                                   04020000
042500 S231-EXIT.                                                       04030000
042600      EXIT.                                                       04040000
042700* ---------------------------------------------------------- *    04050000
042800* ZUGRIFF: UPDATE                                            *    04060000
042900* ---------------------------------------------------------- *    04070000
043000 S232-DBZ-UPDATE              SECTION.                            04080000
043100 S232-010.                                                        04081000
043200     EXEC SQL                                                     04090000
043300          UPDATE AR_ABRECH_DET                                    04100000
043400             SET                                                  04110000
043500*<#986           ABR_SCHL        = :ABR-SCHL                      04113000
043600                 ABR_SCHL        = :DCLAR-ABRECH-DET.ABR-SCHL     04113100
043700               , TAR_BETR        = :TAR-BETR                      04114000
043800               , TAR_WHG         = :TAR-WHG                       04115000
043900               , RABATT_PROZ     = :RABATT-PROZ                   04116000
044000               , RABATT_BETR     = :RABATT-BETR                   04117000
044100               , RABATT_WHG      = :RABATT-WHG                    04118000
044200               , RECH_BETR       = :RECH-BETR                     04119000
044300               , RECH_WHG        = :RECH-WHG                      04119100
044400               , RECH_MM_BETR    = :RECH-MM-BETR                  04119200
044500               , RECH_MM_WHG     = :RECH-MM-WHG                   04119300
044600               , KZ_BEMERKUNG    = :KZ-BEMERKUNG                  04119400
044700            WHERE TAR_SYM  = :TAR-SYM                             04260000
044800     END-EXEC.                                                    04270000
044900 S232-EXIT.                                                       04280000
045000     EXIT.                                                        04290000
045100* ---------------------------------------------------------- *    04300000
045200* DELETE ZUGRIFF                                            *     04310000
045300* ---------------------------------------------------------- *    04320000
045400 S233-DBZ-DELETE              SECTION.                            04330000
045500* ZUR ZEIT NICHT VERLANGT                                         04340000
045600 S233-EXIT.                                                       04350000
045700     EXIT.                                                        04360000
045800* ---------------------------------------------------------- *    04370000
045900* STEUERUNG:  OPEN-ZUGRIFFE                                 *     04380000
046000* ---------------------------------------------------------- *    04390000
046100 S24-DBZ-OPEN                 SECTION.                            04400000
046200 S24-010.                                                         04410000
046300     EVALUATE   TRUE                                              04420000
046400     WHEN       ARPO-OP-OPEN-DB                                   04430000
046500                CONTINUE                                          04440000
046600     WHEN       ARPO-OP-OPEN-CR                                   04450000
046700                PERFORM U1-VERARB-CURSOR                          04460000
046800     WHEN       OTHER                                             04470000
046900                SET ARPO-RC-F-OPERAT  TO TRUE                     04480000
047000     END-EVALUATE.                                                04490000
047100 S24-EXIT.                                                        04500000
047200     EXIT.                                                        04510000
047300* ---------------------------------------------------------- *    04520000
047400* STEUERUNG:  CLOSE-ZUGRIFFE                                 *    04530000
047500* ---------------------------------------------------------- *    04540000
047600 S25-DBZ-CLOSE                SECTION.                            04550000
047700 S25-010.                                                         04560000
047800     EVALUATE   TRUE                                              04570000
047900     WHEN       ARPO-OP-CLOSE-DB                                  04580000
048000                CONTINUE                                          04590000
048100     WHEN       ARPO-OP-CLOSE-CR                                  04600000
048200                PERFORM U1-VERARB-CURSOR                          04610000
048300     WHEN       OTHER                                             04620000
048400                SET ARPO-RC-F-OPERAT  TO TRUE                     04630000
048500     END-EVALUATE.                                                04640000
048600 S25-EXIT.                                                        04650000
048700     EXIT.                                                        04660000
048800* ----------------------------------------------------------- *   04670000
048900* STD-SQL-FEHLERROUTINE                                       *   04680000
049000* ----------------------------------------------------------- *   04690000
049100 S26-SQLSTATE           SECTION.                                  04700000
049200     IF SQLSTATE NOT = '00000'                                    04710000
049300       SET ARPO-RC-F-SQL     TO TRUE                              04720000
049400     END-IF.                                                      04721000
049500 S26-EXIT.                                                        04730000
049600     EXIT.                                                        04740000
049700* ----------------------------------------------------------- *   04750000
049800* ABSCHLUSS-ROUTINEAB                                         *   04760000
049900* ----------------------------------------------------------- *   04770000
050000 S3-NACHLAUF                  SECTION.                            04780000
050100     IF FEHLER                                                    04781000
050200        SET ARPO-RC-F-SQL TO TRUE                                 04782000
050300        MOVE SQLCODE      TO ARPO-OBJ-MZ-SQLC                     04783000
050400        MOVE 'L1002'      TO ARPO-OBJ-MELD-NR                     04784000
050500     END-IF.                                                      04785000
050600 S3-010.                                                          04800000
050700 S3-EXIT.                                                         04801000
050800     EXIT.                                                        04810000
050900* ----------------------------------------------------------- *   04820000
051000* ZUGRIFF: ZEILEN-TAB                                         *   04830000
051100* ----------------------------------------------------------- *   04840000
051200*                                                                 04850000
051300 U1-VERARB-CURSOR             SECTION.                            04860000
051400 U1-010.                                                          04870000
051500     PERFORM U2-VERARB-CURSOR.                                    04880000
051600 U1-EXIT.                                                         04890000
051700     EXIT.                                                        04900000
051800* ----------------------------------------------------------- *   04910000
051900* ZUGRIFF: ZEILEN-TAB - DIE AUSGABE BESTEHT AUS TEIL SPALTEN  *   04920000
052000* ----------------------------------------------------------- *   04930000
052100 U2-VERARB-CURSOR             SECTION.                            04940000
052200 U2-010.                                                          04950000
052300     EVALUATE   TRUE                                              04960000
052400     WHEN       ARPO-OP-OPEN-CR                                   04970000
052500                EXEC SQL                                          04980000
052600                     OPEN C1-AD-ABRECH                            04990000
052700                END-EXEC                                          05000000
052800     WHEN       ARPO-OP-CLOSE-CR                                  05010000
052900                EXEC SQL                                          05020000
053000                     CLOSE C1-AD-ABRECH                           05030000
053100                END-EXEC                                          05040000
053200     WHEN       ARPO-OP-FETCH                                     05050000
053300                                                                  05080000
053400                PERFORM UNTIL SQLCODE NOT = 0                     05100000
053500                  EXEC SQL                                        05120000
053600                     FETCH C1-AD-ABRECH INTO                      05130000
053700                                    :TAR-SYM ,                    05140000
053800*<#986                              :ABR-SCHL ,                   05141000
053900                                    :DCLAR-ABRECH-DET.ABR-SCHL,   05141100
054000                                    :RECH-BETR ,                  05142000
054100                                    :RECH-WHG ,                   05143000
054200*#986 - ZZGL. RECH-MM-BETR                                        05143100
054300                                    :RECH-MM-BETR                 05144000
054400                  END-EXEC                                        05150000
054500                  EVALUATE  SQLCODE                               05160000
054600                  WHEN      ZEROES                                05170000
054700                            SET ARPO-RC-OK TO TRUE                05180000
054800                            PERFORM U31-VERARB-TAB                05190000
054900                  WHEN      +100                                  05200000
055000                            SET ARPO-RC-ENDE-DATEN TO TRUE        05210000
055100* KEINE DATEN                                                     05220000
055200                  WHEN      OTHER                                 05230000
055300* SQLCODE ANALYSE                                                 05240000
055400                            SET ARPO-RC-F-SQL TO TRUE             05250000
055500                  END-EVALUATE                                    05260000
055600                END-PERFORM                                       05270000
055700     END-EVALUATE.                                                05280000
055800 U2-090.                                                          05290000
055900 U2-EXIT.                                                         05300000
056000     EXIT.                                                        05310000
056100* ----------------------------------------------------------- *   05430000
056200* VERARBEITUNG TABELLE                                        *   05440000
056300* ----------------------------------------------------------- *   05450000
056400 U31-VERARB-TAB               SECTION.                            05460000
056500 U31-010.                                                         05470000
056600*                                                                 05470100
056700* BEGINN ‰NDERUNG V1.02- GFP/AL4 - HEIDI WELTER                   05470200
056800*                                                                 05470300
056900     IF TAR-SYM (2:1) = ('5' OR '6' OR '7' OR '8' OR '9'          05471000
057000                      OR 'E' OR 'F' OR 'G' OR 'H')                05471100
057100***= PERSONENBEZ. PAUSCH. TARIF                                   05472000
057200        MOVE 'P' TO PTAR-ART                                      05480100
057300     ELSE                                                         05480200
057400        MOVE ' ' TO PTAR-ART                                      05480300
057500     END-IF                                                       05480400
057600*                                                                 05480500
057700     EVALUATE   TRUE                                              05480600
057800     WHEN       TAR-SYM = 'A ' OR 'A1'                            05480700
057900                               OR 'A5'                            05480800
058000                       OR 'AA' OR 'AE'                            05480900
058100                MOVE RECH-BETR   TO PTAR-A                        05481000
058200                MOVE RECH-WHG-TEXT  TO RECH-A-WHG                 05481100
058300     WHEN       TAR-SYM = 'I ' OR 'I1' OR 'I2'                    05481200
058400                               OR 'I5' OR 'I6'                    05481300
058500                               OR 'IA' OR 'IB' OR 'IE' OR 'IF'    05481400
058600                MOVE RECH-BETR   TO PTAR-I                        05481500
058700                MOVE RECH-WHG-TEXT  TO RECH-I-WHG                 05481600
058800     WHEN       TAR-SYM = 'R ' OR 'R1' OR 'R2' OR 'R3' OR 'R4'    05481700
058900                       OR 'R5' OR 'R6' OR 'R7' OR 'R8'            05481800
059000                       OR 'RA' OR 'RB' OR 'RC' OR 'RD'            05481900
059100                       OR 'RE' OR 'RF' OR 'RG' OR 'RH'            05482000
059200                MOVE RECH-BETR   TO PTAR-R                        05482100
059300                MOVE RECH-WHG-TEXT  TO RECH-R-WHG                 05482200
059400     WHEN       TAR-SYM = 'V ' OR 'V1'                            05482300
059500                               OR 'V5'                            05482400
059600                MOVE RECH-BETR   TO PTAR-V                        05482500
059700                MOVE RECH-WHG-TEXT  TO RECH-V-WHG                 05482600
059800     WHEN       TAR-SYM = 'H ' OR 'H1'                            05482700
059900                               OR 'H5'                            05482800
060000                MOVE RECH-BETR   TO PTAR-H                        05482900
060100                MOVE RECH-WHG-TEXT  TO RECH-H-WHG                 05483000
060200     WHEN       TAR-SYM = 'HK'                                    05483100
060300                MOVE RECH-BETR   TO PTAR-HK                       05483200
060400                MOVE RECH-WHG-TEXT  TO RECH-HK-WHG                05483300
060500     WHEN       TAR-SYM = 'P1' OR 'A5' OR 'AE'                    05483400
060600                ADD  RECH-BETR   TO PTAR-P1                       05483500
060700                MOVE RECH-WHG-TEXT  TO RECH-P1-WHG                05483600
060800*==> #986                                                         05483700
060900                IF TAR-SYM = 'P1'                                 05483800
061000                THEN                                              05483900
061100* SQLCODE VOM ABRECH-DET-ZUGRIFF MERKEN                           05484000
061200                   MOVE SQLCODE TO ZW-ABRECH-DET-SQLCODE          05484100
061300                   PERFORM U32-WERK-SEL                           05484200
061400                   PERFORM U33-MV-SEL                             05484300
061500                   PERFORM UP92-FREQUENZ                          05484400
061600                   INITIALIZE ZW-BETR1                            05484500
061700                   COMPUTE ZW-BETR1   ROUNDED  =                  05484600
061800                           RECH-MM-BETR OF DCLAR-ABRECH-DET       05484700
061900                                         * ZW-MULTIPLIKATOR       05484800
062000                   ADD     ZW-BETR1   TO   PTAR-P1                05484900
062100* SQLCODE WIEDER AUF JENEN VOM ABRECH-DET-ZUGRIFF SETZEN          05485000
062200                   MOVE ZW-ABRECH-DET-SQLCODE TO SQLCODE          05485100
062300                END-IF                                            05485200
062400*ENDE #986                                                        05485300
062500     WHEN       TAR-SYM = 'P2'                                    05485400
062600                ADD  RECH-BETR   TO PTAR-P2                       05485500
062700                MOVE RECH-WHG-TEXT  TO RECH-P2-WHG                05485600
062800*==> #986                                                         05485700
062900                IF TAR-SYM = 'P2'                                 05485800
063000                THEN                                              05485900
063100* SQLCODE VOM ABRECH-DET-ZUGRIFF MERKEN                           05486000
063200                   MOVE SQLCODE TO ZW-ABRECH-DET-SQLCODE          05486100
063300                   PERFORM U32-WERK-SEL                           05486200
063400                   PERFORM U33-MV-SEL                             05486300
063500                   PERFORM UP92-FREQUENZ                          05486400
063600                   INITIALIZE ZW-BETR2                            05486500
063700                   COMPUTE ZW-BETR2   ROUNDED  =                  05486600
063800                           RECH-MM-BETR OF DCLAR-ABRECH-DET       05486700
063900                                         * ZW-MULTIPLIKATOR       05486800
064000                   ADD     ZW-BETR2   TO   PTAR-P2                05486900
064100* SQLCODE WIEDER AUF JENEN VOM ABRECH-DET-ZUGRIFF SETZEN          05487000
064200                   MOVE ZW-ABRECH-DET-SQLCODE TO SQLCODE          05487100
064300                END-IF                                            05487200
064400*ENDE #986                                                        05487300
064500     WHEN       OTHER                                             05487400
064600                CONTINUE                                          05487500
064700*                                                                 05487600
064800* ENDE ‰NDERUNG V1.02- GFP/AL4 - HEIDI WELTER                     05487700
064900*                                                                 05487800
065000     END-EVALUATE.                                                05488000
065100 U31-090.                                                         05520000
065200     EXIT.                                                        05530000
065300/                                                                 05540000
065400* ------------------------------------------------------------*   05560000
065500* #986 - DATEN AUS WERK, F¸R DIE JAHRESPAUSCHALEN-BERECHNUNG  *   05570000
065600* ZUGRIFF:   SELECT AUF AR_WERK MIT WK_SCHL                   *   05571000
065700* ------------------------------------------------------------*   05580000
065800 U32-WERK-SEL               SECTION.                              05590000
065900 U32-010.                                                         05600000
066000      MOVE ARKY-PRI-KEY-C (01)    TO  ABR-SCHL OF DCLAR-ABRECH    05610000
066100*                                                                 05620000
066200      EXEC SQL                                                    05630000
066300           SELECT                                                 05640000
066400                VALUE (FREQ_PAUSCH,0)                             05680000
066500               ,MV_SCHL                                           05700000
066600               ,VALUE (MONTH (DAT_ABR_PAUSCH),0)                  05750000
066700               ,ABR_JJJJMM                                        05760000
066800           INTO                                                   05770000
066900                 :DCLAR-WERK.FREQ-PAUSCH                          05780000
067000               , :DCLAR-WERK.MV-SCHL                              05800000
067100               , :ZW-DB-PAUSCH-MM                                 05810000
067200               , :DCLAR-ABRECH.ABR-JJJJMM                         05820000
067300          FROM   AR_WERK  A                                       05900000
067400                ,AR_ABRECH B                                      05901000
067500          WHERE  B.ABR_SCHL  = :DCLAR-ABRECH.ABR-SCHL             05911000
067600          AND    A.WK_SCHL   = B.WK_SCHL                          05912000
067700      END-EXEC.                                                   05920000
067800      MOVE ABR-JJJJMM  OF DCLAR-ABRECH                            05920100
067900      TO   ZW-ABR-JJJJMM-N.                                       05921000
068000      MOVE FREQ-PAUSCH OF DCLAR-WERK                              05921100
068100      TO   ZW-FREQ.                                               05922000
068200 U32-EXIT.                                                        05930000
068300      EXIT.                                                       05940000
068400/                                                                 05950000
068500* ------------------------------------------------------------*   05960000
068600* #986 - DATEN AUS MV, F¸R DIE JAHREPAUSCHALEN-BERECHNUNG     *   05970000
068700* ZUGRIFF:   SELECT AUF AR_MANTELVERTRAG MIT MV_SCHL          *   05980000
068800* ------------------------------------------------------------*   05990000
068900 U33-MV-SEL               SECTION.                                06000000
069000 U33-010.                                                         06010000
069100      EXEC SQL                                                    06040000
069200           SELECT                                                 06050000
069300                MONTH (DAT_VERTR_BEGINN)                          06060000
069400           INTO                                                   06090000
069500                 :ZW-DB-VERTR-MM                                  06100000
069600          FROM   AR_MANTELVERTRAG                                 06130000
069700          WHERE  MV_SCHL  = :DCLAR-WERK.MV-SCHL                   06150000
069800      END-EXEC.                                                   06170000
069900 U33-EXIT.                                                        06180000
070000      EXIT.                                                       06190000
070100/*************************************************************    06200000
070200*   #986 - ANALOG ARPRDR11                                        06210000
070300*   ERMITTLUNG DER RELEVANTEN ABRECHNUNGSMONATE PRO JAHR          06211000
070400**************************************************************    06220000
070500 UP92-FREQUENZ SECTION.                                           06230000
070600 UP92-00.                                                         06240000
070700*                                                                 06250000
070800     MOVE    '1'                        TO MK-LESEN               06260000
070900*                                                                 06270000
071000*    IN DER ZW-MONATSTABELLE WIRD FESTGELEGT, IN WELCHEN MONATEN  06280000
071100*    ABGERECHNET WERDEN SOLL. DIE MONATE, DIE GEM‰ﬂ FREQUENZ      06290000
071200*    RELEVANT SIND, WERDEN MIT 1 GEF¸LLT, DIE NICHT RELEVANTEN    06300000
071300*    MIT 0. BEISPIEL F¸R EINE VIERTELJ‰HRLICHE ABRECHNUNG MIT DEM 06310000
071400*    ABRECHNUNGSMONAT 10                                          06320000
071500*    ZW-MONATSTABELLE = 100100100100                              06330000
071600*                                                                 06340000
071700     INITIALIZE ZW-MONATSTABELLE.                                 06350000
071800*                                                                 06360000
071900* IND1 WIRD MIT MONAT VON AR_WERK.DAT_ABR_PAUSCH BELEGT           06400000
072000     SET     IND1    TO  ZW-DB-PAUSCH-MM                          06401000
072100*                                                                 06402000
072200* DEFAULTWERT FUER INDEX MONAT SETZEN WENN DIESER NICHT           06402100
072300* GEFUELLT IST                                                    06402200
072400* WENN AR_WERK.DAT_ABR_PAUSCH NICHT GEF¸HLT SEIN SOLLTE, WIRD     06403000
072500* DER MONAT VON AR_MANTELVERTRAG.DAT_VERTR_BEGINN GENUTZT         06404000
072600     IF      IND1                        = 0                      06410000
072700             SET     IND1               TO ZW-DB-VERTR-MM         06420000
072800     END-IF                                                       06430000
072900     .                                                            06440000
073000     MOVE    1                          TO ZW-TAB-MM (IND1).      06450000
073100*                                                                 06460000
073200* ZW-FREQ = 0 : MONATLICH                                         06470000
073300*                                                                 06480000
073400     IF      ZW-FREQ                     = 0                      06490000
073500             MOVE    11                 TO ZW-ANZAHL              06500000
073600             MOVE    01                 TO ZW-SUMMAND             06510000
073700             MOVE    01                 TO ZW-MULTIPLIKATOR       06520000
073800     END-IF                                                       06530000
073900     .                                                            06540000
074000*                                                                 06550000
074100* ZW-FREQ = 1 : VIERTELJ‰HRLICH                                   06560000
074200*                                                                 06570000
074300     IF      ZW-FREQ                     = 1                      06580000
074400             MOVE    03                 TO ZW-ANZAHL              06590000
074500             MOVE    03                 TO ZW-SUMMAND             06600000
074600             MOVE    03                 TO ZW-MULTIPLIKATOR       06610000
074700     END-IF                                                       06630000
074800     .                                                            06640000
074900*                                                                 06650000
075000* ZW-FREQ = 2 : HALBJ‰HRLICH                                      06660000
075100*                                                                 06670000
075200     IF      ZW-FREQ                     = 2                      06680000
075300             MOVE    01                 TO ZW-ANZAHL              06690000
075400             MOVE    06                 TO ZW-SUMMAND             06700000
075500             MOVE    06                 TO ZW-MULTIPLIKATOR       06710000
075600     END-IF                                                       06720000
075700     .                                                            06730000
075800*                                                                 06740000
075900* ZW-FREQ = 3 : J‰HRLICH                                          06750000
076000*                                                                 06760000
076100     IF      ZW-FREQ                     = 3                      06770000
076200             MOVE    12                 TO ZW-MULTIPLIKATOR       06780000
076300     END-IF                                                       06790000
076400     .                                                            06800000
076500*                                                                 06810000
076600     IF      ZW-FREQ                     < 3                      06830000
076700             PERFORM ZW-ANZAHL TIMES                              06840000
076800              SET    IND1            UP BY ZW-SUMMAND             06850000
076900              IF     IND1                > 12                     06860000
077000                     SET     IND1  DOWN BY 12                     06870000
077100              END-IF                                              06880000
077200              MOVE    1                 TO ZW-TAB-MM (IND1)       06890000
077300             END-PERFORM                                          06900000
077400     END-IF                                                       06910000
077500     .                                                            06920000
077600     SET     IND1                       TO ZW-ABR-MM              06930000
077700     MOVE    ZW-TAB-MM (IND1)           TO MK-LESEN               06940000
077800*                                                                 06950000
077900     IF      UEBERLESEN                                           06960000
078000             MOVE    0                  TO ZW-MULTIPLIKATOR       06970000
078100     END-IF                                                       06980000
078200     .                                                            07000000
078300*                                                                 07010000
078400 UP92-99.                                                         07020000
078500     EXIT.                                                        07030000
