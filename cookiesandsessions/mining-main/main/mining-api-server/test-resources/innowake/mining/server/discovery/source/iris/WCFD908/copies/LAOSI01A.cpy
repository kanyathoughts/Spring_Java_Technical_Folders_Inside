000100***************************************************************** 00010000
000200*                                                               * 00020000
000300*        Cobol - Copybook FUER DAS SSI-A                        * 00030000
000400*                                                               * 00040000
000500***************************************************************** 00050000
000600* GEÄNDERT AM  ! VON            ! ÄNDERUNG                      * 00060000
000700*==============+================+===============================* 00070000
000800*    09/2001   ! Pötschke       ! Ersterstellung                * 00080000
000900*--------------+----------------+-------------------------------* 00090000
001000*    06/2002   ! Pötschke       ! Neue Felder                   * 00100000
001100*--------------+----------------+-------------------------------* 00110000
001200*    01/2004   ! Pötschke       ! Neue Felder                   * 00120000
001300*--------------+----------------+-------------------------------* 00130000
001400*    04/2004   ! Pötschke       ! Neue Felder                   * 00140000
001500*--------------+----------------+-------------------------------* 00150000
001600*    12/2004   ! Pötschke       ! Neue Felder                   * 00160000
001700*--------------+----------------+-------------------------------* 00170000
001800*    03/2005   ! Straßfeld      ! Neue Felder                   * 00180000
001900*--------------+----------------+-------------------------------* 00190000
002000*    04/2005   ! Pötschke       ! Neue Felder                   * 00200000
002100*--------------+----------------+-------------------------------* 00210000
002200*    05/2005   ! Pötschke       ! Neue Felder                   * 00220000
002300*--------------+----------------+-------------------------------* 00230000
002400*    06/2005   ! Petzoldt       ! Neue Felder und dies richtig  * 00240000
002500*--------------+----------------+-------------------------------* 00250000
002600*    08/2005   ! Welter         !- Entfernung abw.Ratenzuschl.JN* 00260000
002700*              !                !- Entfernung abw.RabattJN      * 00270000
002800*              !                !- Aufnahme GROSSABKKZ          * 00280000
002900*--------------+----------------+-------------------------------* 00290000
003000*    09/2005   ! Straßfeld      ! Schriftsteuerung              * 00300000
003100*--------------+----------------+-------------------------------* 00310000
003200*    10/2005   ! Welter         !  Gewinnbenachrichtigung       * 00320000
003300*              !                !  -Art/-Weg/-Empf/-Monat/-AbkKz* 00330000
003400*--------------+----------------+-------------------------------* 00340000
003500*    11/2005   ! WELTER         !  GEWINNBENACHRICHTIGUNG       * 00350000
003600*              !                !  AUCH IN METHODE 15           * 00360000
003700*--------------+----------------+-------------------------------* 00370000
003800* 02.12.2005   ! WELTER         !  zzgl. Konsorten-Name u. -Ort * 00380000
003900*--------------+----------------+-------------------------------* 00390000
004000* 08.12.2005   ! WELTER         !  zzgl.: VORLAGEINKASSOSBJN    * 00400000
004100*              !                !         VORLAGESBKZ           * 00410000
004200*              !                !         ABKABRECHNUNGKZ       * 00420000
004300*--------------+----------------+-------------------------------* 00430000
004400* 01.03.2006   ! WELTER         ! DB-Change45 - Konsortial:     * 00440000
004500*              !                ! - Differenzierung FedFührKost.* 00450000
004600*              !                ! - KonsortVertragKz (statt JN) * 00460000
004700*              !                ! - Wegfall: GewinnermittlungKz * 00470000
004800*--------------+----------------+-------------------------------* 00480000
004900* 02.11.2006   ! WELTER         ! Methode 10/Partnerdaten       * 00490000
005000*              !                ! zusätzl.:                     * 00500000
005100*              !                ! - AbkommensbranchenKz         * 00510000
005200*--------------+----------------+-------------------------------* 00520000
005300* 02.05.2007   ! WELTER         ! Methode 1/Abkommensgrunddaten * 00530000
005400*              !                ! zusätzl.:                     * 00540000
005500*              !                ! - AbkommensbranchenKz         * 00550000
005600*              !                ! neue Methode/n wg. Druck      * 00560000
005700*              !                ! - AbkommensbranchenKz         * 00570000
005800*--------------+----------------+-------------------------------* 00580000
005900* 21.09.2007   ! WELTER         ! Entfernung RentenuebergangKz  * 00590000
006000*--------------+----------------+-------------------------------* 00600000
006100* 24.09.2007   ! SCHRÖDER       ! neu: BO04-Druckoptionen       * 00610000
006200*              !                ! Methode 16                    * 00620000
006300* Rel.4.9/01/08! WELTER         !(ACHTUNG: Inaktiv - bei Akti-  * 00621000
006400*              !                ! vierung, OCCURS bei den       * 00622000
006500*              !                ! Kopien von 6 auf 5 reduzieren * 00623000
006600*              !                ! Dabei Abgleich mit BO-04-     * 00624000
006700*              !                ! Druckoptionen & DB-Modell !!!)* 00625000
006800*--------------+----------------+-------------------------------* 00626000
006900* 30.10.2007   ! SCHRÖDER       ! Methode 6 entfernt            * 00627000
007000* 06.11.2007   !                ! neu: ANTMODELLVERZICHKZ       * 00628000
007100*              !                !      RISKGRPZUSCHLAGKZ        * 00629000
007200*              !                !      ABWRISKGRPZUSCHLAG       * 00630000
007300*--------------+----------------+-------------------------------* 00640000
007400* 15.01.2008   ! WELTER         ! Methode 4/Pricing zusätzl.    * 00650000
007500*              !                ! - ALLEVERTRARTENJN            * 00660000
007600*--------------+----------------+-------------------------------* 00670000
007700* 21.02.2008   ! WELTER         ! METHODE 8/KOMBIGRUPPE - NEU:  * 00671000
007800*              !                ! - LISTMANNAHMEERKLKZ          * 00672000
007900*--------------+----------------+-------------------------------* 00673000
008000* 28.04.2008   ! WELTER         ! METHODE 12/KONSORTIALDATEN:   * 00674000
008100*              !                ! - Konsorten-Name von 32 auf 64* 00675000
008200*              !                !   verlängert                  * 00676000
008300*--------------+----------------+-------------------------------* 00677000
008400* 15.07.2008   ! WELTER         ! METHODE 13/VT-Daten-Rente->NEU* 00678000
008500*              !                ! - GESETZLREINTRALTKZ          * 00679000
008600*--------------+----------------+-------------------------------* 00679100
008700* 15.07.2008   ! WELTER         ! METHODE 12/Konsortial ->NEU:  * 00679200
008800*              !                ! - BILANZ-BINDENR              * 00679300
008900*--------------+----------------+-------------------------------* 00679400
009000* 06.04.2009   ! WELTER         ! METHODE 12/Konsortial ->NEU:  * 00679500
009100*              !                ! - VUKONTOKORRKONTONR          * 00679600
009200*              !                ! - VUABKNR                     * 00679700
009300*--------------+----------------+-------------------------------* 00679800
009400* 29.07.2009   ! WELTER         ! Rel. 4. 16: div. neue Felder  * 00679900
009500*--------------+----------------+-------------------------------* 00680000
009600* 12.11.2009   ! WELTER         ! METHODE 8/KOMBIGRUPPE - NEU:  * 00680100
009700*              !                ! - ARTRISIKOPRKZ               * 00680200
009800*--------------+----------------+-------------------------------* 00680300
009900* 16.02.2010   ! WELTER         ! Rel. 4. 17: listmAblaufSchrKz * 00680400
010000*--------------+----------------+-------------------------------* 00680500
010100* 07.06.2010   ! WELTER         ! NEUE METHODE 17/VT-Daten-Kapital00680600
010200*--------------+----------------+-------------------------------* 00680700
010300* 11.03.2011   ! WELTER         ! rel. 5.3 - neue Datenfelder:    00680800
010400*              !                ! - kostenVersionKz  (Pricing)    00680900
010500*              !                ! - talanxKonsortVarKz (Konsort.) 00681000
010600*              !                ! - konsortBesonderhKz (Grunddat.)00681100
010700*              !                ! - LISTMABLSCHRKAPKZ(KOMBIGRUPPE)00681200
010800*              !                ! - VERSABLSCHRRENKZ (KOMBIGRUPPE)00681300
010900*              !                ! - VERSABLSCHRKAPKZ (KOMBIGRUPPE)00681400
011000*--------------+----------------+-------------------------------* 00681500
011100* 02.09.2011   ! WELTER         !-NEUE METHODE 18/Standardabk mit 00681600
011200*              !                ! Pointer                         00681700
011300*              !                !-NEUE DF bei Methode 1           00681800
011400*--------------+----------------+-------------------------------* 00681900
011500* 26.03.2012   ! WELTER         ! Rel.5.5-div.(neue)Datenfelder:  00682000
011600*              !                ! Methode 1:-BUCONTROLLINGKZ      00682100
011700*              !                !           -ABSCHLPOTENTPERS     00682200
011800*              !                ! Methode 8:-KMBGRBEZ 80stellig   00682300
011900*              !                !           -RECHTLWARTEZEITMON   00682400
012000*              !                !           -FRISTRISKPRUEFDAT    00682500
012100*              !                !           -AUSCHLTARIFBWKZ      00682600
012200*              !                !           -AUSCHLRENTSTEIGKZ    00682700
012300*              !                !           -MAXPRAEMIENDYNKZ     00682800
012400*              !                !           -MAXPRAEMIENDYNPROZ   00682900
012500*--------------+----------------+-------------------------------* 00683000
012600* 26.06.2012   ! WELTER         ! Rel.5.6-div.(neue)Datenfelder:  00683100
012700*              ! LC-4862        ! Methode  8: archivDruckStKz     00683200
012800*              !                ! Methode 13: vereinbarterBegMon  00683300
012900*              !                ! Methode 17: vereinbarterBegMon  00683400
013000*              !                ! Methode 19-NEU: LA_ABkFLV       00683500
013100*              !                ! Methode 20-NEU:LA_ABkbupruefung 00683600
013200*--------------+----------------+-------------------------------* 00683700
013300* 19.07.2012   ! WELTER         ! Rel.5.6-div.(neue)Datenfelder:  00683800
013400*              ! LC-4862        ! Methode  2: unisexKalkStufeKz   00683900
013500*              !                ! Methode 14: anteilRiskGrProz    00684000
013600*--------------+----------------+-------------------------------* 00684100
013700* 05.10.2012   ! WELTER         ! Rel.5.7-div.(neue)Datenfelder:  00684200
013800*              ! 44903/Unisex   ! Methode 8: -artVorgUnisex       00684300
013900*              !                !            -RisikozukollKz      00684400
014000*              !                ! Methode 14: -anteilRiskGrProz   00684500
014100*              !                ! durch       -ANTEILRISIKOGRUPPE 00684600
014200*              !                ! ersetzt                         00684700
014300*              !                ! RAUS: Methode2-unisexKalkStufeKz00684900
014400*--------------+----------------+-------------------------------* 00685000
014500* 18.09.2014   ! WELTER         ! Rel.6.2                         00685100
014600*              ! P 46289/TG 2015! Methode  4: ABSCHLKOSTENEINMAL  00685200
014700***************************************************************** 00685300
014800                                                                  00685400
014900********************************                                  00685500
015000 10  'LAOSI01A-'KEY-BEREICH.                                      00685600
015100********************************                                  00685700
015200    15  'LAOSI01A-'ANSCHLVERTRKZ           PIC S9(04) COMP.       00686000
015300*                                                                 00687000
015400********************************                                  00688000
015500 10  'LAOSI01A-'DATA-BEREICH.                                     00689000
015600********************************                                  00690000
015700   15  'LAOSI01A-'DATA                     PIC  X(31500).         00700000
015800*                                                                 00710000
015900*   DATENSTRUKTUR FÜR ABKOMMENSGRUNDATEN (Methode-1)              00720000
016000 10  'LAOSI01A-'ABK-DATA REDEFINES 'LAOSI01A-'DATA-BEREICH.       00730000
016100    15 'LAOSI01A-'RECHTKZ                  PIC S9(04) COMP.       00740000
016200    15 'LAOSI01A-'ABKKURZBEZ               PIC  X(32).            00750000
016300    15 'LAOSI01A-'KONSORTVERTRAGKZ         PIC S9(04) COMP.       00760000
016400    15 'LAOSI01A-'STDKORRSPRACHEKZ         PIC  X(04).            00770000
016500    15 'LAOSI01A-'VERZICHTLEBENSNWJN       PIC  X(01).            00780000
016600    15 'LAOSI01A-'GROSSABKKZ               PIC S9(04) COMP.       00790000
016700    15 'LAOSI01A-'KONSORTBESONDERHKZ       PIC S9(04) COMP.       00800000
016800    15 'LAOSI01A-'FUEHRUNGSBSABKKZ         PIC S9(04) COMP.       00810000
016900    15 'LAOSI01A-'FUEHRUNGSABKNR           PIC S9(17)V COMP-3.    00820000
017000    15 'LAOSI01A-'BUCONTROLLINGKZ          PIC S9(04) COMP.       00830000
017100    15 'LAOSI01A-'ABSCHLPOTENTPERS         PIC S9(9) USAGE COMP.  00840000
017200*                                                                 00850000
017300*   DATENSTRUKTUR FÜR PERSONENKREIS        (Methode-2)            00860000
017400 10  'LAOSI01A-'PK-DATA REDEFINES 'LAOSI01A-'DATA-BEREICH.        00870000
017500    15 'LAOSI01A-'BUZ-KLAUSELKZ            PIC S9(04) COMP.       00880000
017600* BUZ-KLAUSEKKZ=BUZ-Ausschlussklausel im PersonenKreis!           00880100
017700* Hat NICHTS mit vers. tech. Daten zu tun (s. u. a. Methode-14)   00880200
017800    15 'LAOSI01A-'UNISEXKALKSTUFEKZ        PIC S9(4) COMP.        00880300
017900*                                                                 00900000
018000*   DATENSTRUKTUR FÜR BAVDATEN (Methode 3, 7, 326)                00910000
018100 10  'LAOSI01A-'BAV-DATA REDEFINES 'LAOSI01A-'DATA-BEREICH.       00920000
018200    15 'LAOSI01A-'BAVARTKZ                 PIC S9(04) COMP.       00930000
018300    15 'LAOSI01A-'BAVPERSTYPKZ             PIC S9(04) COMP.       00940000
018400    15 'LAOSI01A-'PAUSCHSTEUERJN           PIC  X(01).            00950000
018500    15 'LAOSI01A-'AUTOMUEBGANGVNJN         PIC  X(01).            00960000
018600    15 'LAOSI01A-'FLEXALTERSGRENZEJN       PIC  X(01).            00970000
018700    15 'LAOSI01A-'BEZUGRECHTFORMKZ-B       PIC S9(04) COMP.       00980000
018800    15 'LAOSI01A-'STDBEZUGRECHTTODKZ-B     PIC S9(04) COMP.       00990000
018900    15 'LAOSI01A-'BEZUGRECHTVORBEHKZ-B     PIC S9(04) COMP.       01000000
019000    15 'LAOSI01A-'LEISTEINSCHBEZUGKZ-B     PIC S9(04) COMP.       01010000
019100    15 'LAOSI01A-'AUSZVERFUEGTODKZ-B       PIC S9(04) COMP.       01020000
019200    15 'LAOSI01A-'PAUSCHALSTEUERKZ         PIC S9(4) COMP.        01030000
019300    15 'LAOSI01A-'PRAEMZAHLERTYPKZ         PIC S9(4) COMP.        01040000
019400    15 'LAOSI01A-'BEZUGRECHTUVAKZ          PIC S9(4) COMP.        01050000
019500    15 'LAOSI01A-'UEBERTRAGUNGVNUVAJN      PIC  X(1).             01060000
019600    15 'LAOSI01A-'HINWEISPARAGR22JN        PIC  X(1).             01070000
019700    15 'LAOSI01A-'VEREINBUEBTRAGJN         PIC  X(1).             01080000
019800    15 'LAOSI01A-'UEBGVNEINMPRAEMKZ        PIC S9(4) COMP.        01090000
019900    15 'LAOSI01A-'AENRECHAUSVERTODKZ       PIC S9(4) COMP.        01100000
020000    15 'LAOSI01A-'AUSSCHEIDREGELKZ         PIC S9(4) COMP.        01110000
020100    15 'LAOSI01A-'ABMELDEARTKZ             PIC S9(4) COMP.        01120000
020200    15 'LAOSI01A-'ABMELDAUSWIRKUNGKZ       PIC S9(4) COMP.        01130000
020300    15 'LAOSI01A-'ABLOESVERSORDNJN         PIC  X(1).             01140000
020400    15 'LAOSI01A-'AUSZAHLREGELUNGKZ        PIC S9(4) COMP.        01150000
020500    15 'LAOSI01A-'STDBEZUGRECHTERLKZ       PIC S9(4) COMP.        01160000
020600    15 'LAOSI01A-'BEZUGRECHTUVANOTIZ       PIC  X(300).           01170000
020700    15 'LAOSI01A-'VORBEHPRAEMIENJN         PIC  X(1).             01180000
020800    15 'LAOSI01A-'PAUSCHSTEUERZAHLKZ       PIC S9(4) COMP.        01190000
020900    15 'LAOSI01A-'WEITERFUEHRUNGJN         PIC  X(1).             01200000
021000    15 'LAOSI01A-'FOERDERUNGSARTKZ         PIC S9(4) COMP.        01210000
021100*                                                                 01220000
021200*   DATENSTRUKTUR FÜR BEZUGSRECHTSDATEN (Methode 3, 326)          01230000
021300 10  'LAOSI01A-'BRECHT-DATA REDEFINES 'LAOSI01A-'DATA-BEREICH.    01240000
021400    15 'LAOSI01A-'BEZUGRECHTFORMKZ         PIC S9(04) COMP.       01250000
021500    15 'LAOSI01A-'STDBEZUGRECHTTODKZ       PIC S9(04) COMP.       01260000
021600    15 'LAOSI01A-'BEZUGRECHTVORBEHKZ       PIC S9(04) COMP.       01270000
021700    15 'LAOSI01A-'LEISTEINSCHBEZUGKZ       PIC S9(04) COMP.       01280000
021800    15 'LAOSI01A-'AUSZVERFUEGTODKZ         PIC S9(04) COMP.       01290000
021900*                                                                 01300000
022000*   DATENSTRUKTUR FÜR PRICINGDATEN (Methode 4)                    01310000
022100 10  'LAOSI01A-'PRIC-DATA REDEFINES 'LAOSI01A-'DATA-BEREICH.      01320000
022200    15 'LAOSI01A-'VERTRARTKZ               PIC S9(04) COMP.       01330000
022300    15 'LAOSI01A-'KOSTENSYSTEMKZ           PIC S9(04) COMP.       01340000
022400    15 'LAOSI01A-'STUECKKOSTENKZ           PIC S9(04) COMP.       01350000
022500    15 'LAOSI01A-'WAEHVERTRKZ              PIC S9(04) COMP.       01360000
022600    15 'LAOSI01A-'STUECKKOSTENBETR         PIC S9(09)V99 COMP-3.  01370000
022700    15 'LAOSI01A-'KONZERNANGJN             PIC  X(01).            01380000
022800    15 'LAOSI01A-'ZILLMERSATZ              PIC S9(03)V99 COMP-3.  01390000
022900    15 'LAOSI01A-'ABSCHLKOSTEN             PIC S9(03)V99 COMP-3.  01400000
023000    15 'LAOSI01A-'FAKTORGAMMAKOSTEN        PIC S9(03)V99 COMP-3.  01410000
023100    15 'LAOSI01A-'ABWRABATT                PIC S9(03)V99 COMP-3.  01420000
023200    15 'LAOSI01A-'ABWRATENZUSCHLAG         PIC S9(03)V99 COMP-3.  01430000
023300    15 'LAOSI01A-'FAKTGAMMAKOSTFZEIT       PIC S9(03)V99 COMP-3.  01440000
023400    15 'LAOSI01A-'RISKGRPZUSCHLAGKZ        PIC S9(04) COMP.       01450000
023500    15 'LAOSI01A-'ABWRISKGRPZUSCHLAG       PIC S9(03)V99 COMP-3.  01460000
023600    15 'LAOSI01A-'ALLEVERTRARTENJN         PIC  X(01).            01470000
023700    15 'LAOSI01A-'KOSTENVERSIONKZ          PIC S9(04) COMP.       01480000
023800    15 'LAOSI01A-'ABSCHLKOSTENEINMAL       PIC S9(03)V99 COMP-3.  01481000
023900*                                                                 01490000
024000*   DATENSTRUKTUR FÜR INKASSODATEN (Methode 5)                    01500000
024100 10  'LAOSI01A-'INK-DATA REDEFINES 'LAOSI01A-'DATA-BEREICH.       01510000
024200    15 'LAOSI01A-'INKTYPKZ                 PIC S9(04) COMP.       01520000
024300    15 'LAOSI01A-'MTGLBTRBERECHMODKZ       PIC S9(04) COMP.       01530000
024400    15 'LAOSI01A-'INKSTELLEKZ              PIC S9(04) COMP.       01540000
024500    15 'LAOSI01A-'ZAHLWEGLSRETOUREKZ       PIC S9(04) COMP.       01550000
024600    15 'LAOSI01A-'STELLEANZPERSNRMAS       PIC S9(04) COMP.       01560000
024700    15 'LAOSI01A-'GKPERSNR.                                       01570000
024800       16 'LAOSI01A-'GKPERSNR-TAB OCCURS 15.                      01580000
024900          20 'LAOSI01A-'STELLEPERSNRMASKZ  PIC S9(04) COMP.       01590000
025000    15 'LAOSI01A-'BSINKGKKZ                PIC S9(04) COMP.       01600000
025100    15 'LAOSI01A-'INKGKNR                  PIC S9(17) COMP-3.     01610000
025200    15 'LAOSI01A-'LFDNRINKGK               PIC S9(17) COMP-3.     01620000
025300    15 'LAOSI01A-'ABRECHWAEHKZ             PIC S9(04) COMP.       01630000
025400    15 'LAOSI01A-'ZAHLWEGEINZEL.                                  01640000
025500       16 'LAOSI01A-'ZAHLWEGEINZEL-TAB OCCURS 15.                 01650000
025600          20 'LAOSI01A-'ZAHLWEGEINZELKZ    PIC S9(04) COMP.       01660000
025700    15 'LAOSI01A-'ANFORDARTGKKZ            PIC S9(04) COMP.       01670000
025800    15 'LAOSI01A-'LSVARTKZ                 PIC S9(04) COMP.       01680000
025900    15 'LAOSI01A-'DTAJN                    PIC  X(01).            01690000
026000    15 'LAOSI01A-'VORLAGEINKASSOSBJN       PIC  X(01).            01700000
026100    15 'LAOSI01A-'FINGIERTERBEGMONAT       PIC S9(04) COMP.       01710000
026200    15 'LAOSI01A-'ZAHLWEISE                PIC S9(04) COMP.       01711000
026300                                                                  01712000
026400*                                                                 01713000
026500*   DATENSTRUKTUR FÜR KOMBIGRUPPE (Methode 8)                     01714000
026600 10  'LAOSI01A-'KMBGR-DATA REDEFINES 'LAOSI01A-'DATA-BEREICH.     01715000
026700    15 'LAOSI01A-'KMBGRBEZ                 PIC  X(80).            01716000
026800    15 'LAOSI01A-'KMBGRSTATUSKZ            PIC S9(04) COMP.       01717000
026900    15 'LAOSI01A-'POLICEKZ                 PIC S9(04) COMP.       01718000
027000    15 'LAOSI01A-'ANZMTGL                  PIC S9(04) COMP.       01719000
027100    15 'LAOSI01A-'INSTANZEN.                                      01720000
027200       16 'LAOSI01A-'INSTANZ-TAB OCCURS 15.                       01730000
027300          20 'LAOSI01A-'BAUTYPKZ           PIC S9(03) COMP.       01740000
027400          20 'LAOSI01A-'LFDNRBAUTYP        PIC S9(09) COMP.       01750000
027500          20 'LAOSI01A-'INSTANZBEZ         PIC  X(32).            01760000
027600    15 'LAOSI01A-'ABKABRECHNUNGKZ          PIC S9(04) COMP.       01770000
027700    15 'LAOSI01A-'VORLAGESBKZ              PIC S9(04) COMP.       01780000
027800    15 'LAOSI01A-'ANTMODELLVERZICHKZ       PIC S9(04) COMP.       01790000
027900    15 'LAOSI01A-'LISTMANNAHMEERKLKZ       PIC S9(04) COMP.       01800000
028000    15 'LAOSI01A-'ARTRISIKOPRKZ            PIC S9(04) COMP.       01810000
028100    15 'LAOSI01A-'LISTMABLAUFSCHRKZ        PIC S9(04) COMP.       01820000
028200    15 'LAOSI01A-'LISTMABLSCHRKAPKZ        PIC S9(04) COMP.       01830000
028300    15 'LAOSI01A-'VERSABLSCHRRENKZ         PIC S9(04) COMP.       01840000
028400    15 'LAOSI01A-'VERSABLSCHRKAPKZ         PIC S9(04) COMP.       01850000
028500    15 'LAOSI01A-'RECHTLWARTEZEITMON       PIC S9(4) USAGE COMP.  01860000
028600    15 'LAOSI01A-'FRISTRISKPRUEFDAT        PIC X(10).             01870000
028700    15 'LAOSI01A-'AUSCHLTARIFBWKZ          PIC S9(4) USAGE COMP.  01871000
028800    15 'LAOSI01A-'AUSCHLRENTSTEIGKZ        PIC S9(4) USAGE COMP.  01872000
028900    15 'LAOSI01A-'MAXPRAEMIENDYNKZ         PIC S9(4) USAGE COMP.  01873000
029000    15 'LAOSI01A-'MAXPRAEMIENDYNPROZ       PIC S9(3)V9(2) COMP-3. 01874000
029100    15 'LAOSI01A-'ARCHIVDRUCKSTKZ          PIC S9(4) USAGE COMP.  01874100
029200    15 'LAOSI01A-'ARTVORGUNISEX            PIC S9(4) USAGE COMP.  01874200
029300    15 'LAOSI01A-'RISIKOZUKOLLKZ           PIC S9(4) USAGE COMP.  01874300
029400*                                                                 01875000
029500*   DATENSTRUKTUR FÜR SPARTENKZ (Methode 9)                       01876000
029600 10  'LAOSI01A-'SPARTEN-DATA REDEFINES 'LAOSI01A-'DATA-BEREICH.   01877000
029700    15 'LAOSI01A-'ABKTYPKZ                 PIC S9(04) COMP.       01878000
029800    15 'LAOSI01A-'ABKUNTERTYPKZ            PIC S9(04) COMP.       01879000
029900    15 'LAOSI01A-'VERTRARTKZ               PIC S9(04) COMP.       01880000
030000    15 'LAOSI01A-'SPARTENKZ                PIC S9(04) COMP.       01890000
030100*                                                                 01900000
030200*   DATENSTRUKTUR FÜR PARTNERDATEN (Methode 10)                   01910000
030300 10  'LAOSI01A-'PARTNER-DATA REDEFINES 'LAOSI01A-'DATA-BEREICH.   01920000
030400    15 'LAOSI01A-'VOLLMACHTABPJN           PIC  X(01).            01930000
030500    15 'LAOSI01A-'ABKOMMENSBRANCHEKZ       PIC S9(04) COMP.       01940000
030600*                                                                 01950000
030700*   DATENSTRUKTUR FÜR SAMMELVERSICHERUNGSSCHEIN (Methode 11)      01960000
030800 10  'LAOSI01A-'SAMMEL-DATA REDEFINES 'LAOSI01A-'DATA-BEREICH.    01970000
030900    15 'LAOSI01A-'SAMMELSCHEINJN           PIC  X(01).            01980000
031000*                                                                 01990000
031100*   DATENSTRUKTUR FÜR KONSORTIALDATEN (Methode 12)                02000000
031200 10  'LAOSI01A-'KONSORTIAL-DATA REDEFINES 'LAOSI01A-'DATA-BEREICH.02010000
031300    15 'LAOSI01A-'KONSORTIAL-ANZ           PIC S9(4)   COMP.      02020000
031400    15 'LAOSI01A-'KONSORTIAL               OCCURS 20.             02030000
031500      16 'LAOSI01A-'VUBETEILIGUNGQUOTE     PIC S99V99  COMP-3.    02040000
031600      16 'LAOSI01A-'VUNR                   PIC S9(17)V COMP-3.    02050000
031700      16 'LAOSI01A-'KONSORTEID             PIC S9(4)   COMP.      02060000
031800      16 'LAOSI01A-'KONSORTEN-NAME.                               02070000
031900        20 'LAOSI01A-'KONSORTEN-NAME-1     PIC X(32).             02080000
032000        20 'LAOSI01A-'KONSORTEN-NAME-2     PIC X(32).             02090000
032100      16 'LAOSI01A-'KONSORTEN-ORT          PIC X(32).             02100000
032200* NEU ab 03/2006 - Kosten je VU                                   02110000
032300      16 'LAOSI01A-'FEDFUEHKSTREGPROZ      PIC S9(3)V9(2) COMP-3. 02120000
032400      16 'LAOSI01A-'FEDFUEHKSTREGBEZKZ     PIC S9(4)   COMP.      02130000
032500      16 'LAOSI01A-'FEDFUEHKSTRENPROZ      PIC S9(3)V9(2) COMP-3. 02140000
032600      16 'LAOSI01A-'FEDFUEHKSTRENBEZKZ     PIC S9(4)   COMP.      02150000
032700      16 'LAOSI01A-'FEDFUEHKSTBUPROZ       PIC S9(3)V9(2) COMP-3. 02160000
032800      16 'LAOSI01A-'FEDFUEHKSTBUBEZKZ      PIC S9(4)   COMP.      02170000
032900* NEU ab 04/2009 - VUKONTOKORRKONTONR je VU                       02180000
033000      16 'LAOSI01A-'VUKONTOKORRKONTONR     PIC S9(9)   COMP.      02190000
033100      16 'LAOSI01A-'VUABKNR                PIC  X(31).            02200000
033200    15 'LAOSI01A-'BILANZ-BINDENR           PIC S9(4)   COMP.      02201000
033300    15 'LAOSI01A-'TALANXKONSORTVARKZ       PIC S9(4)   COMP.      02202000
033400*                                                                 02203000
033500*                                                                 02204000
033600*   DATENSTRUKTUR FÜR RENTENDATEN (Methode 13)                    02205000
033700 10  'LAOSI01A-'RENTE-DATA REDEFINES 'LAOSI01A-'DATA-BEREICH.     02206000
033800    15 'LAOSI01A-'KAPABFINDUNGJN       PIC X.                     02207000
033900    15 'LAOSI01A-'RENTENZAHLWEISE      PIC S9(4) COMP.            02208000
034000    15 'LAOSI01A-'RENTENGARANTIEZEIT   PIC S9(4) COMP.            02209000
034100    15 'LAOSI01A-'RENTENGARANTIEZEITJN PIC X.                     02210000
034200    15 'LAOSI01A-'UEBERGANGRENTEBETR   PIC S9(9)V9(2) COMP-3.     02220000
034300    15 'LAOSI01A-'UEBERGANGRENTEPROZ   PIC S9(3)V9(2) COMP-3.     02230000
034400    15 'LAOSI01A-'ABFWIEDERHEIRATJN    PIC X.                     02240000
034500    15 'LAOSI01A-'WAISRENTEKZ          PIC S9(4) COMP.            02250000
034600    15 'LAOSI01A-'VOLLWAISRENTEBETR    PIC S9(9)V9(2) COMP-3.     02260000
034700    15 'LAOSI01A-'VOLLWAISRENTEPROZ    PIC S9(3)V9(2) COMP-3.     02270000
034800    15 'LAOSI01A-'HALBWAISRENTEBETR    PIC S9(9)V9(2) COMP-3.     02280000
034900    15 'LAOSI01A-'HALBWAISRENTEPROZ    PIC S9(3)V9(2) COMP-3.     02290000
035000    15 'LAOSI01A-'ENDALTERWAISENGELD   PIC S9(4) COMP.            02300000
035100    15 'LAOSI01A-'WAISGELDBEGRENZKZ    PIC S9(4) COMP.            02310000
035200    15 'LAOSI01A-'INDIVRENTEBEGINNJN   PIC X.                     02320000
035300    15 'LAOSI01A-'GEWFORMAUFSCHUBKZ    PIC S9(4) COMP.            02330000
035400    15 'LAOSI01A-'GEWFORMFAELLZEITKZ   PIC S9(4) COMP.            02340000
035500    15 'LAOSI01A-'HAUPTMINDESTBA       PIC S9(4) COMP.            02350000
035600    15 'LAOSI01A-'BUZKZ                PIC S9(4) COMP.            02360000
035700    15 'LAOSI01A-'GESETZLREINTRALTKZ   PIC S9(4) COMP.            02370000
035800    15 'LAOSI01A-'REN-PRODUKTGRUPPEKZ  PIC S9(4) COMP.            02380000
035900* REN-PRODUKTGRUPPEKZ: z. Zt. immer 1                             02390000
036000*                      (s. Abk-Key: versTechnDatenTypKZ)          02400000
036100    15 'LAOSI01A-'REN-VERTRIEBSWEGKZ   PIC S9(4) COMP.            02410000
036200    15 'LAOSI01A-'REN-VERTRIEBSMODKUR  PIC X(12).                 02420000
036300    15 'LAOSI01A-'REN-PRODUKTKEY       PIC X(12).                 02430000
036400    15 'LAOSI01A-'REN-VEREINBARTBEGMON PIC S9(4) USAGE COMP.      02431000
036500*                                                                 02432000
036600*                                                                 02433000
036700*   DATENSTRUKTUR FÜR BUZDATEN (Methode 14)                       02434000
036800 10  'LAOSI01A-'BUZ-EUZ-DATA REDEFINES 'LAOSI01A-'DATA-BEREICH.   02435000
036900    15 'LAOSI01A-'BUZTARIFKZ2          PIC S9(4) COMP.            02436000
037000    15 'LAOSI01A-'BUZTARIFKZ1          PIC S9(4) COMP.            02437000
037100    15 'LAOSI01A-'RISIKOGRUPPEKZ       PIC S9(4) COMP.            02438000
037200    15 'LAOSI01A-'BUZKARENZZEIT        PIC S9(4) COMP.            02439000
037300    15 'LAOSI01A-'MINDESTBUZGRADPROZ   PIC S9(3)V9(2) COMP-3.     02440000
037400    15 'LAOSI01A-'BUZGEWINNBETEILBKZ   PIC S9(4) COMP.            02450000
037500    15 'LAOSI01A-'BUZGEWINNBETEILRKZ   PIC S9(4) COMP.            02460000
037600    15 'LAOSI01A-'BUZDYNAMIKPROZ       PIC S9(3)V9(2) COMP-3.     02470000
037700    15 'LAOSI01A-'PRAEMWEITERVERWJN    PIC X.                     02480000
037800    15 'LAOSI01A-'BUZMINDESTBA         PIC S9(4) COMP.            02490000
037900    15 'LAOSI01A-'BUZHOECHSTBA         PIC S9(4) COMP.            02500000
038000    15 'LAOSI01A-'BUZVERTRDAUERB       PIC S9(4) COMP.            02510000
038100    15 'LAOSI01A-'BUZVERTREAB          PIC S9(4) COMP.            02520000
038200    15 'LAOSI01A-'BUZLEISTDAUER        PIC S9(4) COMP.            02530000
038300    15 'LAOSI01A-'BUZLEISTEAB          PIC S9(4) COMP.            02540000
038400    15 'LAOSI01A-'BUZGESRENTEBETR      PIC S9(9)V9(2) COMP-3.     02550000
038500    15 'LAOSI01A-'BUZGESPROZ           PIC S9(3)V9(2) COMP-3.     02560000
038600    15 'LAOSI01A-'BUZGESRENTEBETR2     PIC S9(9)V9(2) COMP-3.     02570000
038700    15 'LAOSI01A-'BUZGESPROZ2          PIC S9(3)V9(2) COMP-3.     02580000
038800    15 'LAOSI01A-'BUZGESRENTEBETR3     PIC S9(9)V9(2) COMP-3.     02590000
038900    15 'LAOSI01A-'BUZGESPROZ3          PIC S9(3)V9(2) COMP-3.     02600000
039000    15 'LAOSI01A-'BUZVERTRDAUERR1      PIC S9(4) COMP.            02610000
039100    15 'LAOSI01A-'BUZVERTREAR          PIC S9(4) COMP.            02620000
039200    15 'LAOSI01A-'BUZVERTRDAUERR2      PIC S9(4) COMP.            02630000
039300    15 'LAOSI01A-'BUZVERTREAR2         PIC S9(4) COMP.            02640000
039400    15 'LAOSI01A-'BUZVERTRDAUERR3      PIC S9(4) COMP.            02650000
039500    15 'LAOSI01A-'BUZVERTREAR3         PIC S9(4) COMP.            02660000
039600    15 'LAOSI01A-'BUZLEISTEABW         PIC S9(4) COMP.            02670000
039700    15 'LAOSI01A-'BUZLEISTEAR          PIC S9(4) COMP.            02680000
039800    15 'LAOSI01A-'BUZVERTREABW         PIC S9(4) COMP.            02690000
039900    15 'LAOSI01A-'ANTEILRISIKOGRUPPE   PIC S9(9)V9(2) COMP-3.     02700000
040000*                                                                 02710000
040100*   DATENSTRUKTUR FÜR GEWINNBENACHRICHTIGUNG (Methode 15)         02720000
040200*                                                                 02730000
040300 10  'LAOSI01A-'GEWINN-BN REDEFINES 'LAOSI01A-'DATA-BEREICH.      02740000
040400    15 'LAOSI01A-'LABELKZ               PIC S9(04) COMP.          02750000
040500    15 'LAOSI01A-'VORRANGLABELKZ        PIC S9(04) COMP.          02760000
040600    15 'LAOSI01A-'GEWBENACHRARTKZ       PIC S9(04) COMP.          02770000
040700    15 'LAOSI01A-'GEWBENACHRWEGKZ       PIC S9(04) COMP.          02780000
040800    15 'LAOSI01A-'GEWBENACHREMPFKZ      PIC S9(04) COMP.          02790000
040900    15 'LAOSI01A-'GEWBENACHRMONATKZ     PIC S9(04) COMP.          02800000
041000    15 'LAOSI01A-'GEWBENACHRABKSONDERKZ PIC S9(04) COMP.          02810000
041100    15 'LAOSI01A-'DRUCKENKZ             PIC S9(04) COMP.          02820000
041200*                                                                 02830000
041300*   DATENSTRUKTUR FÜR DRUCKOPTIONEN (Methode 16)                  02840000
041400*                                                                 02850000
041500 10  'LAOSI01A-'DRUCKOPT-DATA REDEFINES 'LAOSI01A-'DATA-BEREICH.  02860000
041600     15 'LAOSI01A-'BAUDRUCK.                                      02870000
041700*    Daten aus Tabelle LA_ABKBAUDRUCK                             02880000
041800        20 'LAOSI01A-'DRUCKOPTBEZKURZ    PIC  X(32).              02890000
041900        20 'LAOSI01A-'DRUCKOPTSPRACHEKZ  PIC  X(04).              02900000
042000        20 'LAOSI01A-'DRUCKOPTBEZLANG    PIC  X(120).             02910000
042100*----------------------------------------------------------------*02920000
042200* 10 MöGLICHE BRIEFE                                             *02930000
042300*----------------------------------------------------------------*02940000
042400     15 'LAOSI01A-'DRUCKBRIEF.                                    02950000
042500        17 'LAOSI01A-'BRIEF-TAB.                                  02960000
042600          20 'LAOSI01A-'BRIEF-ANZ     PIC S9(04) COMP.            02970000
042700          20 'LAOSI01A-'BRIEF OCCURS 10.                          02980000
042800*----------------------------------------------------------------*02990000
042900* DRUCKBRIEF                                                     *03000000
043000*----------------------------------------------------------------*03010000
043100*    Daten aus Tabelle LA_ABKDRUCKBRIEF                           03020000
043200             25 'LAOSI01A-'BRIEFTYPKURZKZ PIC S9(04) COMP.        03030000
043300             25 'LAOSI01A-'KOPIENANZAHL   PIC S9(04) COMP.        03040000
043400             25 'LAOSI01A-'VERSWEGORIGIKZ PIC S9(04) COMP.        03050000
043500             25 'LAOSI01A-'EMPFORIGINAL   PIC  X(10).             03060000
043600             25 'LAOSI01A-'ZWEMPFORIGINAL PIC  X(10).             03070000
043700*----------------------------------------------------------------*03080000
043800* KOPIEN                                                         *03090000
043900*----------------------------------------------------------------*03100000
044000             25 'LAOSI01A-'KOPIE-TAB.                             03110000
044100                30 'LAOSI01A-'KOPIE-ANZ     PIC S9(04) COMP.      03120000
044200                30 'LAOSI01A-'KOPIE OCCURS 6.                     03130000
044300*    Daten aus Tabelle LA_ABKDRUCKKOPIE                           03140000
044400                   35 'LAOSI01A-'KOPIENR       PIC S9(10)V COMP-3.03150000
044500                   35 'LAOSI01A-'EMPFKOPIE     PIC  X(10).        03160000
044600                   35 'LAOSI01A-'VERSWEGKOPKZ  PIC S9(04) COMP.   03170000
044700                   35 'LAOSI01A-'ZWEMPFKOPIE   PIC  X(10).        03180000
044800*----------------------------------------------------------------*03190000
044900* SCHRIFTSTÜCKE                                                  *03200000
045000*----------------------------------------------------------------*03210000
045100             25 'LAOSI01A-'SCHRIFTST-TAB.                         03220000
045200                30 'LAOSI01A-'SCHRIFTST-ANZ PIC S9(04) COMP.      03230000
045300                30 'LAOSI01A-'SCHRIFTST OCCURS 20.                03240000
045400*    Daten aus Tabelle LA_ABKDRUCKSCHRSTK                         03250000
045500                   35 'LAOSI01A-'SCHRIFTSTKURZKZ  PIC S9(04) COMP.03260000
045600*----------------------------------------------------------------*03270000
045700* SONDER                                                         *03280000
045800*----------------------------------------------------------------*03290000
045900             25 'LAOSI01A-'SONDER.                                03300000
046000*    Daten aus Tabelle LA_ABKDRUCKSONDER                          03310000
046100                30 'LAOSI01A-'GEWBENACHRARTKZ   PIC S9(04) COMP.  03320000
046200                30 'LAOSI01A-'GEWBENACHRMONATKZ PIC S9(04) COMP.  03330000
046300*                                                                 03340000
046400*                                                                 03350000
046500*   DATENSTRUKTUR FÜR VT - K A P I T A L - DATEN (Methode 17)     03360000
046600*                                                                 03370000
046700 10  'LAOSI01A-'KAPITAL-DATA REDEFINES 'LAOSI01A-'DATA-BEREICH.   03371000
046800    15 'LAOSI01A-'KAP-PRODUKTGRUPPEKZ  PIC S9(4) COMP.            03372000
046900* KAP-PRODUKTGRUPPEKZ: z. Zt. immer 2                             03373000
047000*                      (s. Abk-Key: versTechnDatenTypKZ)          03374000
047100    15 'LAOSI01A-'KAP-PRODAUSWAHLDAT   PIC X(10).                 03375000
047200    15 'LAOSI01A-'KAP-PRODUKTKEY       PIC X(12).                 03376000
047300    15 'LAOSI01A-'KAP-VERTRIEBSWEGKZ   PIC S9(4) COMP.            03377000
047400    15 'LAOSI01A-'KAP-VERTRIEBSMODKUR  PIC X(12).                 03378000
047500    15 'LAOSI01A-'KAPITALVSEA          PIC S9(4) USAGE COMP.      03379000
047600    15 'LAOSI01A-'KAPITALGEWFORMKZ     PIC S9(4) USAGE COMP.      03380000
047700    15 'LAOSI01A-'KAP-BUZKZ            PIC S9(4) COMP.            03381000
047800    15 'LAOSI01A-'KAP-VEREINBARTBEGMON PIC S9(4) USAGE COMP.      03382000
047900*                                                                 03383000
048000*                                                                 03384000
048100*   Methode 18:                                                   03385000
048200*   DATENSTRUKTUR FÜR Standardabkommensauskunft mit Pointer       03386000
048300*                                                                 03387000
048400 10  'LAOSI01A-'ABKSTDAUSK-DATA REDEFINES 'LAOSI01A-'DATA-BEREICH.03388000
048500*                                                                 03389000
048600    15 'LAOSI01A-'STDAUSKBUENBSABKKZ   PIC S9(4)  COMP.           03390000
048700    15 'LAOSI01A-'STDAUSKBUENDABKNR    PIC S9(17) COMP-3.         03391000
048800    15 'LAOSI01A-'STDAUSKBUENDLFDNR    PIC S9(09) COMP.           03391100
048900*                                                                 03391200
049000    15 'LAOSI01A-'STDAUSK-ANZ          PIC S9(4)  COMP.           03391300
049100    15 'LAOSI01A-'STDAUSK-ANCHOR   USAGE IS POINTER.              03391400
049200    15 'LAOSI01A-'NEXT-STDAUSK-PTR USAGE IS POINTER.              03391500
049300*                                                                 03391600
049400    15 'LAOSI01A-'STDAUSK-BSABKKZ      PIC S9(4)  COMP.           03391700
049500    15 'LAOSI01A-'STDAUSK-ABKNR        PIC S9(17) COMP-3.         03391800
049600    15 'LAOSI01A-'STDAUSK-LFDNR        PIC S9(09) COMP.           03391900
049700*                                                                 03392000
049800*                                                                 03393000
049900*   Methode 19:                                                   03394000
050000*   DATENSTRUKTUR FÜR VT - F L V - DATEN (Methode 19)             03395000
050100*                                                                 03396000
050200 10  'LAOSI01A-'FLV-DATA REDEFINES 'LAOSI01A-'DATA-BEREICH.       03397000
050300    15 'LAOSI01A-'FLV-PRODUKTGRUPPEKZ  PIC S9(4) COMP.            03398000
050400* FLV-PRODUKTGRUPPEKZ: z. Zt. immer 3                             03399000
050500*                      (s. Abk-Key: versTechnDatenTypKZ)          03400000
050600    15 'LAOSI01A-'FLV-PRODAUSWAHLDAT   PIC X(10).                 03410000
050700    15 'LAOSI01A-'FLV-PRODUKTKEY       PIC X(12).                 03420000
050800    15 'LAOSI01A-'FLV-VERTRIEBSWEGKZ   PIC S9(4) COMP.            03430000
050900    15 'LAOSI01A-'FLV-VERTRIEBSMODKUR  PIC X(12).                 03440000
051000    15 'LAOSI01A-'FLV-VSEA             PIC S9(4) COMP.            03450000
051100    15 'LAOSI01A-'FLV-GEWFORMKZ        PIC S9(4) COMP.            03460000
051200    15 'LAOSI01A-'FLV-RENTGARANTZEITKZ PIC S9(4) COMP.            03470000
051300    15 'LAOSI01A-'FLV-RENTGARANTIEZEIT PIC S9(4) COMP.            03480000
051400    15 'LAOSI01A-'FLV-RGZBISENDALTER   PIC S9(4) COMP.            03490000
051500    15 'LAOSI01A-'FLV-HOECHSTBA        PIC S9(4) COMP.            03500000
051600    15 'LAOSI01A-'FLV-MAXENDALTER      PIC S9(4) COMP.            03510000
051700    15 'LAOSI01A-'FLV-VEREINBARTBEGMON PIC S9(4) COMP.            03520000
051800    15 'LAOSI01A-'FLV-BUZKZ            PIC S9(4) COMP.            03530000
051900*                                                                 03540000
052000*                                                                 03550000
052100*   Methode 20:                                                   03560000
052200*   DATENSTRUKTUR FÜR BU-Pruefung-DATEN (Methode 20)              03570000
052300 10  'LAOSI01A-'BUPRUEFUNG-DATA REDEFINES 'LAOSI01A-'DATA-BEREICH.03580000
052400    15 'LAOSI01A-'MAXBUPRAEMIEKZ       PIC S9(4) COMP.            03590000
052500    15 'LAOSI01A-'MAXBUPRAEMIE         PIC S9(9)V9(2)  COMP-3.    03600000
052600    15 'LAOSI01A-'MAXBUPRAEMIEBBGKZ    PIC S9(4) COMP.            03610000
052700    15 'LAOSI01A-'MAXBUPRINDGEHKZ      PIC S9(4) COMP.            03620000
052800    15 'LAOSI01A-'MAXBUPRINDGEHPROZ    PIC S9(3)V9(2)  COMP-3.    03630000
052900    15 'LAOSI01A-'BUMAXENDALTER        PIC S9(4) COMP.            03640000
053000    15 'LAOSI01A-'BUHOECHSTBA          PIC S9(4) COMP.            03650000
053100    15 'LAOSI01A-'BURISKGRUPPEA1BETR   PIC S9(9)V9(2)  COMP-3.    03660000
053200    15 'LAOSI01A-'BURISKGRUPPEA1KZ     PIC S9(4) COMP.            03670000
053300    15 'LAOSI01A-'BURISKGRUPPEABETR    PIC S9(9)V9(2)  COMP-3.    03680000
053400    15 'LAOSI01A-'BURISKGRUPPEAKZ      PIC S9(4) COMP.            03690000
053500    15 'LAOSI01A-'BURISKGRUPPEB1BETR   PIC S9(9)V9(2)  COMP-3.    03700000
053600    15 'LAOSI01A-'BURISKGRUPPEB1KZ     PIC S9(4) COMP.            03710000
053700    15 'LAOSI01A-'BURISKGRUPPEBBETR    PIC S9(9)V9(2)  COMP-3.    03720000
053800    15 'LAOSI01A-'BURISKGRUPPEBKZ      PIC S9(4) COMP.            03730000
053900    15 'LAOSI01A-'BURISKGRUPPEC1BETR   PIC S9(9)V9(2)  COMP-3.    03740000
054000    15 'LAOSI01A-'BURISKGRUPPEC1KZ     PIC S9(4) COMP.            03750000
054100    15 'LAOSI01A-'BURISKGRUPPECBETR    PIC S9(9)V9(2)  COMP-3.    03760000
054200    15 'LAOSI01A-'BURISKGRUPPECKZ      PIC S9(4) COMP.            03770000
054300    15 'LAOSI01A-'BURISKGRUPPEDBETR    PIC S9(9)V9(2)  COMP-3.    03780000
054400    15 'LAOSI01A-'BURISKGRUPPEDKZ      PIC S9(4) COMP.            03790000
