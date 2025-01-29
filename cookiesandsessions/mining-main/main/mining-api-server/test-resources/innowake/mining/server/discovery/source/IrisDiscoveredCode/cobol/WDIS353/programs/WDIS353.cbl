000590 IDENTIFICATION DIVISION.
000680 PROGRAM-ID. WDIS353.
000810 ENVIRONMENT DIVISION.
000860 
000990 DATA DIVISION.
001030 
001040 
001050 WORKING-STORAGE SECTION.
001470 01 WS_RMCRULES_DATA.
001480    05  WS_SYSTEM                        PIC X(3)  VALUE "SYS".
001490    05  WS_APPLICATION                   PIC X(7)  VALUE "SYS".
001500    05  WS_FLAG_YES                      PIC X(1)  VALUE "Y".
001510    05  WS_SP                            PIC X(1)  VALUE "".
001520    05  WS_TABLE_NAME_RMC2                  PIC X(20) VALUE "RMC2"
IWCONT .
005310 01 WS_CLMUNIT_EXTERNAL_FILES    IS EXTERNAL.
005320     05  CLPMGTMP_FL                     PIC S9(09) COMP.
005330     05  PRV_MAST_FL                     PIC S9(09) COMP.
005340     05  PRV_PHYREL_FL                   PIC S9(09) COMP.
005350     05  MBR_GRP_FLG_FL                  PIC S9(09) COMP.
005360 
005370 
005380********************************************************************************
005390*-------------------------------------------------------------------------------
005400*   Report Section
005410*-------------------------------------------------------------------------------
005420********************************************************************************
005430 REPORT SECTION.
005440 RD DLY_CLMINV_AA_REPORT
005450    CONTROLS ARE FINAL WS_RPT_TITLE
005460    PAGE LIMIT IS 66 LINES
005470    HEADING       1
005480    FIRST DETAIL  7
005490    LAST DETAIL   58
005500    FOOTING       60.
005510 
005520*01  CONTROL-HEADER TYPE IS CONTROL HEADING WS_RPT_TITLE
005530 01  CONTROL-FOOTER TYPE IS CONTROL FOOTING WS_RPT_TITLE
005540     NEXT GROUP NEXT PAGE.
005550     05  LINE PLUS 1.
005560         10  COLUMN  1                   PIC  X(01)  VALUE " ".
005570 
005580 01  TYPE IS PAGE HEADING.
005590     05  LINE 1.
005600         10  COLUMN 1                    PIC X(18)
005610                                         VALUE "CLM_RPT:CLMINV_AA_
IWCONT-"".
005620         10  COLUMN 19                   PIC X(08)
005630                                         SOURCE WS_START_DATE.
005640         10  COLUMN 27                   PIC X(04)
005650                                         VALUE ".RPT".
005660         10  COLUMN 60                   PIC X(10)
005670                                         SOURCE WS_RPT_TITLE.
005680         10  COLUMN 113                  PIC X(11)
005690                                         SOURCE WS_RPT_DATE.
005700         10  COLUMN 125                  PIC X(02)
005710                                         SOURCE WS_CURRENT_HH.
005720         10  COLUMN 127                  PIC X(01)
005730                                         VALUE ":".
005740         10  COLUMN 128                  PIC X(02)
005750                                         SOURCE WS_CURRENT_MM.
005760 
005770     05  LINE 2.
005780         10  COLUMN 42                   PIC X(48)
005790                 VALUE  "Claims Inventory Received and Adjudicated
IWCONT-" Report".
005800 
005810     05  LINE 3.
005820         10  COLUMN 48                   PIC X(8)
005830                                         VALUE "Period: ".
005840         10  COLUMN 56                   PIC X(11)
005850                                         SOURCE WS_RPT_START_DATE.
005860         10  COLUMN 68                   PIC X(03)
005870                                         VALUE "to ".
005880         10  COLUMN 71                   PIC X(11)
005890                                         SOURCE WS_RPT_END_DATE.
005900         10  COLUMN 120                  PIC X(5)
005910                                         VALUE "Page:".
005920         10  COLUMN 126                  PIC ZZZ9
005930                                         SOURCE PAGE-COUNTER.
005940     05  LINE 4.
005950         10  COLUMN 1                    PIC X(130)
005960                                         VALUE ALL "-".
005970     05  LINE 5.
005980         10  COLUMN 39                   PIC X(03)
005990                                         VALUE "ACS".
006000         10  COLUMN 59                   PIC X(03)
006010                                         VALUE "EDI".
006020     05  LINE 6.
006030         10  COLUMN 33                   PIC X(04)
006040                                         VALUE "PROF".
006050         10  COLUMN 43                   PIC X(04)
006060                                         VALUE "HOSP".
006070         10  COLUMN 53                   PIC X(04)
006080                                         VALUE "PROF".
006090         10  COLUMN 63                   PIC X(04)
006100                                         VALUE "HOSP".
006110         10  COLUMN 74                   PIC X(05)
006120                                         VALUE "TOTAL".
006130         10  COLUMN 87                   PIC X(07)
006140                                         VALUE "PERCENT".
006150 
006160 01  DLT_LINE_1 TYPE DETAIL.
006170     05  LINE PLUS 1.
006180         10  COLUMN 1                    PIC X(24)
006190                                         SOURCE
IWCONT CINV_AA_DESCRIPTS(SUB2).
006200         10  COLUMN 30                   PIC ZZZ,ZZZ
006210                                         SOURCE
IWCONT CINV_AA_TOTS(SUB1,SUB2,1).
006220         10  COLUMN 40                   PIC ZZZ,ZZZ
006230                                         SOURCE
IWCONT CINV_AA_TOTS(SUB1,SUB2,2).
006240         10  COLUMN 50                   PIC ZZZ,ZZZ
006250                                         SOURCE
IWCONT CINV_AA_TOTS(SUB1,SUB2,3).
006260         10  COLUMN 60                   PIC ZZZ,ZZZ
006270                                         SOURCE
IWCONT CINV_AA_TOTS(SUB1,SUB2,4).
006280         10  COLUMN 70                   PIC Z,ZZZ,ZZZ
006290                                         SOURCE
IWCONT CINV_AA_TOTS(SUB1,SUB2,5).
006300         10  COLUMN 87                   PIC ZZZ.ZZ
006310                                         SOURCE
IWCONT CINV_AA_PCT(SUB1,SUB2).
006320         10  COLUMN 93                   PIC X(01)
006330                                         VALUE "%".
006340 
006350 01  DLT_LINE_2 TYPE DETAIL.
006360     05  LINE PLUS 1.
006370         10  COLUMN 1                    PIC X(56)
006380             VALUE "Auto Adjudicated Claims plus 75% of Suffix Pen
IWCONT-"ded Claims".
006390         10  COLUMN 87                   PIC ZZZ.ZZ
006400                                         SOURCE
IWCONT CINV_ADJ_PCT(SUB1).
006410         10  COLUMN 93                   PIC X(01)
006420                                         VALUE "%".
006430 01  BLANK_LINE TYPE DETAIL.
006440     05  LINE PLUS 1.
006450         10  COLUMN 1                    PIC X(130)
006460                                         VALUE SPACES.
006470 
006480 
006490 
006500*******************************************************************************
006510*                                                                             *
006520*        P R O C E D U R E    D I V I S I O N                                 *
006530*                                                                             *
006540*******************************************************************************
006550 
006560 PROCEDURE DIVISION.
006570 
006580 MAIN SECTION.
006850 A1000_INITIALIZATION.
006860 
006870     DISPLAY "START PROCESSING ...".

020640 Z9300_EXIT.
020650     EXIT.
020660 END PROGRAM WDIS353.
