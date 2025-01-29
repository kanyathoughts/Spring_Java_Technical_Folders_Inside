000520 IDENTIFICATION DIVISION.
000530
000540****************************************************************
000550****************************************************************
000560 PROGRAM-ID.    CLM_BNK_HOL_01.
000570
000580
000590****************************************************************
000600 ENVIRONMENT DIVISION.
000610****************************************************************
000620 INPUT-OUTPUT SECTION.
000630 FILE-CONTROL.
000640
000650 SELECT TABLES_MASTER_FILE
000660        ASSIGN TO TBLMSTR_FILE_NAME
000670        RESERVE 32 AREAS
000680        ORGANIZATION IS INDEXED
000690        ACCESS MODE IS DYNAMIC
000700        RECORD KEY IS TBLMSTR_KEY0
000710        FILE STATUS IS TBLMSTR_FILE_STATUS.
000720
000730 I-O-CONTROL.
000740     APPLY LOCK-HOLDING ON
000750                        TABLES_MASTER_FILE.
000760
000770
000780****************************************************************
000790 DATA DIVISION.
000800****************************************************************
000810 FILE SECTION.
000820****************************************************************
000830
000840 FD  TABLES_MASTER_FILE
000850      VALUE OF FILE-ID IS TBLMSTR_FILE_NAME.
000860 COPY "CDD_REC.TBLMSTR_MASTER" FROM DICTIONARY
000870        REPLACING ==TBLMSTR_MASTER== BY ==TBLMSTR_RECORD==.
000880 66  TBLMSTR_TERSE RENAMES TBLMSTR_TABLE_DATA.
000890
000900
000910****************************************************************
000920 WORKING-STORAGE SECTION.
000930****************************************************************
001110 01  DATE_YEAR_LEAP_MTH_DAYS.
001120     07  PIC X(39) VALUE
001130                        "000031059090120151181212243273304334365".
001140     07  PIC X(39) VALUE
001150                        "000031060091121152182213244274305335366".
001160     07  PIC X(24) VALUE "312831303130313130313031".
001580 01  TBLMSTR_FILE_CONTROLS.
001590     03  TBLMSTR_FILE_NAME         PIC X(64) VALUE
001600                                                 "TABLES_MASTER:".
001610     03  TBLMSTR_RECORD_COUNT      PIC 9(6)  VALUE ZEROES.
001620     03  TBLMSTR_FILE_STATUS       PIC X(2)  VALUE "00".
001630         88  TBLMSTR_RMS_SUCCESS             VALUE "00" "02" "90".
001640         88  TBLMSTR_RMS_END_FILE            VALUE "10" "46".
001650         88  TBLMSTR_RMS_NORECORD            VALUE "23".
001660
001960****************************************************************
001970 PROCEDURE DIVISION.
