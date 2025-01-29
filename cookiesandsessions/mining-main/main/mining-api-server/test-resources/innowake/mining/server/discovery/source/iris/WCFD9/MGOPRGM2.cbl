       IDENTIFICATION DIVISION.
       PROGRAM-ID. MGOPRGM1.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  REC.
      * COPY_START MGOCOPY1 7 0 7
      ** Copybook name: MGOCOPY1
        02  FIRST-NAME              PIC X(10).
        02  LAST-NAME              PIC X(20).



      * COPY_END MGOCOPY1

      * COPY_START MGOCOPY3 8 0 37
00001 *                                                               
00002  01  MAP-INPUT-AREA.
00003 *
00004      05  FILLER                          PIC X(12).
00005 *
00014      05  MAP-DAT-STR-IN.
00015          10  MAP-DAT-LEN                 PIC S9(04) COMP.
00016          10  MAP-DAT-FLD                 PIC X.
00017          10  FILLER
00018              REDEFINES MAP-DAT-FLD.
00019              15 MAP-DAT-ATR              PIC X.
00020          10  MAP-DAT-I                   PIC X(17).
00021 *
00022      05  MAP-USR-ID-STR-IN.
00023          10  MAP-USR-ID-LEN              PIC S9(04) COMP.
00024          10  MAP-USR-ID-FLD              PIC X.
00025          10  FILLER
00026              REDEFINES MAP-USR-ID-FLD.
00027              15 MAP-USR-ID-ATR           PIC X.
00028          10  MAP-USR-ID-I                PIC X(12).
00029 *
00140  01  MAP-OUTPUT-AREA
00141      REDEFINES MAP-INPUT-AREA.
00142      10 FILLER                           PIC X(12).
00143 *
00148      05  MAP-DAT-STR-OUT.
00149          10  FILLER                      PIC X(03).
00150          10  MAP-DAT-O                   PIC X(17).
00151 *
00152      05  MAP-USR-ID-STR-OUT.
00153          10  FILLER                      PIC X(03).
00154          10  MAP-USR-ID-O                PIC X(12).
00155 *



      * COPY_END MGOCOPY3


       PROCEDURE DIVISION.
          ACCEPT FIRST-NAME.
         
      * COPY_START MGOCOPY2 12 0 12
      ** Copybook name: MGOCOPY2
        ACCEPT LAST-NAME.
        IF LAST-NAME = 'K'
           DISPLAY 'K'.
        IF LAST-NAME = 'U'
           DISPLAY 'U'.
        IF LAST-NAME = 'H'
           DISPLAY 'H'
        ELSE
           DISPLAY 'D'.

      * COPY_END MGOCOPY2

          DISPLAY 'DETAILS : ' REC.
          STOP RUN.
