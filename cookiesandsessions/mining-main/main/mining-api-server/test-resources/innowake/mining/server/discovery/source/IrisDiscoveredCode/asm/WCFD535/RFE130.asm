         MACRO                                                          00001000
&NAME    RFE130 &ENTRY                                                  00002000
         GBLA  &LPREV,&PD,&PX1                                          00003000
         LCLA  &X1,&R1,&R2,&R3,&D,&L,&KVAL,&LWORK                       00004000
         LCLA  &CURL                                                    00005000
         LCLC  &DATA1,&DATA2,&DATA3                                     00006000
         LCLC  &CORP                                                    00007000
*        RFE130 MACRO    *RCIF*                                   2.0   00008000
         TITLE 'CRF PREFIX TABLE'                                       00009000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00025000
*                                                                     * 00026000
*                       P R E F I X  T A B L E                        * 00027000
*                                                                     * 00028000
*                      * * I M P O R T A N T * *                      * 00029000
*                                                                     * 00030000
*               ANY CHANGE TO THIS TABLE MAY CHANGE THE KEYS          * 00031000
*               OF CRF RECORDS CLASSIFIED AS INDIVIDUALS.             * 00032000
*                                                                     * 00033000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00034000
         SPACE                                                          00035000
PREFIXES DS    0CL1 -             START OF PREFIX TABLE                 00036000
&LPREV   SETA  0             RESET                                      00037000
&PD      SETA  0                   GLOBALS                              00038000
&PX1     SETA  0                                                        00039000
&X1      SETA  &X1+1                                                    00040000
         AGO   .PRE1                                                    00041000
.P1      ANOP                                                           00042000
&X1      SETA  &X1+1                                                    00043000
         AIF   (&X1 LT N'&SYSLIST OR &X1 EQ N'&SYSLIST).PRE1            00044000
         DC    X'FF' -            END OF PREFIX TABLE                   00045000
         MEXIT                                                          00046000
.PRE1    ANOP                                                           00047000
         AIF   (K'&SYSLIST(&X1) GT 24).INV                              00048000
.*                                                                      00049000
.*  INITIALIZE  AND  CHECK  FOR LEADING PARENTHESIS                     00050000
&R1      SETA  1                                                        00051000
&D       SETA  1             SET INITIAL DISPLACEMENT VALUE             00052000
&L       SETA  K'&SYSLIST(&X1)    INITIAL LENGTH                        00053000
         AIF   ('&SYSLIST(&X1)'(1,1) NE '(').APOST                      00054000
&L       SETA  &L-2           DECREMENT LENGTH                          00055000
&D       SETA  &D+1           INCREMENT DISPLACEMENT                    00056000
.*                            *                                         00057000
.*  CHECK FOR LEADING QUOTE   *                                         00058000
.*                            *                                         00059000
.APOST   ANOP                                                           00060000
         AIF   ('&SYSLIST(&X1)'(&D,1) NE '''').LCHECK                   00061000
&L       SETA  &L-2           DECREMENT LENGTH                          00062000
&D       SETA  &D+1           INCREMENT DISPLACEMENT                    00063000
.LCHECK  ANOP                                                           00064000
&LWORK   SETA  &L                                                       00065000
         AIF   (&LWORK GT 20).INV                                       00066000
         AIF   (&LPREV EQ 0).DISP      BYPASS SEQUENCE CHECK            00067000
         AIF   (&L GT &LPREV).LPR                                       00068000
.*  USE LENGTH OF CURRENT ENTRY                                         00069000
&CORP    SETC  'C'           INDICATE USING CURRENT LENGTH              00070000
         AIF   (&L LE 8).L1                                             00071000
&R2      SETA  8                                                        00072000
         AGO   .DISP                                                    00073000
.*  USE LENGTH OF PREVIOUS ENTRY                                        00074000
.LPR     ANOP                                                           00075000
&CORP    SETC  'P'                                                      00076000
&LWORK   SETA  &LPREV                                                   00077000
         AIF   (&LPREV LE 8).LPR1 Q. L'PREVIOUS ENTRY LE 8              00078000
&R2      SETA  8                  A. NO, SET MAXIMUM LENGTH             00079000
         AGO   .DISP                                                    00080000
.L1      ANOP                 SET LENGTH OF 8 OR LESS (CURRENT LENGTH)  00081000
&R2      SETA  &L                                                       00082000
         AGO   .DISP                                                    00083000
.LPR1    ANOP                 SET LENGTH OF 8 OR LESS (PREVIOUS ENTRY)  00084000
&R2      SETA  &LPREV                                                   00085000
.DISP    ANOP                 SET INITIAL                               00086000
&R3      SETA  &PD             DISPLACEMENTS                            00087000
&R1      SETA  &D               FOR COMPARE                             00088000
.*                                                                      00089000
.*   RESET DATA FIELDS                                                  00090000
.*                                                                      00091000
&DATA1   SETC  ''                                                       00092000
&DATA2   SETC  ''                                                       00093000
&DATA3   SETC  ''                                                       00094000
.*                                                                      00095000
.*             SET DATA FOR GENERATION                                  00096000
&CURL    SETA  &L                                                       00097000
.*                                                                      00098000
         AIF   (&CURL LT 9).DAT1                                        00099000
.*                                                                      00100000
.*   CHARACTER STRING GREATER THAN 8                                    00101000
.*                                                                      00102000
&DATA1   SETC  '&SYSLIST(&X1)'(&R1,8)                                   00103000
&R1      SETA  &R1+8                                                    00104000
&CURL    SETA  &CURL-8                                                  00105000
         AIF   (&CURL LT 9).DAT2                                        00106000
&DATA2   SETC  '&SYSLIST(&X1)'(&R1,8)                                   00107000
&R1      SETA  &R1+8                                                    00108000
&CURL    SETA  &CURL-8                                                  00109000
         AIF   (&CURL LT 9).DAT3                                        00110000
&DATA3   SETC  '&SYSLIST(&X1)'(&R1,8)                                   00111000
         AGO   .DATAX                                                   00112000
.DAT1    ANOP                                                           00113000
&DATA1   SETC  '&SYSLIST(&X1)'(&R1,&CURL)                               00114000
         AGO   .DATAX                                                   00115000
.DAT2    ANOP                                                           00116000
&DATA2   SETC  '&SYSLIST(&X1)'(&R1,&CURL)                               00117000
         AGO   .DATAX                                                   00118000
.DAT3    ANOP                                                           00119000
&DATA3   SETC  '&SYSLIST(&X1)'(&R1,&CURL)                               00120000
.*                                                                      00121000
.DATAX   ANOP                                                           00122000
&R1      SETA  &D                                                       00123000
         AIF   (&LPREV EQ 0).GEN                                        00124000
.*  COMPARE CURRENT ENTRY TO LAST (IN SEQUENCE) ENTRY                   00125000
.COMP    ANOP                                                           00126000
         AIF   ('&SYSLIST(&X1)'(&R1,&R2) GT '&SYSLIST(&PX1)'(&R3,&R2)).X00127000
               COMPX                                                    00128000
         AIF   ('&SYSLIST(&X1)'(&R1,&R2) LT '&SYSLIST(&PX1)'(&R3,&R2)).X00129000
               SQERR                                                    00130000
&R1      SETA  &R2+&R1            BUMP DISPLACEMENT (CURRENT ENTRY)     00131000
.*             IF CURRENT ENTRY'S COMPARE LENGTH IS EXHAUSTED AND       00132000
.*                  ENTRIES HAVE COMPARED EQUAL - THEN THE CURRENT      00133000
.*                       ENTRY IS EQUAL OR LESS.SQERR                   00134000
&LWORK   SETA  &LWORK-&R2     DECREMENT WORK LENGTH                     00135000
         AIF   (&LWORK LE 0 AND '&CORP' EQ 'C').SQERR                   00136000
&R3      SETA  &R2+&R3                                                  00137000
.*             IF PREVIOUS ENTRY'S COMPARE LENGTH IS EXHAUSTED AND      00138000
.*                  ENTRIES HAVE COMPARED EQUAL - THEN THE CURRENT      00139000
.*                       ENTRY IS GREATER.                              00140000
         AIF   (&LWORK LE 0 AND '&CORP' EQ 'P').COMPX                   00141000
.*                                                                      00142000
         AIF   (&LWORK GT 8).COMP1                                      00143000
&R2      SETA  &LWORK                                                   00144000
         AGO   .COMP                                                    00145000
.*                                                                      00146000
.COMP1   ANOP                                                           00147000
&R2      SETA  8                                                        00148000
         AGO   .COMP                                                    00149000
.COMPX   ANOP                                                           00150000
.GEN     ANOP                                                           00151000
&LPREV   SETA  &L                                                       00152000
&PD      SETA  &D                                                       00153000
&PX1     SETA  &X1                                                      00154000
.GEN1    ANOP                                                           00155000
         DC    YL1(&L)                                                  00156000
         AIF   ('&DATA2' NE '').GEN2                                    00157000
         DC    C'&DATA1 '                                               00158000
         AGO   .P1                                                      00159000
.GEN2    ANOP                                                           00160000
         AIF   ('&DATA3' NE '').GEN3                                    00161000
         DC    C'&DATA1&DATA2 '                                         00162000
         AGO   .P1                                                      00163000
.GEN3    ANOP                                                           00164000
         DC    C'&DATA1&DATA2&DATA3 '                                   00165000
         AGO   .P1                                                      00166000
.SQERR   MNOTE 1,'FOLLOWING ENTRY OUT OF SEQUENCE'                      00167000
         AGO   .GEN                                                     00168000
.INV     MNOTE 1,'&SYSLIST(&X1) EXCEEDS 20 BYTES'                       00169000
         MNOTE 1,'MACRO TERMINATED'                                     00170000
         MEXIT                                                          00171000
         MEND                                                           00172000
