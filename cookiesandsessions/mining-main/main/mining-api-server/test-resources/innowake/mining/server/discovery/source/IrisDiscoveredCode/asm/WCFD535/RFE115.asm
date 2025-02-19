         MACRO                                                          00001000
&NAME    RFE115 &ENTRY                                                  00002000
         GBLA  &LPREV,&PD,&PX1                                          00003000
         LCLA  &X1,&R1,&R2,&R3,&D,&L,&KVAL,&LWORK                       00004000
         LCLA  &CURL                                                    00005000
         LCLC  &DATA1,&DATA2,&DATA3                                     00006000
         LCLC  &CORP                                                    00007000
*        RFE115 MACRO    *RCIF*                                   2.0   00008000
         TITLE 'CRF INDIVIDUAL SUFFIX TABLE'                            00009000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00025000
*                                                                     * 00026000
*                R E T A I L   S U F F I X   T A B L E                * 00027000
*                                                                     * 00028000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00029000
         SPACE                                                          00030000
SRETAIL  DS    0CL1 -             START OF RETAIL SUFFIX TABLE          00031000
&LPREV   SETA  0             RESET                                      00032000
&PD      SETA  0                   GLOBALS                              00033000
&PX1     SETA  0                                                        00034000
&X1      SETA  &X1+1                                                    00035000
         AGO   .PRE1                                                    00036000
.P1      ANOP                                                           00037000
&X1      SETA  &X1+1                                                    00038000
         AIF   (&X1 LT N'&SYSLIST OR &X1 EQ N'&SYSLIST).PRE1            00039000
         DC    X'FF' -            END OF SUFFIX TABLE                   00040000
         MEXIT                                                          00041000
.PRE1    ANOP                                                           00042000
         AIF   (K'&SYSLIST(&X1) GT 24).INV                              00043000
.*                                                                      00044000
.*  INITIALIZE  AND  CHECK  FOR LEADING PARENTHESIS                     00045000
&R1      SETA  1                                                        00046000
&D       SETA  1             SET INITIAL DISPLACEMENT VALUE             00047000
&L       SETA  K'&SYSLIST(&X1)    INITIAL LENGTH                        00048000
         AIF   ('&SYSLIST(&X1)'(1,1) NE '(').APOST                      00049000
&L       SETA  &L-2           DECREMENT LENGTH                          00050000
&D       SETA  &D+1           INCREMENT DISPLACEMENT                    00051000
.*                            *                                         00052000
.*  CHECK FOR LEADING QUOTE   *                                         00053000
.*                            *                                         00054000
.APOST   ANOP                                                           00055000
         AIF   ('&SYSLIST(&X1)'(&D,1) NE '''').LCHECK                   00056000
&L       SETA  &L-2           DECREMENT LENGTH                          00057000
&D       SETA  &D+1           INCREMENT DISPLACEMENT                    00058000
.LCHECK  ANOP                                                           00059000
&LWORK   SETA  &L                                                       00060000
         AIF   (&LWORK GT 20).INV                                       00061000
         AIF   (&LPREV EQ 0).DISP      BYPASS SEQUENCE CHECK            00062000
         AIF   (&L GT &LPREV).LPR                                       00063000
.*  USE LENGTH OF CURRENT ENTRY                                         00064000
&CORP    SETC  'C'           INDICATE USING CURRENT LENGTH              00065000
         AIF   (&L LE 8).L1                                             00066000
&R2      SETA  8                                                        00067000
         AGO   .DISP                                                    00068000
.*  USE LENGTH OF PREVIOUS ENTRY                                        00069000
.LPR     ANOP                                                           00070000
&CORP    SETC  'P'                                                      00071000
&LWORK   SETA  &LPREV                                                   00072000
         AIF   (&LPREV LE 8).LPR1 Q. L'PREVIOUS ENTRY LE 8              00073000
&R2      SETA  8                  A. NO, SET MAXIMUM LENGTH             00074000
         AGO   .DISP                                                    00075000
.L1      ANOP                 SET LENGTH OF 8 OR LESS (CURRENT LENGTH)  00076000
&R2      SETA  &L                                                       00077000
         AGO   .DISP                                                    00078000
.LPR1    ANOP                 SET LENGTH OF 8 OR LESS (PREVIOUS ENTRY)  00079000
&R2      SETA  &LPREV                                                   00080000
.DISP    ANOP                 SET INITIAL                               00081000
&R3      SETA  &PD             DISPLACEMENTS                            00082000
&R1      SETA  &D               FOR COMPARE                             00083000
.*                                                                      00084000
.*   RESET DATA FIELDS                                                  00085000
.*                                                                      00086000
&DATA1   SETC  ''                                                       00087000
&DATA2   SETC  ''                                                       00088000
&DATA3   SETC  ''                                                       00089000
.*                                                                      00090000
.*             SET DATA FOR GENERATION                                  00091000
&CURL    SETA  &L                                                       00092000
.*                                                                      00093000
         AIF   (&CURL LT 9).DAT1                                        00094000
.*                                                                      00095000
.*   CHARACTER STRING GREATER THAN 8                                    00096000
.*                                                                      00097000
&DATA1   SETC  '&SYSLIST(&X1)'(&R1,8)                                   00098000
&R1      SETA  &R1+8                                                    00099000
&CURL    SETA  &CURL-8                                                  00100000
         AIF   (&CURL LT 9).DAT2                                        00101000
&DATA2   SETC  '&SYSLIST(&X1)'(&R1,8)                                   00102000
&R1      SETA  &R1+8                                                    00103000
&CURL    SETA  &CURL-8                                                  00104000
         AIF   (&CURL LT 9).DAT3                                        00105000
&DATA3   SETC  '&SYSLIST(&X1)'(&R1,8)                                   00106000
         AGO   .DATAX                                                   00107000
.DAT1    ANOP                                                           00108000
&DATA1   SETC  '&SYSLIST(&X1)'(&R1,&CURL)                               00109000
         AGO   .DATAX                                                   00110000
.DAT2    ANOP                                                           00111000
&DATA2   SETC  '&SYSLIST(&X1)'(&R1,&CURL)                               00112000
         AGO   .DATAX                                                   00113000
.DAT3    ANOP                                                           00114000
&DATA3   SETC  '&SYSLIST(&X1)'(&R1,&CURL)                               00115000
.*                                                                      00116000
.DATAX   ANOP                                                           00117000
&R1      SETA  &D                                                       00118000
         AIF   (&LPREV EQ 0).GEN                                        00119000
.*  COMPARE CURRENT ENTRY TO LAST (IN SEQUENCE) ENTRY                   00120000
.COMP    ANOP                                                           00121000
         AIF   ('&SYSLIST(&X1)'(&R1,&R2) GT '&SYSLIST(&PX1)'(&R3,&R2)).X00122000
               COMPX                                                    00123000
         AIF   ('&SYSLIST(&X1)'(&R1,&R2) LT '&SYSLIST(&PX1)'(&R3,&R2)).X00124000
               SQERR                                                    00125000
&R1      SETA  &R2+&R1            BUMP DISPLACEMENT (CURRENT ENTRY)     00126000
.*             IF CURRENT ENTRY'S COMPARE LENGTH IS EXHAUSTED AND       00127000
.*                  ENTRIES HAVE COMPARED EQUAL - THEN THE CURRENT      00128000
.*                       ENTRY IS EQUAL OR LESS.SQERR                   00129000
&LWORK   SETA  &LWORK-&R2     DECREMENT WORK LENGTH                     00130000
         AIF   (&LWORK LE 0 AND '&CORP' EQ 'C').SQERR                   00131000
&R3      SETA  &R2+&R3                                                  00132000
.*             IF PREVIOUS ENTRY'S COMPARE LENGTH IS EXHAUSTED AND      00133000
.*                  ENTRIES HAVE COMPARED EQUAL - THEN THE CURRENT      00134000
.*                       ENTRY IS GREATER.                              00135000
         AIF   (&LWORK LE 0 AND '&CORP' EQ 'P').COMPX                   00136000
.*                                                                      00137000
         AIF   (&LWORK GT 8).COMP1                                      00138000
&R2      SETA  &LWORK                                                   00139000
         AGO   .COMP                                                    00140000
.*                                                                      00141000
.COMP1   ANOP                                                           00142000
&R2      SETA  8                                                        00143000
         AGO   .COMP                                                    00144000
.COMPX   ANOP                                                           00145000
.GEN     ANOP                                                           00146000
&LPREV   SETA  &L                                                       00147000
&PD      SETA  &D                                                       00148000
&PX1     SETA  &X1                                                      00149000
.GEN1    ANOP                                                           00150000
         DC    YL1(&L)                                                  00151000
         AIF   ('&DATA2' NE '').GEN2                                    00152000
         DC    C'&DATA1 '                                               00153000
         AGO   .P1                                                      00154000
.GEN2    ANOP                                                           00155000
         AIF   ('&DATA3' NE '').GEN3                                    00156000
         DC    C'&DATA1&DATA2 '                                         00157000
         AGO   .P1                                                      00158000
.GEN3    ANOP                                                           00159000
         DC    C'&DATA1&DATA2&DATA3 '                                   00160000
         AGO   .P1                                                      00161000
.SQERR   MNOTE 1,'FOLLOWING ENTRY OUT OF SEQUENCE'                      00162000
         AGO   .GEN                                                     00163000
.INV     MNOTE 1,'&SYSLIST(&X1) EXCEEDS 20 BYTES'                       00164000
         MNOTE 1,'MACRO TERMINATED'                                     00165000
         MEXIT                                                          00166000
         MEND                                                           00167000
