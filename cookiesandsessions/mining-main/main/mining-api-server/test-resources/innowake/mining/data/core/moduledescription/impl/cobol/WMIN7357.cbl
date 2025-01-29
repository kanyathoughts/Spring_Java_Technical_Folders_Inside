000100 IDENTIFICATION DIVISION.                                         00010099
000200 PROGRAM-ID. WMIN7357                                             00020099
000800*  ***************************************************************00080099
000900*  * DESCRIPTION:                                                *00090099
001000*  *  Please note: Module descriptions are calculated only       *00120099
001100*  *               if they don't exist yet! Otherwise you first  *00121099
001200*  *               have to remove it, before calling Mining ->   *00122099
001200*  *               Analysis -> Identify Module Descriptions      *00123099
001300*  *                                                             *00120099
003100*  * REVISION-                                                   *00290099
003200*  *    DATE    PERSON     REASON                                *00300099
003300*  * 22-01-17   NPR        Minified test case to reproduce       *00280099
003800*  ***************************************************************00310099
003900*                                                                 00320099
004400 DATA DIVISION.                                                   00370099
004500 WORKING-STORAGE SECTION.                                         00380099
004600*  * This field comment must not be collected !!!                 00390099
004700 01 FIELD1    PIC X(04)  VALUE 'Test'.                            00390199
004600*  * This INCLUDE comment must not be collected !!!               00390299
027700          INCLUDE ACDV1AA1                                        03050099
049200/                                                                 05160099
049300 LINKAGE SECTION.                                                 05170099
004600*  * This LINKAGE SECTION comment must not be collected !!!       00390099
049500 01  DFHCOMMAREA.                                                 05190099
049600     03  COM-TEST                PIC  X(04).                      05200099
052100/                                                                 05500099
052200***************************************************************** 05510099
052300 PROCEDURE DIVISION.                                              05520099
052400****************************************************************  05530099
052500                                                                  05540099
004600*  * This PROCEDURE DIVISION comment must not be collected !!!    00390099
052600     DISPLAY FIELD1.                                              05550099
052700                                                                  05560099
054200/                                                                 05710099
054300***************************************************************** 05720099
054400 0000-MAIN-PROCESS.                                               05730099
054500****************************************************************  05740099
004600*  * This 0000-MAIN-PROCESS comment must not be collected !!!     00390099
054600     MOVE 'End' TO FIELD1.                                        05760099
054700*  * This END comment must not be collected !!!                   00390099
