********************************************************************
******* MMRS-M01  MAINFRAME MODERNIZATION REFERENCE SYSTEM
********************************************************************
******* MMRS7ZA1  GET DATE TIME AND CPU-TIME
********************************************************************
MMRS7ZA1 TITLE 'MMRS7ZA1 - get date time and cpu time '
MMRS7ZA1 CSECT
MMRS7ZA1 AMODE 31
MMRS7ZA1 RMODE ANY
         SAVE  (14,12),,*
         LR    R3,R15
         USING MMRS7ZA1,R3
         ST    R13,SAVE+4
         LR    R12,R13
         LA    R13,SAVE
         ST    R13,8(R12)
         L     R2,0(,R1)
         USING MYPARM,R2
*
TIME10   EQU   *
*
*        GET DATE AND TIME
*
         XC    TIMEDATE,TIMEDATE
         TIME  DEC,TIMEDATE,ZONE=LT,LINKAGE=SYSTEM,DATETYPE=YYYYMMDD
         UNPK  TEMP(13),TIMEDATE(7)
         MVC   PARZEIT,TEMP
         UNPK  TEMP(9),TIMEDATE+8(5)
         MVC   PARDATUM,TEMP
         ST    R15,RC1
*
TIME20   EQU   *
*
*        USED CPU-TIME
*
         XC    CPUTIME,CPUTIME
         TIMEUSED STORADR=CPUTIME,LINKAGE=SYSTEM,CPU=MIC
         MVC   PARCPU,CPUTIME
         ST    R15,RC2
*
TIME30   EQU   *
*
*        BACK TO CALLER
*
         ICM   R15,15,RC1        TIME RC
         BNZ   TIME40
         ICM   R15,15,RC2        CPU  RC
TIME40   DS    0H
         L     R13,4(R13)
         RETURN (14,12),RC=(15)
*
         B     ENDIGN
         EJECT
* TEST ASSEMBLER
MYNUM3   EQU   3
MYDATA   DS    CL8
* JUST ONE PARAMETER EQUATES
         LA    R1,20
         LA    R1,(20)
         LA    R1,(MYNUM3)
* JUST ONE PARAMETER EXPRESSIONS
         LA    R1,20+7
         LA    R1,(20+7)
         LA    R1,(MYNUM3+7)
* JUST ONE PARAMETER DATAFIELDS
         LA    R1,MYDATA
         LA    R1,(MYDATA)
         LA    R1,MYDATA+7
         LA    R1,(MYDATA+7)
* ONLY INDEX REGISTER
         LA    R1,(5*4)(R2)
         LA    R1,(5*4)(R2+1)
* ONLY BASE REGISTER
         LA    R1,20(,MYNUM3)
         LA    R1,20+7(,MYNUM3)
         LA    R1,(20+7)(,MYNUM3)
         LA    R1,(5*4)(,R2)
* BASE AND INDEX REGISTER
         LA    R1,(20)(MYNUM3,R2)
         LA    R1,(5*4)(3,R2)
         LA    R1,(5*4)(R3,R2)
         LA    R1,(5*4)(R3+1,R2)
* STATEMENTS NOT ACCEPTED BY ASSEMBLER ===============================
*        LA    R1,(,3)
*        LA    R1,(,MYNUM3)
*        LA    R1,20(MYDATA)
*        LA    R1,20(,MYDATA)
*        LA    R1,20(,MYDATA)
* HERE ARE THE ERROR MESSAGES OF ASSEMBLER
* 000136 0000 0000            00000   131          LA    R1,(,3)
* ** ASMA074E ILLEGAL SYNTAX IN EXPRESSION - (
* ** ASMA435I RECORD 89 IN MMRS00A.A.COBOL(MMRS0ZA1) ON VOLUME: VPWRKK
* 00013A 0000 0000            00000   132          LA    R1,(,MYNUM3)
* ** ASMA074E ILLEGAL SYNTAX IN EXPRESSION - (
* ** ASMA435I RECORD 90 IN MMRS00A.A.COBOL(MMRS0ZA1) ON VOLUME: VPWRKK
* 00013E 0000 0000            00000   133          LA    R1,20(MYDATA)
* ** ASMA029E INCORRECT REGISTER SPECIFICATION - MYDATA
* ** ASMA435I RECORD 91 IN MMRS00A.A.COBOL(MMRS0ZA1) ON VOLUME: VPWRKK
* 000142 0000 0000            00000   134          LA    R1,20(,MYDATA)
* ** ASMA029E INCORRECT REGISTER SPECIFICATION - MYDATA
* ** ASMA435I RECORD 92 IN MMRS00A.A.COBOL(MMRS0ZA1) ON VOLUME: VPWRKK
* 000146 0000 0000            00000   135          LA    R1,20(,MYDATA)
* ** ASMA029E INCORRECT REGISTER SPECIFICATION - MYDATA
* ** ASMA435I RECORD 93 IN MMRS00A.A.COBOL(MMRS0ZA1) ON VOLUME: VPWRKK
*
ENDIGN   DS    0H
* END OF STATEMENTS NOT ACCEPTED BY ASSEMBLER ========================
*
*
SAVE     DC    18F'0'
TEMP     DS    CL20
*
TIMEPARM DS    0H
TIMEDATE DC    2D'0'
CPUTIME  DC    D'0'
*
         LTORG
         YREGS
*
MYPARM   DSECT
PAREYET  DS    CL4
RC1      DC    F'0'
PARZEIT  DS    CL12
PARDATUM DS    CL8
PAREYEC  DS    CL4
RC2      DC    F'0'
PARCPU   DS    CL8
*
         END   MMRS7ZA1
