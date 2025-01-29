*
* Sample of a callable program (refreshable or re-enterable)
* This sample is usable also for reusable or non-reusable programs.
*
SUBPROG  CSECT
         USING SUBPROG,R15         Register 15 contains address
SUBPROG
         B     START               Skip data
         DC    C'SUBPROG '         Program-name
         DC    C'&SYSDATE'         Date
         DC    C'&SYSTIME'         Time
         DC    C'V1R2.05'          Version number
         DC    0H                  Re-align on halfword-boundary
*
START    STM   R14,R12,12(R13)     Save all registers
         DROP  R15                 No longer needed as base
         LR    R12,R15             Fill reg.12 with base address
         USING SUBPROG,R12         Use reg.12 as base
         LA    R1,PRIVATE_LEN      Amount of storage required
         GETMAIN RU,LV=(R1)        Allocate storage for save-area etc.
* Address of allocated storage now in register 1
         USING PRIVATE,R13         Make storage addressable
         ST    R13,4(R1)           Point to previous save-area
         ST    R1,8(R13)           Point to next save-area
         LR    R13,R1              R13 points to a free save-area again
*        ...                       Other program-code

EXIT     LR    R1,R13              Keep address of our private area
         L     R13,4(R13)          Get address of previous save-area
         LA    R2,PRIVATE_LEN
         FREEMAIN A=(R1),LV=(R2)   Free allocated storage
         LM    R14,R12,12(R13)     Restore all registers (except 13)
         LA    R15,...             Returncode in reg.15
         BR    R14                 Return to caller
         DROP  R12                 Base no longer needed
*
         LTORG                     All literals
*
* This dsect describes all variables private to each caller.
PRIVATE  DSECT
SAVEAREA DS    18F
*        ...                       Other private variables
PRIVATE_LEN EQU *-PRIVATE