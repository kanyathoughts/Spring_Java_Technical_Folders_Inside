ASMWTOA1 CSECT
***********************************************************************
*             ASMWTOA1.asm - This is an HLASM Program                 *
***********************************************************************
* The purpose of this program is to test the various forms of the     *
* WTO (Write-To-Operator) macro.                                      *
***********************************************************************
* WTO to display the copyright information
*
         WTO   '* ASMWTOA1 An Example of using the WTO macro  '
         LTR   R15,R15     
         BNZ   ABEND4      
*
         WTO   '* ASMWTOA1 '
         LTR   R15,R15     
         BNZ   ABEND4      
***********************************************************************
*
         END