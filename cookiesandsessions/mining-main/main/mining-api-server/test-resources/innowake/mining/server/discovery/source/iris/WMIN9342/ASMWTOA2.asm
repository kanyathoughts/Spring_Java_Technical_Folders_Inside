ASMWTOA2 CSECT
***********************************************************************
*             ASMWTOA1.asm - This is an HLASM Program                 *
***********************************************************************
* The purpose of this program is to test the various forms of the     *
* WTO (Write-To-Operator) macro.                                      *
***********************************************************************
* WTO to display the copyright information
*
         LTR   R15,R15     
         ABEND 2040,DUMP      

***********************************************************************
*
         END