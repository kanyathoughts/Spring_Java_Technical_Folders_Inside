       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIN2449.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 VARIABLE PIC X(05) VALUE 'NEW'.
       PROCEDURE DIVISION.
       ENTER PL1 'ROUTINENAME' USING dn3.
       ENTER ASM 'SAMPLE' USING dn1 .
       ENTER FTN "SUBROUTINE-NAME" USING dn1, dn2.
       ENTER MASM "C$INFO" USING NEW-FILE, NEW.
       ENTER MASM "C$INFO" USING dn1.
       ENTER MASM 'PRISE' USING WU-POINTERS, 10.
       STOP