000060 IDENTIFICATION DIVISION.
000080
000090 PROGRAM-ID.  TEST.
000100
000110 AUTHOR. MARSIN S RAJ (A49).
000120
000130 DATE-WRITTEN.  9-DEC-2016.

000260 ENVIRONMENT DIVISION.
000270
000280 CONFIGURATION SECTION.
000290 SOURCE-COMPUTER.        VAX.
000300 OBJECT-COMPUTER.        VAX.
000310
000320 INPUT-OUTPUT SECTION.
000400 DATA DIVISION.
000410 WORKING-STORAGE SECTION.

003460 01  INV_AA_RECORD.
003630     03  INV_AA_ERROR                   PIC 9(03).
003910     03  INV_AA_SVC                     PIC X(02).
003920     03  INV_AA_ATTACH                  PIC X(1).

003990 01  WS_DB_FIELDS.
004200     05  WS_NULL_IND_21                  PIC S9(09) COMP.
004210     05  WS_NULL_IND_22                  PIC S9(09) COMP.
004220     05  WS_NULL_IND_23                  PIC S9(09) COMP.

000590*DECLARE_CURSOR.
000600
000810      EXEC SQL
000820           DECLARE GLOBAL CLMINV ALIAS FILENAME 'ClmInv_Db'
000830      END-EXEC.

004920 PROCEDURE DIVISION.
015660     STOP RUN.
