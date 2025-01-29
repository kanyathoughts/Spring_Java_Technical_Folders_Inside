      * modfied to not contain variable X or NO ADVANCING
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      PRG01.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 B             PIC X(333).
       01 A             PIC X(333) VALUE
           '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWX
      -    'YZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUV
      -    'WXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRST
      -    'UVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQR
      -    'STUVWXYZ0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOP
      -    'QRSTUVWXYZ'.
       PROCEDURE        DIVISION.
           MOVE
           'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567
      -    '89abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ012345
      -    '6789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123
      -    '456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01
      -    '23456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ
      -    '0123456789' TO B.
           DISPLAY A B.
           STOP RUN.