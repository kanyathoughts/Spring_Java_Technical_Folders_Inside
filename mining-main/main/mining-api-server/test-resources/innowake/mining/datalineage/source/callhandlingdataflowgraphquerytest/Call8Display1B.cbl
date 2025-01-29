       IDENTIFICATION DIVISION.
       PROGRAM-ID. WC47CLLR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

        01 DUMMY PIC X(4).

        01 WS-FIELD-A-1 PIC X(1).
        01 WS-FIELD-A-2 PIC X(2).
        01 WS-FIELD-A-3 PIC X(3).
        01 WS-FIELD-A-4 PIC X(4).

        01 WS-FIELD-B-1 PIC X(1).
        01 WS-FIELD-B-2 PIC X(2).
        01 WS-FIELD-B-3 PIC X(3).
        01 WS-FIELD-B-4 PIC X(4).

       PROCEDURE DIVISION.

         CALL "Call8Display1A" USING WS-FIELD-A-1.
         CALL "Call8Display1A" USING WS-FIELD-A-2.
         CALL "Call8Display1A" USING WS-FIELD-A-3.
         CALL "Call8Display1A" USING WS-FIELD-A-4.
         CALL "Call8Display1A" USING DUMMY, WS-FIELD-B-1.
         CALL "Call8Display1A" USING DUMMY, WS-FIELD-B-2.
         CALL "Call8Display1A" USING DUMMY, WS-FIELD-B-3.
         CALL "Call8Display1A" USING DUMMY, WS-FIELD-B-4.

       END PROGRAM WC47PGM8.
