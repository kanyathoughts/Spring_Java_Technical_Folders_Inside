       IDENTIFICATION DIVISION.
       PROGRAM-ID. WC47CLLR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

        01 DUMMY PIC X(1).

        01 WS-FIELD-A-1 PIC X(1).
        01 WS-FIELD-A-2 PIC X(2).
        01 WS-FIELD-A-3 PIC X(3).
        01 WS-FIELD-A-4 PIC X(4).

        01 WS-FIELD-B-1 PIC X(1).
        01 WS-FIELD-B-2 PIC X(2).
        01 WS-FIELD-B-3 PIC X(3).
        01 WS-FIELD-B-4 PIC X(4).

       PROCEDURE DIVISION.

           CALL "TEST9a" USING WS-FIELD-A-1.
      * 9c does not exist
           CALL "TEST9c" USING WS-FIELD-A-1.
         
           CALL "TEST9a" USING WS-FIELD-A-2.
      * 9c does not exist   
           CALL "TEST9c" USING WS-FIELD-A-2.
         
           CALL "TEST9a" USING WS-FIELD-A-3.
      * 9c does not exist   
           CALL "TEST9c" USING WS-FIELD-A-3.
         
           CALL "TEST9a" USING WS-FIELD-A-4.
      * 9c does not exist   
           CALL "TEST9c" USING WS-FIELD-A-4.
         
           CALL "TEST9a" USING DUMMY, WS-FIELD-B-1.
      * 9c does not exist   
           CALL "TEST9c" USING DUMMY, WS-FIELD-B-1.
         
           CALL "TEST9a" USING DUMMY, WS-FIELD-B-2.
      * 9c does not exist   
           CALL "TEST9c" USING DUMMY, WS-FIELD-B-2.
         
           CALL "TEST9a" USING DUMMY, WS-FIELD-B-3.
      * 9c does not exist   
           CALL "TEST9c" USING DUMMY, WS-FIELD-B-3.
     
           CALL "TEST9a" USING DUMMY, WS-FIELD-B-4.
      * 9c does not exist   
           CALL "TEST9c" USING DUMMY, WS-FIELD-B-4.

       END PROGRAM WC47PGM8.
