       IDENTIFICATION DIVISION.
       PROGRAM-ID. WMIN539FILE1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.

      * READ
           READ FILE
           END-READ.

      * WRITE
           WRITE RECORD
           END-WRITE.
           
      * REWRITE
           REWRITE RECORD
           END-REWRITE.
      
      * DELETE
           DELETE FILE
           END-DELETE.

      * READ
           READ FILE
           END-READ.
      
           GOBACK.
