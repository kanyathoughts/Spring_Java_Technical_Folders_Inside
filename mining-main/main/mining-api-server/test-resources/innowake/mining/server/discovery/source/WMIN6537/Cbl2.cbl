       IDENTIFICATION DIVISION.
       PROGRAM-ID. Cbl2.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.
       BEGIN.
          EXEC SQL
            SELECT A.EXT_CLIENT_ID,TRD_DT                               03150000
              FROM TABLE_1 A                                            03160000
                  ,TABLE_2     B                                        03170000
                  ,TABLE_3 C                                            03180000
                  ,TABLE_4  ASST                                        03190000
                  ,TABLE_5          ADV                                 03200000
              WHERE CAST(SUBSTR(A.EXT_REF_NUM,1,15) AS DECIMAL(15,0))   03210000
                   = ASST.ACCT_ID                                       03220000
          END-EXEC
       GOBACK.
