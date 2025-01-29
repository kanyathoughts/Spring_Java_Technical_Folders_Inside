       IDENTIFICATION DIVISION.
       PROGRAM-ID. IBMMF.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
            EXEC SQL 
               INSERT INTO VINST_BUS_PROCS_DT
               ( BUS_PROCS_DT_CD
               ,BUS_PROCS_DT
               ,ABBRV_NM
               ,NM
               ,DESC
               ,LST_UPDTD_USER_ID
               ,LST_UPDTD_TS
               )
               VALUES ( :WS-BUS-PROCS-DT-CD
               , :WS-PROCESS-DATEX
               , 'ALNOMNIDTE'
               , 'ALN OMNI PROCESS DATE'
               , 'ALN OMNI PROCESS DATE USED TO CREATE LATEST ALN
      -          'ALN OMNI PROCESS DATE' 
               , :WS-ISAB0561
               , CURRENT TIMESTAMP
               )
           END-EXEC
