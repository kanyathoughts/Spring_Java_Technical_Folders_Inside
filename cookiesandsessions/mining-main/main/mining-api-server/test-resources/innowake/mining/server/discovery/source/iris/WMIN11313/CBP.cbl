       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PROGA.
      ****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       PROCEDURE DIVISION.
       0210-PROCESS-REC.
           EXEC SQL
                CALL SQLPGM (:WS-CCYYMMDD,
                                    :WS-DDMMCCYY)
           END-EXEC.
           EXEC SQL
                CALL SYSPROC.SPROC1 (:WS-CCYYMMDD,
                                       :WS-DDMMCCYY)
           END-EXEC.
           EXEC SQL
                CALL SYSPROC.SPROC2 (:WS-CCYYMMDD,
                                      :WS-DDMMCCYY)
           END-EXEC.
           EXEC SQL
                    CALL SPROC3 (:WS-CCYYMMDD,
                                          :WS-DDMMCCYY)
           END-EXEC.
       0210-EXIT.
           EXIT.
           EJECT
