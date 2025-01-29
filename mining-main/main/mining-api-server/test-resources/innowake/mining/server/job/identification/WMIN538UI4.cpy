      * SEND PAGE
           EXEC CICS SEND PAGE
           END-EXEC.
      
      * SEND MAP
           EXEC CICS SEND
               MAP    (WS-CURRENT-MAPNAME)
               MAPSET (WS-CURRENT-MAPSET)
               FROM   (UTMHELPO)
               LENGTH (LENGTH OF UTMHELPO)
               ERASE
           END-EXEC

      * SEND CONTROL
           EXEC CICS SEND CONTROL ERASE FREEKB
           END-EXEC

      * SEND TEXT
           EXEC CICS SEND TEXT
               FROM (WS-MSG-OUT)
               LENGTH(9)
               CURSOR(9)
              ERASE
           END-EXEC

      * RECEIVE MAP
           EXEC CICS RECEIVE
                MAP ('UTMHELP')
                MAPSET ('AI01HLM')
                INTO (UTMHELPI)
           END-EXEC

      * RECEIVE
           EXEC CICS RECEIVE
               LENGTH(WS-LENGTH)
           END-EXEC

      * SEND
           EXEC CICS SEND
               FROM(WS-FROM)
               LENGTH(WS-LENGTH)
           END-EXEC

