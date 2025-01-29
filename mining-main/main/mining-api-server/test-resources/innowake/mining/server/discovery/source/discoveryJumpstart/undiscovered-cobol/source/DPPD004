00001 *                                                                 02/22/90
00002 **   DISPLAY SQLCA INFORMATION                                    DPPD004
00003 *                                                                    LV001
00004      CALL 'DSNTTTT' USING SQLCA                                   DPPD004
00005                           DP004-FORMATTED-MESSAGE                 DPPD004
00006                           DP004-ERROR-LINE-LENGTH.                DPPD004
00007 *                                                                 DPPD004
00008      DISPLAY DP004-ASTERISK-LINE.                                 DPPD004
00009      PERFORM                                                      DPPD004
00010          VARYING DP004-MSG-IDX                                    DPPD004
00011          FROM 1 BY 1                                              DPPD004
00012          UNTIL DP004-MSG-IDX > DP004-MAX-MESSAGE-LINES            DPPD004
00013              DISPLAY '--'                                         DPPD004
00014                      DP004-MESSAGE-LINE (DP004-MSG-IDX)           DPPD004
00015                      ' --'                                        DPPD004
00016      END-PERFORM.                                                 DPPD004
00017      DISPLAY DP004-ASTERISK-LINE.                                 DPPD004
