00001 ***************************************************************** 02/22/90
00002 **                                                             ** DPWS004
00003 ** DPWS004                                                     **    LV002
00004 **                                                             ** DPWS004
00005 **                                                             ** DPWS004
00006 ***************************************************************** DPWS004
00007  01  DP004-DB2-ERROR-FIELDS.                                      DPWS004
00008      05  DP004-ASTERISK-LINE       PIC  X(80)  VALUE ALL '*'.        CL**2
00009      05  DP004-MAX-MESSAGE-LINES   PIC S9(04)  VALUE +12             CL**2
00010                                                COMP SYNC.            CL**2
00011      05  DP004-ERROR-LINE-LENGTH   PIC S9(09)  VALUE +75          DPWS004
00012                                                COMP SYNC.         DPWS004
00013      05  DP004-FORMATTED-MESSAGE.                                 DPWS004
00014          10  FILLER                PIC S9(04)  VALUE +900         DPWS004
00015                                                COMP SYNC.         DPWS004
00016          10  DP004-MESSAGE-LINE    OCCURS 12 TIMES                DPWS004
00017                                    INDEXED BY DP004-MSG-IDX       DPWS004
00018                                    PIC  X(75).                    DPWS004
