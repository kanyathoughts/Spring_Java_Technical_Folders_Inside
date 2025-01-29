000100*01  DBPCB.                                                           1
000200     05  E1896-PCB-DBD-NAME  PIC  X(8).                               1
000300     05  E1897-PCB-SEG-LEVEL PIC  X(2).                               9
000400     05  E1864-PCB-STA-CD    PIC  X(2).                              11
000500     05  E1898-PCB-PROC-OPTS PIC  X(4).                              13
000600     05  E1899-DBPCB-DLI-RESRV                                       17
000700                             PIC  S9(05)      COMP.
000800     05  E1900-PCB-SEG-NAME  PIC  X(8).                              21
000900     05  E1901-PCB-KEY-LENGTH                                        29
001000                             PIC  S9(05)      COMP.
001100     05  E1902-PCB-NUM-SENS-SEG                                      33
001200                             PIC  S9(05)      COMP.
001300     05  E1903-PCB-CONT-KEY  PIC  X(220).                            37
