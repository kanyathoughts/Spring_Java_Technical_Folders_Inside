000100*01  IOPCB.                                                       0000100
000200     05  E1890-PCB-LTERM-NAME                                     0000100
000300                             PIC  X(8).                           0000000
000400     05  E1891-PCB-DLI-RESRV PIC  X(2).                           0000900
000500     05  E1864-PCB-STA-CD    PIC  X(2).                           0001100
000600     05  PREFIX.                                                  0001300
000700         10  E1892-PCB-JUL-DATE                                   0001300
000800                             PIC  S9(07)      COMP-3.             0000000
000900         10  E1893-PCB-TIME  PIC  S9(07)      COMP-3.             0001700
001000         10  E1894-PCB-MSG-SEQ                                    0002100
001100                             PIC  S9(07)      COMP.               0000000
001200     05  E1895-PCB-MOD-NAME  PIC  X(8).                           0002500
001200     05  E1895-PCB-USER-ID   PIC  X(8).                           0002500
