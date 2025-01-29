002600 IDENTIFICATION  DIVISION.                                                
002700 PROGRAM-ID.     FDSIRFMT IS COMMON.                              BIM00463
002800 ENVIRONMENT     DIVISION.                                                
002900 DATA            DIVISION.                                                
003000 WORKING-STORAGE SECTION.                                                 
003100 01  FILLER         PIC X(08) VALUE 'FDSIRFMT'.                           
003200 01  WS-MAX-DSP-LEN PIC S9(4) COMP  VALUE +20.                            
003300 01  WS-MAX-NUM-DEC PIC S9(4) COMP  VALUE +18.                                                                                                                             
006000 LINKAGE         SECTION.                                                 
006100 01  LS-VALUE-X  PIC X(9).                                                                                            
007800                                                                          
007900 PROCEDURE DIVISION USING  LS-VALUE-X                                     
008000                           LS-DISPLAY                                     
008100                           LS-DSP-LEN                                     
008200                           LS-NUM-DEC.                                                                                                                                                           
027500 END PROGRAM FDSIRFMT.                                                    
027600 EJECT                                                                    
027650                                                                  BIM00463
027700 IDENTIFICATION  DIVISION.                                        BIM00463
027800 PROGRAM-ID.     FDSIRFMR IS COMMON.                              BIM00463
027900 ENVIRONMENT     DIVISION.                                        BIM00463
028000 DATA            DIVISION.                                        BIM00463
028100 WORKING-STORAGE SECTION.                                         BIM00463
028200 01  FILLER         PIC X(08) VALUE 'FDSIRFMR'.                   BIM00463
028300 01  I              PIC S9(4) COMP.                               BIM00463
028400 01  J              PIC S9(4) COMP.                               BIM00463
028500                                                                  BIM00463
028600 01  WS-DSP      PIC X(20) VALUE SPACE.                           BIM00463
028700                                                                  BIM00463
028800 01  WS-DSP2     PIC X(20) VALUE SPACE.                           BIM00463
028900                                                                  BIM00463
029000 LINKAGE         SECTION.                                         BIM00463
029100 01  LS-VALUE-X  PIC X(9).                                        BIM00463
029500                                                                  BIM00463
029600 PROCEDURE DIVISION USING  LS-VALUE-X                             BIM00463
029700                           LS-DISPLAY                             BIM00463
029800                           LS-DSP-LEN                             BIM00463
029900                           LS-NUM-DEC.                            BIM00463                                                      
032700                                                                  BIM00463
032800 END PROGRAM FDSIRFMR.                                            BIM00463
