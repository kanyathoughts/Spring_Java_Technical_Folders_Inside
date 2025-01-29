003300 IDENTIFICATION DIVISION.                                         00003300
005000 ENVIRONMENT DIVISION.                                            00005000
005100 DATA DIVISION.                                                   00005100
005200 WORKING-STORAGE SECTION.                                         00005200                               
000060 01  W-STATUS-CODE               PIC X(08).                       00650000
006200 LINKAGE SECTION.                                                 00006200                 
006900 01  PARM-4                      PIC X(01).                       00006900
007000                                                                  00007000
007100* DYNAMICALLY SET AREA                                            00007100
007200 01  COMMUNICATION-AREA.         COPY OQDCBCA.                    00007200
007300                                                                  00007300
007400 PROCEDURE DIVISION USING CLASS-FUNCTION                          00007400                                
007700                          PARM-4                                  00007700
007800     .                                                            00007800
007900                                                                  00007900
008000     SET ADDRESS OF COMMUNICATION-AREA                            00008000
008100      TO ADDRESS OF PARM-2                                        00008100
           MOVE SPACES                                                          
             TO W-LMOD-NAME                                                     
                W-STATUS-CODE.                                          00008400
