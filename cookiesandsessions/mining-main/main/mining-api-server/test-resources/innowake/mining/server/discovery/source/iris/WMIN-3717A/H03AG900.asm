********* LAAD ROUTINE VOOR H03AG910 (ALGEMEEN PRINT MODULE) ********   00000100
H03AG900 START                                                          00000200
         LOAD   EP=H03AG910    LAAD H03AG910                            00002000
		 CALL   ASMCBLC1,(PHX1CNTL,PASSDATA)							00002001
		 LINK   EP=ASMCBLC2												00002002
		 LINKX  EP=ASMCBLC3												00002002
		 COPY   FQI5021													00002002
		 XXENTRY BASEREG=(R12,R11),PREFIX=R,RENT=(WORKLTH,S),          C        
         XXBDS1XA USE,MSGN='29',PRINT=NOGEN                                     
         END                                                            00005000 