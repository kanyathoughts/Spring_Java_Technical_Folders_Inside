00001  IDENTIFICATION DIVISION.                                         
00002  PROGRAM-ID. MGOPGM1.
00078  ENVIRONMENT DIVISION.                                                                                                               
00080  DATA DIVISION.                                                   
00081  WORKING-STORAGE SECTION.                                                                                                                                                  
00338  01  WK-COMMAREA.                                                    
00191      03  WK-COMM-SW               PIC X(10).
           03  WK-COMM-LENGTH           PIC X(04).                                                                                                         
00378  PROCEDURE DIVISION.                                              
00382                                                                   
00484      EXEC CICS LINK                                                  
00485                PROGRAM ('MGOPGM2')                                
00486               COMMAREA (WK-COMMAREA)                                
00487                 LENGTH (WK-COMM-LEN)                                 
00488      END-EXEC.                                                    
           EXIT.