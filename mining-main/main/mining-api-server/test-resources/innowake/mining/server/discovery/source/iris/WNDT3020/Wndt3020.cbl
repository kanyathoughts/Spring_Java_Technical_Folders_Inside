00001  IDENTIFICATION DIVISION.                                         
00002  PROGRAM-ID. Wndt3020.                                            
00003  INSTALLATION TXDOT.                                              
00109  ENVIRONMENT DIVISION.                                            
00117  DATA DIVISION.                                                   
00118                                                                   
00119  WORKING-STORAGE SECTION.                                         
00120                                                                   
00134  ++INCLUDE W3020C1                                                
00138                                                                   
00193      EXEC ADABAS                                                  
00194           FIND                                                    
00195           DECLARE A058 CURSOR FOR                                 
00196           SELECT IWFIELD                                          
00200           FROM IWTABLE                                            
00201           WHERE SUPIW BETWEEN :SUPIW-START                        
00202                 AND :SUPIW-END                                    
00203      END-EXEC.                                                    
00204                                                                   
00212  PROCEDURE DIVISION.                                              
00213                                                                   
00230  ++INCLUDE W3020C2                                                
00244      IF MF-IW-IN NOT = ZERO                                       
00245          MOVE ZEROES             TO SD-IW-STRT                    
00247      ELSE                                                         
00248          MOVE ZEROES             TO SD-IW-STRT                    
00249          MOVE 12345678           TO SD-IW-END                     
00250      END-IF.                                                      
00251                                                                   
00409  END PROGRAM Wndt3020.