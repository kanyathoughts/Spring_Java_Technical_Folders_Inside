
000010 IDENTIFICATION DIVISION.                                                 
000020 PROGRAM-ID.    APL001.                                                   
000030 AUTHOR.        XXXXX XXXXXX.                                                                                
000050 DATE-WRITTEN.  FEBRUARY 12, 2024.                                         
000060 DATE-COMPILED.                                                                      
000160                                                                          
000170 ENVIRONMENT DIVISION.                                                    
000180 CONFIGURATION SECTION.                                                   
000190 SOURCE-COMPUTER.    IBM-370.                                             
000200 OBJECT-COMPUTER.    IBM-370.                                             
000210 DATA DIVISION.                                                           
000220     EJECT                                                                
000230 WORKING-STORAGE SECTION.                                                 
000240                                                                          
000250 01  APL001I.                                                             
000260     05  APL001-INP-SEG-01-LENGTH       PIC S9(04) COMP.                  
000270     05  APL001-INP-SEG-01-ZZ           PIC S9(04) COMP.                  
000280     05  E1912-TRAN-CODE                PIC X(08).                      
000290     05  FILLER                         PIC X(1).                         
000300     05  SCREEN-DISPLAY-FLAG            PIC X(1).                         
000310     05  APL001I-WORK-AREA.                                               
000320         10  FILLER                     PIC X(3).                         
000330         10  APL001I-ID                 PIC X(4).                         
000340         10  APL001I-PW                 PIC X(8).                         
000350         10  FILLER                     PIC X(22).                        
000360     05  AP0002I-WORK-AREA REDEFINES APL001I-WORK-AREA.                 
000370         10  FILLER                     PIC X(16).                        
000380         10  AP0002I-ID                 PIC X(4).                         
000390         10  AP0002I-PW                 PIC X(8).                         
000400         10  FILLER                     PIC X(09).                        
000410     05  AP0003I-WORK-AREA REDEFINES APL001I-WORK-AREA.                 52
000420         10  FILLER                     PIC X(16).                        
000430         10  AP0003I-ID                 PIC X(4).                         
000440         10  AP0003I-PW                 PIC X(8).                         
000450         10  FILLER                     PIC X(09).                        
000460*                                                                         
000470*    05  APL001-INP-SEG-01-LENGTH       PIC S9(04) COMP.                  
000480*    05  APL001-INP-SEG-01-ZZ           PIC S9(04) COMP.                  
000490*    05  APL001I-TRAN-CD                PIC X(8).                         
000500*    05  FILLER                         PIC X(1).                         
000510*    05  SCREEN-DISPLAY-FLAG            PIC X(1).                         
000520*    05  FILLER                         PIC X(3).                         
000530*    05  FILLER                         PIC X(11).                        
000540*    05  APL001I-ID                     PIC X(4).                         
000550*    05  APL001I-PW                     PIC X(8).                         
000560**** 05  FILLER                         PIC X(948).                     30
000570*                                                                         
000580                                                                          
000590 01  AP0001O.                                                             
000600     05  AP0001-OUT-SEG-01-LENGTH       PIC S9999 COMP.                   
000610     05  AP0001-OUT-SEG-01-ZZ           PIC S9999 COMP.                   
000620     05  AP0001O-ID                     PIC X(4).                         
000630     05  AP0001O-PW                     PIC X(8).                         
000640     05  FILLER                         PIC X(224) VALUE SPACES.          
000650                                                                          
000660*    05  AP0001O-ID-ATTR                PIC X(2).                         
000670*    05  AP0001O-PW-ATTR                PIC X(2).                         
000680                                                                          
000690 01  AP0002O.                                                             
000700     05  AP0002-OUT-SEG-01-LENGTH       PIC S9999 COMP.                   
000710     05  AP0002-OUT-SEG-01-ZZ           PIC S9999 COMP.                   
000720     05  AP0002O-ID                     PIC X(4).                         
000730     05  AP0002O-PW                     PIC X(8).                         
000740     05  FILLER                         PIC X(589) VALUE SPACES.          
000750                                                                          
000760*    05  AP0002O-ID-ATTR                PIC X(2).                         
000770*    05  AP0002O-PW-ATTR                PIC X(2).                         
000780                                                                          
000790 01  AP0003O.                                                             
000800     05  AP0003-OUT-SEG-01-LENGTH       PIC S9999 COMP.                   
000810     05  AP0003-OUT-SEG-01-ZZ           PIC S9999 COMP.                   
000820     05  AP0003O-ID                     PIC X(4).                         
000830     05  AP0003O-PW                     PIC X(8).                         
000840     05  FILLER                         PIC X(768) VALUE SPACES.          
000850                                                                          
000860*    05  AP0003O-ID-ATTR                PIC X(2).                         
000870*    05  AP0003O-PW-ATTR                PIC X(2).                         
000880                                                                          
000890 01  MISC-STORAGE.                                                        
000900     05  GU                             PIC X(04) VALUE 'GU  '.           
000910     05  ISRT                           PIC X(04) VALUE 'ISRT'.           
000920     05  MOD-NAME                       PIC X(08) VALUE SPACES.           
000930     05  NO-MORE-MSG-SW                 PIC X(01) VALUE 'N'.              
000940         88  NO-MORE-MSG                          VALUE 'Y'.              
000950                                                                          
000960 01  ABEND-LOCATOR.                                                       
000970     05  FILLER                         PIC X(36) VALUE                   
000980         'APL001 CALLED IMSABEND AT LOCATION ('.                          
000990     05  ABEND-LOCATION                 PIC X(01).                        
001000     05  FILLER                         PIC X(25) VALUE                   
001010         '), WITH IMS STATUS CODE ('.                                     
001020     05  ABEND-STATUS-CODE              PIC X(02).                        
001030     05  FILLER                         PIC X(02) VALUE ').'.             
001040                                                                          
001050 LINKAGE SECTION.                                                         
001060 01  IO-PCB.                  COPY IOPCB.                                 
001070     EJECT                                                                
001080 PROCEDURE DIVISION.                                                      
001090     ENTRY 'DLITCBL' USING IO-PCB.                                        
001100     PERFORM 0100-GET-MSG.                                                
001110     PERFORM 0200-ISRT-SCREEN                                             
001120       UNTIL NO-MORE-MSG.                                                 
001130     GOBACK.                                                              
001140                                                                          
001150                                                                          
001160                                                                          
001170 0100-GET-MSG.                                                            
001180     CALL 'CBLTDLI'   USING GU                                            
001190                            IO-PCB                                        
001200                            APL001I.                                      
001210                                                                          
001220     IF E1864-PCB-STA-CD OF IO-PCB = SPACES                               
001230        NEXT SENTENCE                                                     
001240     ELSE                                                                 
001250        IF E1864-PCB-STA-CD OF IO-PCB = 'QC'                              
001260           MOVE 'Y' TO NO-MORE-MSG-SW                                     
001270        ELSE                                                              
001280           MOVE E1864-PCB-STA-CD OF IO-PCB                                
001290             TO ABEND-STATUS-CODE                                         
001300           MOVE '1' TO ABEND-LOCATION                                     
001310           CALL 'IMSABEND'.                                               
001320     EJECT                                                                
001330 0200-ISRT-SCREEN.                                                        
001340     IF SCREEN-DISPLAY-FLAG = '1'                                         
001350        MOVE +240       TO  AP0001-OUT-SEG-01-LENGTH                      
001360        MOVE ZEROS      TO  AP0001-OUT-SEG-01-ZZ                          
001370        MOVE APL001I-ID      TO   PA0001O-ID                              
001380        MOVE APL001I-PW      TO   PA0001O-PW                              
001390        MOVE 'AP0001O ' TO  MOD-NAME                                      
001400        CALL 'CBLTDLI'  USING  ISRT                                       
001410                               IO-PCB                                     
001420                               AP0001O                                    
001430                               MOD-NAME                                   
001440        IF E1864-PCB-STA-CD OF IO-PCB = SPACES                            
001450           NEXT SENTENCE                                                  
001460        ELSE                                                              
001470           MOVE E1864-PCB-STA-CD OF IO-PCB                                
001480             TO ABEND-STATUS-CODE                                         
001490           MOVE '2' TO ABEND-LOCATION                                     
001500           CALL  'IMSABEND'                                               
001510     ELSE                                                                 
001520     IF SCREEN-DISPLAY-FLAG = '2'                                         
001530        MOVE +605       TO  PA0002-OUT-SEG-01-LENGTH                      
001540        MOVE ZEROS      TO  PA0002-OUT-SEG-01-ZZ                          
001550        MOVE AP0002I-ID      TO   AP0002O-ID                              
001560        MOVE AP0002I-PW      TO   AP0002O-PW                              
001570        MOVE 'AP0002O ' TO  MOD-NAME                                      
001580        CALL 'CBLTDLI'  USING  ISRT                                       
001590                               IO-PCB                                     
001600                               AP0002O                                    
001610                               MOD-NAME                                   
001620        IF E1864-PCB-STA-CD OF IO-PCB = SPACES                            
001630           NEXT SENTENCE                                                  
001640        ELSE                                                              
001650           MOVE E1864-PCB-STA-CD OF IO-PCB                                
001660             TO ABEND-STATUS-CODE                                         
001670           MOVE '3' TO ABEND-LOCATION                                     
001680           CALL  'IMSABEND'                                               
001690     ELSE                                                                 
001700     IF SCREEN-DISPLAY-FLAG = '3'                                         
001710        MOVE +784       TO  AP0003-OUT-SEG-01-LENGTH                      
001720        MOVE ZEROS      TO  AP0003-OUT-SEG-01-ZZ                          
001730        MOVE AP0003I-ID      TO   AP0003O-ID                              
001740        MOVE AP0003I-PW      TO   AP0003O-PW                              
001750        MOVE 'AP0003O ' TO  MOD-NAME                                      
001760        CALL 'CBLTDLI'  USING  ISRT                                       
001770                               IO-PCB                                     
001780                               AP0003O                                    
001790                               MOD-NAME                                   
001800        IF E1864-PCB-STA-CD OF IO-PCB = SPACES                            
001810           NEXT SENTENCE                                                  
001820        ELSE                                                              
001830           MOVE E1864-PCB-STA-CD OF IO-PCB                                
001840             TO ABEND-STATUS-CODE                                         
001850           MOVE '4' TO ABEND-LOCATION                                     
001860           CALL  'IMSABEND'.                                              
001870                                                                          
001880     PERFORM 0100-GET-MSG.                                                
