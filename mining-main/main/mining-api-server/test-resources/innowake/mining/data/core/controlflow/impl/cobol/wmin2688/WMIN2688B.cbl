100700 IDENTIFICATION DIVISION. 
101000 PROGRAM-ID.    NN-192.
101100 AUTHOR.        CVCVC XXXXX.
101200 DATE-COMPILED.
101300 DATE-WRITTEN.  JUNE 1991.
105200 ENVIRONMENT DIVISION.
105500 CONFIGURATION SECTION.
106200 SOURCE-COMPUTER. UNIX. 
106300 OBJECT-COMPUTER. UNIX.
106400 SPECIAL-NAMES.
106500     ENVIRONMENT-NAME IS ENV-NAME
106600     ENVIRONMENT-VALUE IS ENV-VALUE.
106700*VME
106800
106900 INPUT-OUTPUT SECTION.
107000**********************
107100 FILE-CONTROL.
109000 DATA DIVISION.
109300 FILE SECTION.
114600 WORKING-STORAGE SECTION.
132300 REPORT SECTION.
148300 PROCEDURE DIVISION. 
163900 MAIN-PROGRAM SECTION.
179800 B100-BOJ-ROUTINE.
GFS001     ACCEPT WS-TIME FROM TIME.
GFS001     PERFORM 9400-SYS-YYMMDD THRU 9400-SYS-YYMMDD-EXIT.
GFS001     DISPLAY "SYS DATE " WS-SYS-CCYYMMDD. 
GFS001     MOVE WS-SYS-MMDDYY          TO WS-GREGORIAN-DATE.
186000 B100-BOJ-ROUTINE-EXIT.
186100     EXIT.
186200/
102600                                                                          
102700 9400-SYS-YYMMDD.                                                         
102800                                                                          
102900     ACCEPT WS-SYS-YYMMDD FROM DATE.                                      
103000                                                                          
103100     MOVE WS-SYS-CCYYMMDD-MM      TO WS-SYS-MMDDCCYY-MM                   
103200                                     WS-SYS-MMDDYY-MM                     
103300                                     WS-SYS-MMDDCCYY-SLASH-MM             
103400                                     WS-SYS-MMDDYY-SLASH-MM.              
103500                                                                          
103600     MOVE WS-SYS-CCYYMMDD-DD      TO WS-SYS-MMDDCCYY-DD                   
103700                                     WS-SYS-MMDDYY-DD                     
103800                                     WS-SYS-MMDDCCYY-SLASH-DD             
103900                                     WS-SYS-MMDDYY-SLASH-DD.              
104000                                                                          
104100     MOVE WS-SYS-CCYYMMDD-YY      TO WS-SYS-MMDDCCYY-YY                   
104200                                     WS-SYS-MMDDYY-YY                     
104300                                     WS-SYS-MMDDCCYY-SLASH-YY             
104400                                     WS-SYS-MMDDYY-SLASH-YY.              
104500                                                                          
104600     IF WS-SYS-CCYYMMDD-YY > 72                                           
104700          MOVE 19                 TO WS-SYS-CCYYMMDD-CC                   
104800                                     WS-SYS-MMDDCCYY-CC                   
104900                                     WS-SYS-MMDDCCYY-SLASH-CC             
105000     ELSE                                                                 
105100          MOVE 20                 TO WS-SYS-CCYYMMDD-CC                   
105200                                     WS-SYS-MMDDCCYY-CC                   
105300                                     WS-SYS-MMDDCCYY-SLASH-CC.            
105400                                                                          
105500 9400-SYS-YYMMDD-EXIT.                                                    