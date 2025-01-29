******** A
       IDENTIFICATION DIVISION.
       PROGRAM-ID. A.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DCL.
           10 PK-NUM         PIC S9(9) COMP.
           10 FIRST-NAME     PIC X(40).
           10 LAST-NAME      PIC X(40).
       01  EMPT.
           10 PK-NUM-B       PIC S9(9) COMP.
           10 FIRST-NAME-B   PIC X(40).
           10 LAST-NAME-B    PIC X(40).
        01  W-SUBJECT-ID           PIC S9(9) COMP.
        01  W-USER-ID              PIC X(10).
        01  W-PK-NUM               PIC S9(9) COMP.
        01  W-PK-NUM-B             PIC S9(9) COMP.

       EXEC SQL                                                     
              DECLARE CRFULL CURSOR FOR                                 
                 SELECT                                                 
                    PK_NUM, FIRST_NAME, LAST_NAME
                 FROM                                                   
                    TABLE1                                    
                 WHERE                                                  
                   PK_NUM = :W-PK-NUM           
                 ORDER BY FIRST_NAME                                    
       END-EXEC.
       EXEC SQL                                                     
              DECLARE CREMPT CURSOR FOR                                 
                 SELECT                                                 
                    PK_NUM, FIRST_NAME, LAST_NAME
                 FROM                                                   
                    TABLE2                                    
                 WHERE                                                  
                   PK_NUM = :W-PK-NUM-B           
                 ORDER BY FIRST_NAME                                    
       END-EXEC.

       LINKAGE SECTION.
       PROCEDURE DIVISION.
       0000-INITIALIZE.
           EXEC SQL                                                  
                    OPEN CRFULL                                      
           END-EXEC
           EXEC SQL                                                  
                    OPEN CREMPT                                      
           END-EXEC

           EXEC SQL                                                    
                  FETCH CRFULL                                         
                  INTO :DCL.PK-NUM,                     
                       :DCL.FIRST-NAME,             
                       :DCL.LAST-NAME
           END-EXEC

           EXEC SQL                                                    
                  FETCH CREMPT                                         
                  INTO :EMPT.PK-NUM-B,                     
                       :EMPT.FIRST-NAME-B,             
                       :EMPT.LAST-NAME-B
           END-EXEC


           EXEC SQL                                                  
                    CLOSE CRFULL                                      
           END-EXEC
           
           EXEC SQL                                                  
                    CLOSE CREMPT                                      
           END-EXEC
