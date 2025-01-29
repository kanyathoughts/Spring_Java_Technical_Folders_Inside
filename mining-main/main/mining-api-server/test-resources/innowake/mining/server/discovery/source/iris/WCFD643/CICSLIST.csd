                                                                                                                                      
 GROUP NAME: TSTCONS     (IBM PROTECTED)                                                                                              
 ----------                                                                                                                           
                                                                                                                                      
 PROGRAM(TESTTRANS)     GROUP(TSTCONS)  DESCRIPTION()          LANGUAGE(COBOL)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(MISSPGM01)     GROUP(TSTCONS)  DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(TRNS)      GROUP(TSTCONS)  DESCRIPTION()          PROGRAM(TESTTRANS)       TWASIZE(0)                           
                                        PROFILE(DFHCICST)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
 
                                        PRIORITY(1)            TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(10)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)    
                                                                                                                
 TRANSACTION(MISS)      GROUP(TSTCONS)  DESCRIPTION()          PROGRAM(MISSPGM01)       TWASIZE(0)                           
                                        PROFILE(DFHCICST)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
 
                                        PRIORITY(1)            TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(10)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                                                                            
 
