                                                                                                                                      
 GROUP NAME: DFHBMS      (IBM PROTECTED)                                                                                              
 ----------                                                                                                                           
                                                                                                                                      
 PROGRAM(DFHTPQ)        GROUP(DFHBMS)                                                                                      
                                        DESCRIPTION(BMS terminal page cleanup program)                                                
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:41)                                                                 
                                        CHANGETIME(16/06/23 11:44:41)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHTPR)        GROUP(DFHBMS)                                                                                      
                                        DESCRIPTION(BMS terminal page retrieval program)                                              
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHTPS)        GROUP(DFHBMS)                                                                                      
                                        DESCRIPTION(BMS terminal page scheduling)                                                     
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
 
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CSPG)      GROUP(DFHBMS)   DESCRIPTION()          PROGRAM(DFHTPR)        TWASIZE(1124)                        
                                        PROFILE(DFHCICSP)      PARTITIONSET(KEEP)     STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(ENABLED)      ISOLATE(YES)                                    
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CSPQ)      GROUP(DFHBMS)   DESCRIPTION()          PROGRAM(DFHTPQ)        TWASIZE(0)                           
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
 
                                                                                                                                      
 TRANSACTION(CSPS)      GROUP(DFHBMS)   DESCRIPTION()          PROGRAM(DFHTPS)        TWASIZE(0)                           
                                        PROFILE(DFHCICSA)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(0)             SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 ************************************************************************************************************************             
                                                                                                                                      
 GROUP NAME: DFHCONS     (IBM PROTECTED)                                                                                              
 ----------                                                                                                                           
                                                                                                                                      
 PROGRAM(DFHCWTO)       GROUP(DFHCONS)  DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
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
                                                                                                                                      
 TRANSACTION(CWTO)      GROUP(DFHCONS)  DESCRIPTION()          PROGRAM(DFHCWTO)       TWASIZE(0)                           
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
                                                                                                                                      
 ************************************************************************************************************************             
                                                                                                                                      
 GROUP NAME: DFHEDF      (IBM PROTECTED)                                                                                              
 ----------                                                                                                                           
                                                                                                                                      
 MAPSET(DFHEDFM)        GROUP(DFHEDF)   DESCRIPTION()          RESIDENT(NO)           USAGE(NORMAL)                        
                                        USELPACOPY(NO)         STATUS(ENABLED)                                                        
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHDBMS)       GROUP(DFHEDF)   DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
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
                                                                                                                                      
 PROGRAM(DFHDBTI)       GROUP(DFHEDF)                                                                                      
                                        DESCRIPTION(CICS EXEC DL/I LD TABLE)                                                          
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(BELOW)    EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
 
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHEDFBR)      GROUP(DFHEDF)                                                                                      
                                        DESCRIPTION(Temporary storage browse transaction)                                             
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHEDFD)       GROUP(DFHEDF)                                                                                      
                                        DESCRIPTION(EDF display program)                                                              
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHEDFE)       GROUP(DFHEDF)                                                                                      
                                        DESCRIPTION(EDF attach error handler)                                                         
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 
 PROGRAM(DFHEDFP)       GROUP(DFHEDF)                                                                                      
                                        DESCRIPTION(EDF control program)                                                              
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHEDFR)       GROUP(DFHEDF)                                                                                      
                                        DESCRIPTION(EDF response table)                                                               
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHEDFX)       GROUP(DFHEDF)                                                                                      
                                        DESCRIPTION(EDF task switch program)                                                          
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(THREADSAFE)                                                                       
                                        API(CICSAPI)                                                                                  
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHEIGDS)      GROUP(DFHEDF)                                                                                      
                                        DESCRIPTION(Translator table (GDS commands))                                                  
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
 
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHEITAB)      GROUP(DFHEDF)                                                                                      
                                        DESCRIPTION(Translator table (basic commands))                                                
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHSMTAB)      GROUP(DFHEDF)                                                                                      
                                        DESCRIPTION(Translator table (CPSM commands))                                                 
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANCLASS(DFHEDFTC)    GROUP(DFHEDF)   DESCRIPTION()                                                                      
                        CLASS-LIMITS                                                                                                  
                                        MAXACTIVE(10)          PURGETHRESH(NO)                                                        
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CEBR)      GROUP(DFHEDF)   DESCRIPTION()          PROGRAM(DFHEDFBR)      TWASIZE(0)                           
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(YES)            CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CEDF)      GROUP(DFHEDF)   DESCRIPTION()          PROGRAM(DFHEDFP)       TWASIZE(0)                           
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(NO)             DUMP(YES)              TRACE(NO)                                       
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(YES)            CMDSEC(YES)                                                            
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CEDX)      GROUP(DFHEDF)   DESCRIPTION()          PROGRAM(DFHEDFP)       TWASIZE(0)                           
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(NO)             DUMP(YES)              TRACE(NO)                                       
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(YES)            CMDSEC(YES)                                                            
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 ************************************************************************************************************************             
                                                                                                                                      
 GROUP NAME: DFHFE       (IBM PROTECTED)                                                                                              
 ----------                                                                                                                           
                                                                                                                                      
 PROGRAM(DFHFEP)        GROUP(DFHFE)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
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
                                                                                                                                      
 PROGRAM(DFHTRAP)       GROUP(DFHFE)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
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
                                                                                                                                      
 
 TRANSACTION(CSFE)      GROUP(DFHFE)    DESCRIPTION()          PROGRAM(DFHFEP)        TWASIZE(0)                           
                                        PROFILE(DFHCICST)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
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
                                                                                                                                      
 ************************************************************************************************************************             
                                                                                                                                      
 GROUP NAME: DFHHARDC    (IBM PROTECTED)                                                                                              
 ----------                                                                                                                           
                                                                                                                                      
 PROGRAM(DFHP3270)      GROUP(DFHHARDC) DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
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
                                                                                                                                      
 TRANSACTION(CSPP)      GROUP(DFHHARDC) DESCRIPTION()          PROGRAM(DFHP3270)      TWASIZE(0)                           
                                        PROFILE(DFHCICST)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
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
                                                                                                                                      
 ************************************************************************************************************************             
                                                                                                                                      
 GROUP NAME: DFHINQUI    (IBM PROTECTED)                                                                                              
 ----------                                                                                                                           
                                                                                                                                      
 PROGRAM(DFHEITBS)      GROUP(DFHINQUI)                                                                                    
                                        DESCRIPTION(Translator table (special commands))                                              
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 ************************************************************************************************************************             
                                                                                                                                      
 GROUP NAME: DFHINTER    (IBM PROTECTED)                                                                                              
 ----------                                                                                                                           
                                                                                                                                      
 PROGRAM(DFHECID)       GROUP(DFHINTER)                                                                                    
                                        DESCRIPTION(CECI service program)                                                             
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
 
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHECIP)       GROUP(DFHINTER)                                                                                    
                                        DESCRIPTION(Command interpreter program (CECI))                                               
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHECSP)       GROUP(DFHINTER)                                                                                    
                                        DESCRIPTION(Command syntax check program (CECS))                                              
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CECI)      GROUP(DFHINTER) DESCRIPTION()          PROGRAM(DFHECIP)       TWASIZE(0)                           
                                        PROFILE(DFHCICST)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        RESSEC(YES)            CMDSEC(YES)                                                            
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CECS)      GROUP(DFHINTER) DESCRIPTION()          PROGRAM(DFHECSP)       TWASIZE(0)                           
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
                                                                                                                                      
 ************************************************************************************************************************             
                                                                                                                                      
 GROUP NAME: DFHISC      (IBM PROTECTED)                                                                                              
 ----------                                                                                                                           
                                                                                                                                      
 PROFILE(DFHCICSF)      GROUP(DFHISC)   DESCRIPTION()          SCRNSIZE(DEFAULT)      UCTRAN(NO)                           
                                        MODENAME()             FACILITYLIKE()         PRINTERCOMP(NO)                                 
                        JOURNALLING                                                                                                   
                                        JOURNAL(NO)            MSGJRNL(NO)                                                            
                        PROTECTION                                                                                                    
                                        MSGINTEG(NO)           ONEWTE(NO)             CHAINCONTROL(NO)                                
                        PROTOCOLS                                                                                                     
                                        DVSUPRT(ALL)           INBFMH(ALL)            RAQ(NO)                                         
                                        LOGREC(NO)                                                                                    
                        RECOVERY                                                                                                      
                                        NEPCLASS(0)            RTIMOUT(NO)                                                            
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 
 PROFILE(DFHCICSR)      GROUP(DFHISC)   DESCRIPTION()          SCRNSIZE(DEFAULT)      UCTRAN(NO)                           
                                        MODENAME()             FACILITYLIKE()         PRINTERCOMP(NO)                                 
                        JOURNALLING                                                                                                   
                                        JOURNAL(NO)            MSGJRNL(NO)                                                            
                        PROTECTION                                                                                                    
                                        MSGINTEG(NO)           ONEWTE(NO)             CHAINCONTROL(NO)                                
                        PROTOCOLS                                                                                                     
                                        DVSUPRT(ALL)           INBFMH(ALL)            RAQ(NO)                                         
                                        LOGREC(NO)                                                                                    
                        RECOVERY                                                                                                      
                                        NEPCLASS(0)            RTIMOUT(NO)                                                            
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROFILE(DFHCICSS)      GROUP(DFHISC)   DESCRIPTION()          SCRNSIZE(DEFAULT)      UCTRAN(NO)                           
                                        MODENAME()             FACILITYLIKE()         PRINTERCOMP(NO)                                 
                        JOURNALLING                                                                                                   
                                        JOURNAL(NO)            MSGJRNL(NO)                                                            
                        PROTECTION                                                                                                    
                                        MSGINTEG(NO)           ONEWTE(NO)             CHAINCONTROL(NO)                                
                        PROTOCOLS                                                                                                     
                                        DVSUPRT(ALL)           INBFMH(ALL)            RAQ(NO)                                         
                                        LOGREC(NO)                                                                                    
                        RECOVERY                                                                                                      
                                        NEPCLASS(0)            RTIMOUT(NO)                                                            
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHCCNV)       GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(CICS OS/2 Conversion program)                                                     
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(YES)                                   
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(THREADSAFE)                                                                       
                                        API(CICSAPI)                                                                                  
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHCHS)        GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(LU2 Remote server for CICS VM/CMS and CICS OS/2)                                  
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
 
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHCLS3)       GROUP(DFHISC)   DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
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
                                                                                                                                      
 PROGRAM(DFHCLS4)       GROUP(DFHISC)   DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
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
                                                                                                                                      
 PROGRAM(DFHCLS5)       GROUP(DFHISC)   DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
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
                                                                                                                                      
 
 PROGRAM(DFHCNV)        GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(CICS OS/2 Conversion table)                                                       
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(BELOW)    EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHCRNP)       GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(Interregion connection manager)                                                   
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHCRQ)        GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(ATI purge program)                                                                
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHCRR)        GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(Interregion session recovery program)                                             
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
 
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHCRS)        GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(Remote scheduler program)                                                         
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHCRSP)       GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(CICS IRC startup module)                                                          
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHCRT)        GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(Transaction routing relay program for APPC devices)                               
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
 
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHDFST)       GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(CICS non-terminal deferred dynamic start Program)                                 
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(YES)                                   
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHDSRP)       GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(Default Distributed Dynamic Routing Program)                                      
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHDYP)        GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(Default Dynamic Transaction Routing program)                                      
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHLUP)        GROUP(DFHISC)   DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
 
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
                                                                                                                                      
 PROGRAM(DFHMIRS)       GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(CICS Function Shipping Mirror Program)                                            
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(YES)              DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(THREADSAFE)                                                                       
                                        API(CICSAPI)                                                                                  
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHMXP)        GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(Local queuing shipper)                                                            
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHRTC)        GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(CRTE cancel command processor)                                                    
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
 
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHRTE)        GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(Transaction routing program)                                                      
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHSHRRP)      GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(CICS Sheduler Services Request Receiving Program)                                 
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHSHRSP)      GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(CICS Sheduler Services Request Sending Program)                                   
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHUCNV)       GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(CICS OS/2 User conversion program)                                                
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(YES)                                   
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(THREADSAFE)                                                                       
                                        API(CICSAPI)                                                                                  
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHZLS1)       GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(LU6.2 CNOS request transaction program)                                           
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CDFS)      GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(Processes deferred dynamic start requests)                                        
                                        PROGRAM(DFHDFST)       TWASIZE(0)             PROFILE(DFHCICSA)                               
                                        PARTITIONSET()         STATUS(ENABLED)        TASKDATALOC(ANY)                                
                                        TASKDATAKEY(CICS)      STORAGECLEAR(NO)       RUNAWAY(SYSTEM)                                 
                                        SHUTDOWN(DISABLED)     ISOLATE(YES)           BREXIT()                                        
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(1)            TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
 
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CEHP)      GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(CICS OS/2 LU2 Remote server)                                                      
                                        PROGRAM(DFHCHS)        TWASIZE(0)             PROFILE(DFHCICST)                               
                                        PARTITIONSET()         STATUS(ENABLED)        TASKDATALOC(ANY)                                
                                        TASKDATAKEY(CICS)      STORAGECLEAR(NO)       RUNAWAY(SYSTEM)                                 
                                        SHUTDOWN(DISABLED)     ISOLATE(YES)           BREXIT()                                        
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(1)            TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID(43454850)                               
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(YES)            CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CEHS)      GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(CICS/VM LU2 Remote server)                                                        
                                        PROGRAM(DFHCHS)        TWASIZE(0)             PROFILE(DFHCICST)                               
                                        PARTITIONSET()         STATUS(ENABLED)        TASKDATALOC(ANY)                                
                                        TASKDATAKEY(CICS)      STORAGECLEAR(NO)       RUNAWAY(SYSTEM)                                 
                                        SHUTDOWN(DISABLED)     ISOLATE(YES)           BREXIT()                                        
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(1)            TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
 
                        SECURITY                                                                                                      
                                        RESSEC(YES)            CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CLQ2)      GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(OUTBOUND RESYNC FOR APPC AND MRO)                                                 
                                        PROGRAM(DFHLUP)        TWASIZE(0)             PROFILE(DFHCICSA)                               
                                        PARTITIONSET()         STATUS(ENABLED)        TASKDATALOC(ANY)                                
                                        TASKDATAKEY(CICS)      STORAGECLEAR(NO)       RUNAWAY(SYSTEM)                                 
                                        SHUTDOWN(ENABLED)      ISOLATE(YES)           BREXIT()                                        
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(254)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
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
                                                                                                                                      
 TRANSACTION(CLR2)      GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(INBOUND RESYNC FOR MRO)                                                           
                                        PROGRAM(DFHLUP)        TWASIZE(0)             PROFILE(DFHCICSA)                               
                                        PARTITIONSET()         STATUS(ENABLED)        TASKDATALOC(ANY)                                
                                        TASKDATAKEY(CICS)      STORAGECLEAR(NO)       RUNAWAY(SYSTEM)                                 
                                        SHUTDOWN(ENABLED)      ISOLATE(YES)           BREXIT()                                        
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(254)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
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
                                                                                                                                      
 TRANSACTION(CLS1)      GROUP(DFHISC)   DESCRIPTION()          PROGRAM(DFHZLS1)       TWASIZE(0)                           
                                        PROFILE(DFHCICSA)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(ENABLED)      ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(254)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID(06F10000)                               
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
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
                                                                                                                                      
 TRANSACTION(CLS2)      GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(OUTBOUND XLN + INBOUND RESYNC/XLN FOR APPC)                                       
                                        PROGRAM(DFHLUP)        TWASIZE(0)             PROFILE(DFHCICSA)                               
                                        PARTITIONSET()         STATUS(ENABLED)        TASKDATALOC(ANY)                                
                                        TASKDATAKEY(CICS)      STORAGECLEAR(NO)       RUNAWAY(SYSTEM)                                 
                                        SHUTDOWN(ENABLED)      ISOLATE(YES)           BREXIT()                                        
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(254)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID(06F20000)                               
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
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
                                                                                                                                      
 TRANSACTION(CLS3)      GROUP(DFHISC)   DESCRIPTION()          PROGRAM(DFHCLS3)       TWASIZE(0)                           
                                        PROFILE(DFHCICSA)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(254)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID(06F3F0F0)                               
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
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
                                                                                                                                      
 TRANSACTION(CLS4)      GROUP(DFHISC)   DESCRIPTION()          PROGRAM(DFHCLS4)       TWASIZE(0)                           
                                        PROFILE(DFHCICSA)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(254)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID(06F3F0F1)                               
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
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
                                                                                                                                      
 TRANSACTION(CMPX)      GROUP(DFHISC)   DESCRIPTION()          PROGRAM(DFHMXP)        TWASIZE(0)                           
                                        PROFILE(DFHCICSA)      PARTITIONSET()         STATUS(ENABLED)                                 
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CPMI)      GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(CICS OS/2 LU62 Mirror)                                                            
                                        PROGRAM(DFHMIRS)       TWASIZE(0)             PROFILE(DFHCICSA)                               
                                        PARTITIONSET()         STATUS(ENABLED)        TASKDATALOC(ANY)                                
                                        TASKDATAKEY(USER)      STORAGECLEAR(NO)       RUNAWAY(SYSTEM)                                 
                                        SHUTDOWN(DISABLED)     ISOLATE(YES)           BREXIT()                                        
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(1)            TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(YES)          OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(YES)            CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 
 TRANSACTION(CQPI)      GROUP(DFHISC)   DESCRIPTION()          PROGRAM(DFHCLS5)       TWASIZE(0)                           
                                        PROFILE(DFHCICSA)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(ENABLED)      ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(254)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID(06F2F0F2)                               
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
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
                                                                                                                                      
 TRANSACTION(CQPO)      GROUP(DFHISC)   DESCRIPTION()          PROGRAM(DFHCLS5)       TWASIZE(0)                           
                                        PROFILE(DFHCICSA)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(ENABLED)      ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(254)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
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
                                                                                                                                      
 TRANSACTION(CRSQ)      GROUP(DFHISC)   DESCRIPTION()          PROGRAM(DFHCRQ)        TWASIZE(0)                           
                                        PROFILE(DFHCICSA)      PARTITIONSET()         STATUS(ENABLED)                                 
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
                                        DTIMOUT(100)           RESTART(NO)            SPURGE(YES)                                     
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
                                                                                                                                      
 TRANSACTION(CRSR)      GROUP(DFHISC)   DESCRIPTION()          PROGRAM(DFHCRS)        TWASIZE(0)                           
                                        PROFILE(DFHCICSA)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(ENABLED)      ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(253)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CRTE)      GROUP(DFHISC)   DESCRIPTION()          PROGRAM(DFHRTE)        TWASIZE(0)                           
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(YES)          OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CRTX)      GROUP(DFHISC)   DESCRIPTION()          PROGRAM(########)      TWASIZE(0)                           
                                        PROFILE(DFHCICST)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(YES)           ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME(CRTX)       TRPROF(DFHCICSS)       LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(1)            TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CSHR)      GROUP(DFHISC)   DESCRIPTION()          PROGRAM(DFHMIRS)       TWASIZE(0)                           
                                        PROFILE(DFHCICSA)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(YES)          OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(YES)            CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CSMI)      GROUP(DFHISC)   DESCRIPTION()          PROGRAM(DFHMIRS)       TWASIZE(0)                           
                                        PROFILE(DFHCICSA)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(YES)          OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(YES)            CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CSM1)      GROUP(DFHISC)   DESCRIPTION()          PROGRAM(DFHMIRS)       TWASIZE(0)                           
                                        PROFILE(DFHCICSA)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(1)            TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID(01000000)                               
 
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(YES)          OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(YES)            CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CSM2)      GROUP(DFHISC)   DESCRIPTION()          PROGRAM(DFHMIRS)       TWASIZE(0)                           
                                        PROFILE(DFHCICSA)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(1)            TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID(02000000)                               
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(YES)          OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(YES)            CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CSM3)      GROUP(DFHISC)   DESCRIPTION()          PROGRAM(DFHMIRS)       TWASIZE(0)                           
                                        PROFILE(DFHCICSA)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(1)            TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID(03000000)                               
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
 
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(YES)          OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(YES)            CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CSM5)      GROUP(DFHISC)   DESCRIPTION()          PROGRAM(DFHMIRS)       TWASIZE(0)                           
                                        PROFILE(DFHCICSA)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(1)            TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID(05000000)                               
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(YES)          OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(YES)            CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CSNC)      GROUP(DFHISC)   DESCRIPTION()          PROGRAM(DFHCRNP)       TWASIZE(0)                           
                                        PROFILE(DFHCICST)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
 
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CSSF)      GROUP(DFHISC)   DESCRIPTION()          PROGRAM(DFHRTC)        TWASIZE(0)                           
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CVMI)      GROUP(DFHISC)                                                                                      
                                        DESCRIPTION(CICS VM LU62 Mirror)                                                              
                                        PROGRAM(DFHMIRS)       TWASIZE(0)             PROFILE(DFHCICSA)                               
                                        PARTITIONSET()         STATUS(ENABLED)        TASKDATALOC(ANY)                                
                                        TASKDATAKEY(USER)      STORAGECLEAR(NO)       RUNAWAY(SYSTEM)                                 
                                        SHUTDOWN(DISABLED)     ISOLATE(YES)           BREXIT()                                        
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(1)            TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(YES)          OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
 
                                        RESSEC(YES)            CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CXRT)      GROUP(DFHISC)   DESCRIPTION()          PROGRAM(DFHCRT)        TWASIZE(0)                           
                                        PROFILE(DFHCICSR)      PARTITIONSET()         STATUS(ENABLED)                                 
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(YES)            CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:42)                                                                 
                                        CHANGETIME(16/06/23 11:44:42)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 ************************************************************************************************************************             
                                                                                                                                      
 GROUP NAME: DFHMISC     (IBM PROTECTED)                                                                                              
 ----------                                                                                                                           
                                                                                                                                      
 PROGRAM(DFHLEINI)      GROUP(DFHMISC)                                                                                     
                                        DESCRIPTION(Discards unwanted LE event handler definitions)                                   
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(17/05/23 10:33:00)                                                                 
                                        CHANGETIME(17/05/23 10:33:00)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHLETRU)      GROUP(DFHMISC)                                                                                     
 
                                        DESCRIPTION(TRUE for native QSAM access for LE and debug tool)                                
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(YES)                                   
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(THREADSAFE)                                                                       
                                        API(CICSAPI)                                                                                  
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHPEP)        GROUP(DFHMISC)  DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(YES)          USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHREST)       GROUP(DFHMISC)  DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(YES)          USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 ************************************************************************************************************************             
                                                                                                                                      
 GROUP NAME: DFHMSWIT    (IBM PROTECTED)                                                                                              
 ----------                                                                                                                           
                                                                                                                                      
 PROGRAM(DFHMSP)        GROUP(DFHMSWIT)                                                                                    
                                        DESCRIPTION(Message switching program)                                                        
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
 
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CMSG)      GROUP(DFHMSWIT) DESCRIPTION()          PROGRAM(DFHMSP)        TWASIZE(528)                         
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
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 ************************************************************************************************************************             
                                                                                                                                      
 GROUP NAME: DFHOPCLS    (IBM PROTECTED)                                                                                              
 ----------                                                                                                                           
                                                                                                                                      
 PROGRAM(DFHFCU)        GROUP(DFHOPCLS)                                                                                    
                                        DESCRIPTION(File open utility program)                                                        
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(BELOW)    EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
 
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CSFU)      GROUP(DFHOPCLS) DESCRIPTION()          PROGRAM(DFHFCU)        TWASIZE(0)                           
                                        PROFILE(DFHCICST)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 ************************************************************************************************************************             
                                                                                                                                      
 GROUP NAME: DFHOPER     (IBM PROTECTED)                                                                                              
 ----------                                                                                                                           
                                                                                                                                      
 MAPSET(DFHCMNH)        GROUP(DFHOPER)                                                                                     
                                        DESCRIPTION(CICS Monitoring Control Mapset)                                                   
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)                                                                               
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 MAPSET(DFHCMNM)        GROUP(DFHOPER)                                                                                     
                                        DESCRIPTION(CICS Monitoring Control Mapset)                                                   
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)                                                                               
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 MAPSET(DFHCTRH)        GROUP(DFHOPER)  DESCRIPTION()          RESIDENT(NO)           USAGE(NORMAL)                        
 
                                        USELPACOPY(NO)         STATUS(ENABLED)                                                        
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 MAPSET(DFHCTRM)        GROUP(DFHOPER)  DESCRIPTION()          RESIDENT(NO)           USAGE(NORMAL)                        
                                        USELPACOPY(NO)         STATUS(ENABLED)                                                        
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 MAPSET(DFHSO1M)        GROUP(DFHOPER)                                                                                     
                                        DESCRIPTION(CICS Certificate Revocation List Mapset)                                          
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)                                                                               
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHCEMNA)      GROUP(DFHOPER)                                                                                     
                                        DESCRIPTION(CICS Monitoring Control Program)                                                  
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHCEMNB)      GROUP(DFHOPER)                                                                                     
                                        DESCRIPTION(CICS Monitoring Control Program)                                                  
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHCEMNC)      GROUP(DFHOPER)                                                                                     
                                        DESCRIPTION(CICS Monitoring Control Program)                                                  
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHCEMND)      GROUP(DFHOPER)                                                                                     
                                        DESCRIPTION(CICS Monitoring Control Program)                                                  
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHCETRA)      GROUP(DFHOPER)  DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHCETRB)      GROUP(DFHOPER)  DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
 
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHCETRC)      GROUP(DFHOPER)  DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHCETRD)      GROUP(DFHOPER)  DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHCETRF)      GROUP(DFHOPER)  DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHECBAM)      GROUP(DFHOPER)  DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
 
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHEITMT)      GROUP(DFHOPER)                                                                                     
                                        DESCRIPTION(Command language table for CEMT)                                                  
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(YES)                                   
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHEITOT)      GROUP(DFHOPER)                                                                                     
                                        DESCRIPTION(Command language table for CEOT)                                                  
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHEITST)      GROUP(DFHOPER)                                                                                     
                                        DESCRIPTION(Command language table for CEST)                                                  
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
 
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHEMTA)       GROUP(DFHOPER)                                                                                     
                                        DESCRIPTION(Programmable interface to Master terminal program)                                
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHEMTD)       GROUP(DFHOPER)                                                                                     
                                        DESCRIPTION(Master terminal service program)                                                  
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(YES)                                   
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHEMTP)       GROUP(DFHOPER)                                                                                     
                                        DESCRIPTION(Master terminal program)                                                          
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(YES)                                   
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 
 PROGRAM(DFHEOTP)       GROUP(DFHOPER)                                                                                     
                                        DESCRIPTION(CEOT service program)                                                             
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHESTP)       GROUP(DFHOPER)                                                                                     
                                        DESCRIPTION(CEST service program)                                                             
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHLDMAP)      GROUP(DFHOPER)                                                                                     
                                        DESCRIPTION(CICS Loader Map Control Program)                                                  
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHLDMHF)      GROUP(DFHOPER)                                                                                     
                                        DESCRIPTION(CICS Loader Map USS Program)                                                      
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(THREADSAFE)                                                                       
 
                                        API(OPENAPI)                                                                                  
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHLDMHS)      GROUP(DFHOPER)                                                                                     
                                        DESCRIPTION(CICS Loader Map Spool Program)                                                    
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHSOCRL)      GROUP(DFHOPER)                                                                                     
                                        DESCRIPTION(CICS Certificate Revocation List Program)                                         
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(YES)              DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CBAM)      GROUP(DFHOPER)  DESCRIPTION()          PROGRAM(DFHECBAM)      TWASIZE(0)                           
                                        PROFILE(DFHCICST)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
 
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CCRL)      GROUP(DFHOPER)                                                                                     
                                        DESCRIPTION(CICS Certificate Revocation List Transaction)                                     
                                        PROGRAM(DFHSOCRL)      TWASIZE(0)             PROFILE(DFHCICST)                               
                                        PARTITIONSET()         STATUS(ENABLED)        TASKDATALOC(ANY)                                
                                        TASKDATAKEY(CICS)      STORAGECLEAR(NO)       RUNAWAY(SYSTEM)                                 
                                        SHUTDOWN(ENABLED)      ISOLATE(YES)           BREXIT()                                        
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(1)            TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(YES)            CMDSEC(YES)                                                            
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CEMN)      GROUP(DFHOPER)                                                                                     
                                        DESCRIPTION(CICS Monitoring Control Transaction)                                              
                                        PROGRAM(DFHCEMNA)      TWASIZE(0)             PROFILE(DFHCICST)                               
                                        PARTITIONSET()         STATUS(ENABLED)        TASKDATALOC(ANY)                                
                                        TASKDATAKEY(CICS)      STORAGECLEAR(NO)       RUNAWAY(SYSTEM)                                 
                                        SHUTDOWN(DISABLED)     ISOLATE(YES)           BREXIT()                                        
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
 
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CEMT)      GROUP(DFHOPER)  DESCRIPTION()          PROGRAM(DFHEMTP)       TWASIZE(0)                           
                                        PROFILE(DFHCICST)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(ENABLED)      ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(YES)            CMDSEC(YES)                                                            
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CEOT)      GROUP(DFHOPER)  DESCRIPTION()          PROGRAM(DFHEOTP)       TWASIZE(0)                           
                                        PROFILE(DFHCICST)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
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
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CEST)      GROUP(DFHOPER)  DESCRIPTION()          PROGRAM(DFHESTP)       TWASIZE(0)                           
                                        PROFILE(DFHCICST)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
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
                                        RESSEC(YES)            CMDSEC(YES)                                                            
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CETR)      GROUP(DFHOPER)  DESCRIPTION()          PROGRAM(DFHCETRA)      TWASIZE(0)                           
                                        PROFILE(DFHCICST)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
 
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CLDM)      GROUP(DFHOPER)                                                                                     
                                        DESCRIPTION(CICS Loader Map Transaction)                                                      
                                        PROGRAM(DFHLDMAP)      TWASIZE(0)             PROFILE(DFHCICST)                               
                                        PARTITIONSET()         STATUS(ENABLED)        TASKDATALOC(ANY)                                
                                        TASKDATAKEY(CICS)      STORAGECLEAR(NO)       RUNAWAY(SYSTEM)                                 
                                        SHUTDOWN(DISABLED)     ISOLATE(YES)           BREXIT()                                        
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(1)            TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 ************************************************************************************************************************             
                                                                                                                                      
 GROUP NAME: DFHRMI      (IBM PROTECTED)                                                                                              
 ----------                                                                                                                           
                                                                                                                                      
 PROGRAM(DFHRMSY)       GROUP(DFHRMI)                                                                                      
                                        DESCRIPTION(Resource manager resync program)                                                  
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CRSY)      GROUP(DFHRMI)   DESCRIPTION()          PROGRAM(DFHRMSY)       TWASIZE(0)                           
                                        PROFILE(DFHCICST)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 ************************************************************************************************************************             
                                                                                                                                      
 GROUP NAME: DFHRSEND    (IBM PROTECTED)                                                                                              
 ----------                                                                                                                           
                                                                                                                                      
 PROGRAM(DFHZRSP)       GROUP(DFHRSEND)                                                                                    
                                        DESCRIPTION(Resync send program)                                                              
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CSRS)      GROUP(DFHRSEND) DESCRIPTION()          PROGRAM(DFHZRSP)       TWASIZE(0)                           
                                        PROFILE(DFHCICSV)      PARTITIONSET()         STATUS(ENABLED)                                 
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 ************************************************************************************************************************             
                                                                                                                                      
 GROUP NAME: DFHSIGN     (IBM PROTECTED)                                                                                              
 ----------                                                                                                                           
                                                                                                                                      
 MAPSET(DFHSNLE)        GROUP(DFHSIGN)  DESCRIPTION()          RESIDENT(NO)           USAGE(NORMAL)                        
                                        USELPACOPY(NO)         STATUS(ENABLED)                                                        
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 MAPSET(DFHSNPE)        GROUP(DFHSIGN)  DESCRIPTION()          RESIDENT(NO)           USAGE(NORMAL)                        
                                        USELPACOPY(NO)         STATUS(ENABLED)                                                        
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 MAPSET(DFHSNSE)        GROUP(DFHSIGN)  DESCRIPTION()          RESIDENT(NO)           USAGE(NORMAL)                        
                                        USELPACOPY(NO)         STATUS(ENABLED)                                                        
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHCEGN)       GROUP(DFHSIGN)                                                                                     
                                        DESCRIPTION(Goodnight transaction stub)                                                       
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
 
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHCESC)       GROUP(DFHSIGN)  DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHSFP)        GROUP(DFHSIGN)                                                                                     
                                        DESCRIPTION(Sign-off program)                                                                 
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHSNP)        GROUP(DFHSIGN)                                                                                     
                                        DESCRIPTION(Sign-on program)                                                                  
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CEGN)      GROUP(DFHSIGN)  DESCRIPTION()          PROGRAM(DFHCEGN)       TWASIZE(0)                           
                                        PROFILE(DFHCICST)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
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
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CESC)      GROUP(DFHSIGN)  DESCRIPTION()          PROGRAM(DFHCESC)       TWASIZE(0)                           
                                        PROFILE(DFHCICST)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(10)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CESF)      GROUP(DFHSIGN)  DESCRIPTION()          PROGRAM(DFHSFP)        TWASIZE(0)                           
 
                                        PROFILE(DFHCICST)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(ENABLED)      ISOLATE(YES)                                    
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
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CESL)      GROUP(DFHSIGN)  DESCRIPTION()          PROGRAM(DFHSNP)        TWASIZE(0)                           
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
                                        CONFDATA(YES)          OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CESN)      GROUP(DFHSIGN)  DESCRIPTION()          PROGRAM(DFHSNP)        TWASIZE(0)                           
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
                                        CONFDATA(YES)          OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 ************************************************************************************************************************             
                                                                                                                                      
 GROUP NAME: DFHSPI      (IBM PROTECTED)                                                                                              
 ----------                                                                                                                           
                                                                                                                                      
 PROGRAM(DFHAMP)        GROUP(DFHSPI)   DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHDMP)        GROUP(DFHSPI)   DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
 
                                                                                                                                      
 PROGRAM(DFHEDAD)       GROUP(DFHSPI)                                                                                      
                                        DESCRIPTION(RDO (CEDA) service program)                                                       
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHEDAP)       GROUP(DFHSPI)                                                                                      
                                        DESCRIPTION(RDO (CEDA) program)                                                               
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHEITSP)      GROUP(DFHSPI)   DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHPUP)        GROUP(DFHSPI)   DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
 
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHTBS)        GROUP(DFHSPI)   DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHTOR)        GROUP(DFHSPI)   DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHZATA)       GROUP(DFHSPI)                                                                                      
                                        DESCRIPTION(Autoinstall program)                                                              
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(YES)              DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHZATD)       GROUP(DFHSPI)                                                                                      
                                        DESCRIPTION(Autoinstall delete program)                                                       
 
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHZATDX)      GROUP(DFHSPI)                                                                                      
                                        DESCRIPTION(User-replaceable austoinstall exit)                                               
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHZATMD)      GROUP(DFHSPI)   DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHZATMF)      GROUP(DFHSPI)   DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
 
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHZATR)       GROUP(DFHSPI)   DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHZATS)       GROUP(DFHSPI)   DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHZCQ)        GROUP(DFHSPI)   DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(YES)          USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHZCTDX)      GROUP(DFHSPI)   DESCRIPTION()          LANGUAGE(COBOL)        RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
 
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHZDTDX)      GROUP(DFHSPI)   DESCRIPTION()          LANGUAGE(C)            RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHZPTDX)      GROUP(DFHSPI)   DESCRIPTION()          LANGUAGE(PLI)          RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CATA)      GROUP(DFHSPI)   DESCRIPTION()          PROGRAM(DFHZATA)       TWASIZE(0)                           
                                        PROFILE(DFHCICSV)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(100)           RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CATD)      GROUP(DFHSPI)   DESCRIPTION()          PROGRAM(DFHZATD)       TWASIZE(0)                           
                                        PROFILE(DFHCICSV)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(100)           RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CATR)      GROUP(DFHSPI)   DESCRIPTION()          PROGRAM(DFHZATR)       TWASIZE(0)                           
                                        PROFILE(DFHCICSV)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
 
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CDTS)      GROUP(DFHSPI)   DESCRIPTION()          PROGRAM(DFHZATS)       TWASIZE(0)                           
                                        PROFILE(DFHCICSV)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(100)           RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CEDA)      GROUP(DFHSPI)   DESCRIPTION()          PROGRAM(DFHEDAP)       TWASIZE(0)                           
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
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CEDB)      GROUP(DFHSPI)   DESCRIPTION()          PROGRAM(DFHEDAP)       TWASIZE(0)                           
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
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CEDC)      GROUP(DFHSPI)   DESCRIPTION()          PROGRAM(DFHEDAP)       TWASIZE(0)                           
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
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CFTS)      GROUP(DFHSPI)   DESCRIPTION()          PROGRAM(DFHZATS)       TWASIZE(0)                           
 
                                        PROFILE(DFHCICSV)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CITS)      GROUP(DFHSPI)   DESCRIPTION()          PROGRAM(DFHZATS)       TWASIZE(0)                           
                                        PROFILE(DFHCICSV)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(100)           RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CMTS)      GROUP(DFHSPI)   DESCRIPTION()          PROGRAM(DFHZATS)       TWASIZE(0)                           
                                        PROFILE(DFHCICSV)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
 
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CRMD)      GROUP(DFHSPI)   DESCRIPTION()          PROGRAM(DFHZATMD)      TWASIZE(0)                           
                                        PROFILE(DFHCICSV)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CRMF)      GROUP(DFHSPI)   DESCRIPTION()          PROGRAM(DFHZATMF)      TWASIZE(0)                           
                                        PROFILE(DFHCICSV)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
 
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 ************************************************************************************************************************             
                                                                                                                                      
 GROUP NAME: DFHSTAND    (IBM PROTECTED)                                                                                              
 ----------                                                                                                                           
                                                                                                                                      
 MAPSET(DFHXMSG)        GROUP(DFHSTAND) DESCRIPTION()          RESIDENT(NO)           USAGE(NORMAL)                        
                                        USELPACOPY(NO)         STATUS(ENABLED)                                                        
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROFILE(DFHCICSA)      GROUP(DFHSTAND) DESCRIPTION()          SCRNSIZE(DEFAULT)      UCTRAN(NO)                           
                                        MODENAME()             FACILITYLIKE()         PRINTERCOMP(NO)                                 
                        JOURNALLING                                                                                                   
                                        JOURNAL(NO)            MSGJRNL(NO)                                                            
                        PROTECTION                                                                                                    
                                        MSGINTEG(NO)           ONEWTE(NO)             CHAINCONTROL(NO)                                
                        PROTOCOLS                                                                                                     
                                        DVSUPRT(ALL)           INBFMH(ALL)            RAQ(NO)                                         
                                        LOGREC(NO)                                                                                    
                        RECOVERY                                                                                                      
                                        NEPCLASS(0)            RTIMOUT(NO)                                                            
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROFILE(DFHCICSE)      GROUP(DFHSTAND) DESCRIPTION()          SCRNSIZE(DEFAULT)      UCTRAN(NO)                           
                                        MODENAME()             FACILITYLIKE()         PRINTERCOMP(NO)                                 
                        JOURNALLING                                                                                                   
                                        JOURNAL(NO)            MSGJRNL(NO)                                                            
                        PROTECTION                                                                                                    
                                        MSGINTEG(NO)           ONEWTE(NO)             CHAINCONTROL(NO)                                
                        PROTOCOLS                                                                                                     
 
                                        DVSUPRT(ALL)           INBFMH(NO)             RAQ(NO)                                         
                                        LOGREC(NO)                                                                                    
                        RECOVERY                                                                                                      
                                        NEPCLASS(0)            RTIMOUT(NO)                                                            
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROFILE(DFHCICSP)      GROUP(DFHSTAND) DESCRIPTION()          SCRNSIZE(DEFAULT)      UCTRAN(YES)                          
                                        MODENAME()             FACILITYLIKE()         PRINTERCOMP(NO)                                 
                        JOURNALLING                                                                                                   
                                        JOURNAL(NO)            MSGJRNL(NO)                                                            
                        PROTECTION                                                                                                    
                                        MSGINTEG(NO)           ONEWTE(NO)             CHAINCONTROL(NO)                                
                        PROTOCOLS                                                                                                     
                                        DVSUPRT(ALL)           INBFMH(NO)             RAQ(NO)                                         
                                        LOGREC(NO)                                                                                    
                        RECOVERY                                                                                                      
                                        NEPCLASS(0)            RTIMOUT(NO)                                                            
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:43)                                                                 
                                        CHANGETIME(16/06/23 11:44:43)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROFILE(DFHCICST)      GROUP(DFHSTAND) DESCRIPTION()          SCRNSIZE(DEFAULT)      UCTRAN(NO)                           
                                        MODENAME()             FACILITYLIKE()         PRINTERCOMP(NO)                                 
                        JOURNALLING                                                                                                   
                                        JOURNAL(NO)            MSGJRNL(NO)                                                            
                        PROTECTION                                                                                                    
                                        MSGINTEG(NO)           ONEWTE(NO)             CHAINCONTROL(NO)                                
                        PROTOCOLS                                                                                                     
                                        DVSUPRT(ALL)           INBFMH(NO)             RAQ(NO)                                         
                                        LOGREC(NO)                                                                                    
                        RECOVERY                                                                                                      
                                        NEPCLASS(0)            RTIMOUT(NO)                                                            
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROFILE(DFHCICSV)      GROUP(DFHSTAND) DESCRIPTION()          SCRNSIZE(DEFAULT)      UCTRAN(NO)                           
                                        MODENAME()             FACILITYLIKE()         PRINTERCOMP(NO)                                 
                        JOURNALLING                                                                                                   
                                        JOURNAL(NO)            MSGJRNL(NO)                                                            
                        PROTECTION                                                                                                    
                                        MSGINTEG(NO)           ONEWTE(NO)             CHAINCONTROL(NO)                                
                        PROTOCOLS                                                                                                     
                                        DVSUPRT(VTAM)          INBFMH(NO)             RAQ(NO)                                         
                                        LOGREC(NO)                                                                                    
                        RECOVERY                                                                                                      
                                        NEPCLASS(0)            RTIMOUT(NO)                                                            
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
 
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROFILE(DFHPPF01)      GROUP(DFHSTAND) DESCRIPTION()          SCRNSIZE(DEFAULT)      UCTRAN(NO)                           
                                        MODENAME()             FACILITYLIKE()         PRINTERCOMP(NO)                                 
                        JOURNALLING                                                                                                   
                                        JOURNAL(NO)            MSGJRNL(NO)                                                            
                        PROTECTION                                                                                                    
                                        MSGINTEG(NO)           ONEWTE(NO)             CHAINCONTROL(NO)                                
                        PROTOCOLS                                                                                                     
                                        DVSUPRT(VTAM)          INBFMH(NO)             RAQ(NO)                                         
                                        LOGREC(NO)                                                                                    
                        RECOVERY                                                                                                      
                                        NEPCLASS(0)            RTIMOUT(NO)                                                            
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROFILE(DFHPPF02)      GROUP(DFHSTAND) DESCRIPTION()          SCRNSIZE(DEFAULT)      UCTRAN(NO)                           
                                        MODENAME()             FACILITYLIKE()         PRINTERCOMP(NO)                                 
                        JOURNALLING                                                                                                   
                                        JOURNAL(NO)            MSGJRNL(NO)                                                            
                        PROTECTION                                                                                                    
                                        MSGINTEG(NO)           ONEWTE(NO)             CHAINCONTROL(NO)                                
                        PROTOCOLS                                                                                                     
                                        DVSUPRT(ALL)           INBFMH(NO)             RAQ(NO)                                         
                                        LOGREC(NO)                                                                                    
                        RECOVERY                                                                                                      
                                        NEPCLASS(0)            RTIMOUT(NO)                                                            
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHACP)        GROUP(DFHSTAND) DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(YES)          USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHCXCU)       GROUP(DFHSTAND) DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
 
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHPIITL)      GROUP(DFHSTAND)                                                                                    
                                        DESCRIPTION(Pipeline Resolution program)                                                      
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHPIPA)       GROUP(DFHSTAND)                                                                                    
                                        DESCRIPTION(Pipeline SAX Parser program for complete_pipeline)                                
                                        LANGUAGE(PLI)          RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHPIXC)       GROUP(DFHSTAND)                                                                                    
                                        DESCRIPTION(Pipeline SAX Parser program for WSAT)                                             
                                        LANGUAGE(PLI)          RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
 
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHPSIP)       GROUP(DFHSTAND) DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHQRY)        GROUP(DFHSTAND)                                                                                    
                                        DESCRIPTION(Query transaction)                                                                
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHSTP)        GROUP(DFHSTAND) DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(BELOW)                             
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHTACP)       GROUP(DFHSTAND) DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(YES)          USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(BELOW)                             
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
 
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHTEP)        GROUP(DFHSTAND) DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(YES)          USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHTEPT)       GROUP(DFHSTAND) DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(YES)          USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHTFP)        GROUP(DFHSTAND) DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(YES)          USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHZXCU)       GROUP(DFHSTAND) DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
 
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHZXRE)       GROUP(DFHSTAND) DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHZXST)       GROUP(DFHSTAND) DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CPIR)      GROUP(DFHSTAND)                                                                                    
                                        DESCRIPTION(Pipeline Resolution Transaction)                                                  
                                        PROGRAM(DFHPIITL)      TWASIZE(0)             PROFILE(DFHCICST)                               
                                        PARTITIONSET()         STATUS(ENABLED)        TASKDATALOC(ANY)                                
                                        TASKDATAKEY(CICS)      STORAGECLEAR(NO)       RUNAWAY(0)                                      
                                        SHUTDOWN(ENABLED)      ISOLATE(YES)           BREXIT()                                        
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(250)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
 
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CQRY)      GROUP(DFHSTAND) DESCRIPTION()          PROGRAM(DFHQRY)        TWASIZE(0)                           
                                        PROFILE(DFHCICST)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
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
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CSAC)      GROUP(DFHSTAND) DESCRIPTION()          PROGRAM(DFHACP)        TWASIZE(40)                          
                                        PROFILE(DFHCICST)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(ENABLED)      ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
 
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CSTE)      GROUP(DFHSTAND) DESCRIPTION()          PROGRAM(DFHTACP)       TWASIZE(432)                         
                                        PROFILE(DFHCICST)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(ENABLED)      ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CXCU)      GROUP(DFHSTAND) DESCRIPTION()          PROGRAM(DFHCXCU)       TWASIZE(0)                           
                                        PROFILE(DFHCICSV)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CXRE)      GROUP(DFHSTAND) DESCRIPTION()          PROGRAM(DFHZXRE)       TWASIZE(0)                           
                                        PROFILE(DFHCICSV)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 ************************************************************************************************************************             
                                                                                                                                      
 GROUP NAME: DFHVTAM     (IBM PROTECTED)                                                                                              
 ----------                                                                                                                           
                                                                                                                                      
 PROGRAM(DFHGMM)        GROUP(DFHVTAM)                                                                                     
                                        DESCRIPTION(VTAM LU startup message)                                                          
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)                                    
                                        USAGE(NORMAL)          USELPACOPY(NO)         STATUS(ENABLED)                                 
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)                                   
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)                                                           
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 
 PROGRAM(DFHZNAC)       GROUP(DFHVTAM)  DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(YES)          USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHZNEP)       GROUP(DFHVTAM)  DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(YES)          USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CSGM)      GROUP(DFHVTAM)  DESCRIPTION()          PROGRAM(DFHGMM)        TWASIZE(0)                           
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
                                        TPURGE(YES)            DUMP(YES)              TRACE(NO)                                       
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 
 TRANSACTION(CSNE)      GROUP(DFHVTAM)  DESCRIPTION()          PROGRAM(DFHZNAC)       TWASIZE(0)                           
                                        PROFILE(DFHCICSV)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(ANY)       TASKDATAKEY(CICS)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(ENABLED)      ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(255)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 ************************************************************************************************************************             
                                                                                                                                      
 GROUP NAME: DFHVTAMP    (IBM PROTECTED)                                                                                              
 ----------                                                                                                                           
                                                                                                                                      
 PROGRAM(DFHCPY)        GROUP(DFHVTAMP) DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHEXI)        GROUP(DFHVTAMP) DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
 
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHPRK)        GROUP(DFHVTAMP) DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 PROGRAM(DFHRKB)        GROUP(DFHVTAMP) DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(NO)               DATALOCATION(ANY)                               
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CSCY)      GROUP(DFHVTAMP) DESCRIPTION()          PROGRAM(DFHCPY)        TWASIZE(16)                          
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
 
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CSPK)      GROUP(DFHVTAMP) DESCRIPTION()          PROGRAM(DFHPRK)        TWASIZE(16)                          
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
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 TRANSACTION(CSRK)      GROUP(DFHVTAMP) DESCRIPTION()          PROGRAM(DFHRKB)        TWASIZE(0)                           
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
                                                                                                                                      
                                        DEFINETIME(16/06/23 11:44:44)                                                                 
                                        CHANGETIME(16/06/23 11:44:44)                                                                 
 
                                        CHANGEUSRID(TKMS002)   CHANGEAGENT(CSDBATCH)  CHANGEAGREL(0700)                               
                                                                                                                                      
 ************************************************************************************************************************             
                                                                                                                                      
 GROUP NAME: PCTP8                                                                                                                    
 ----------                                                                                                                           
                                                                                                                                      
 PROFILE(XXXXDSNC)      GROUP(PCTP8)    DESCRIPTION()          SCRNSIZE(DEFAULT)      UCTRAN(NO)                           
                                        MODENAME()             FACILITYLIKE()         PRINTERCOMP(NO)                                 
                        JOURNALLING                                                                                                   
                                        JOURNAL(NO)            MSGJRNL(NO)                                                            
                        PROTECTION                                                                                                    
                                        MSGINTEG(NO)           ONEWTE(NO)             CHAINCONTROL(NO)                                
                        PROTOCOLS                                                                                                     
                                        DVSUPRT(ALL)           INBFMH(EODS)           RAQ(NO)                                         
                                        LOGREC(NO)                                                                                    
                        RECOVERY                                                                                                      
                                        NEPCLASS(0)            RTIMOUT(NO)                                                            
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROFILE(XXXXPTMC)      GROUP(PCTP8)    DESCRIPTION()          SCRNSIZE(DEFAULT)      UCTRAN(NO)                           
                                        MODENAME()             FACILITYLIKE()         PRINTERCOMP(NO)                                 
                        JOURNALLING                                                                                                   
                                        JOURNAL(NO)            MSGJRNL(NO)                                                            
                        PROTECTION                                                                                                    
                                        MSGINTEG(NO)           ONEWTE(NO)             CHAINCONTROL(NO)                                
                        PROTOCOLS                                                                                                     
                                        DVSUPRT(VTAM)          INBFMH(EODS)           RAQ(NO)                                         
                                        LOGREC(NO)                                                                                    
                        RECOVERY                                                                                                      
                                        NEPCLASS(0)            RTIMOUT(NO)                                                            
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(ADYN)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(DFH99)         TWASIZE(0)                           
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
 
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(CTST)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(CEMTTST)       TWASIZE(10)                          
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(DBA1)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(DBA001MO)      TWASIZE(0)                           
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(DISABLED)                                
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(DBUG)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(DFHDBUG)       TWASIZE(0)                           
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(DISABLED)                                
 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(DEMO)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(INTDEMO)       TWASIZE(0)                           
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(DISP)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(DSNCCOM1)      TWASIZE(1200)                        
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(DRSD)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(DRS1DSPL)      TWASIZE(500)                         
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(DSAB)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(DFHDSAB)       TWASIZE(100)                         
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(DISABLED)                                
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
 
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(DSNC)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(DSNCCOM1)      TWASIZE(1200)                        
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(DYNA)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(DFHDYNA)       TWASIZE(180)                         
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
 
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(D8CS)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(DSN8CC0)       TWASIZE(0)                           
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(D8PP)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(DSN8CP6)       TWASIZE(0)                           
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(D8PS)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(DSN8CP0)       TWASIZE(0)                           
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(D8PT)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(DSN8CP3)       TWASIZE(0)                           
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(D8PU)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(DSN8CP3)       TWASIZE(0)                           
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(ECHO)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(ECHO)          TWASIZE(30)                          
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(DISABLED)                                
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(ENAB)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(DFHENAB)       TWASIZE(100)                         
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(DISABLED)                                
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(FREE)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(DFHDYNA)       TWASIZE(180)                         
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(IECU)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(TZLLZBSS)      TWASIZE(0)                           
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(150)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(IEC1)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(TZLLZASS)      TWASIZE(0)                           
 
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(150)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(IEC2)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(TZLLZDSS)      TWASIZE(0)                           
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(150)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(IEEX)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(TZLLZBSS)      TWASIZE(0)                           
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
 
                        SCHEDULING                                                                                                    
                                        PRIORITY(150)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(IEOL)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(TZLLZBSS)      TWASIZE(0)                           
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(150)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(IEOS)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(JPLINK)        TWASIZE(0)                           
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(150)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
 
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(IERC)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(TZLLZBSS)      TWASIZE(0)                           
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(150)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(IEXE)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(TZLLZXEX)      TWASIZE(0)                           
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(150)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
 
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(INQR)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(DFHINQ01)      TWASIZE(525)                         
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(DISABLED)                                
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(LOAD)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(DFHNEWP)       TWASIZE(10)                          
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(DISABLED)                                
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(LOOK)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(DFHLOOK)       TWASIZE(0)                           
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(DISABLED)                                
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(MSAR)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(LPCZZNUC)      TWASIZE(4096)                        
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(150)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(MSAS)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(LPCZZNUC)      TWASIZE(4096)                        
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(150)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
 
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(MSA1)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(LPCZZNUC)      TWASIZE(4096)                        
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(150)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(MSA2)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(LPCZZNUC)      TWASIZE(4096)                        
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(150)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
 
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(PCPR)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(PLQPRT)        TWASIZE(4096)                        
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(150)          TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(PRNT)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(DFHTDWT$)      TWASIZE(0)                           
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(DISABLED)                                
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 
 TRANSACTION(PTMC)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(PRKCS020)      TWASIZE(0)                           
                                        PROFILE(XXXXPTMC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(P010)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(PRKCS030)      TWASIZE(0)                           
                                        PROFILE(XXXXPTMC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(P020)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(PRKCS040)      TWASIZE(0)                           
                                        PROFILE(XXXXPTMC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(P030)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(PRKCS050)      TWASIZE(0)                           
                                        PROFILE(XXXXPTMC)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(NO)                                      
                                        TPURGE(NO)             DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(SINK)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(AFCPSINK)      TWASIZE(0)                           
                                        PROFILE(DFHCICST)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(50)           TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
 
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(SRCH)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(AFCP2065)      TWASIZE(0)                           
                                        PROFILE(DFHCICST)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM()                                  
                                        REMOTENAME()           TRPROF()               LOCALQ()                                        
                        SCHEDULING                                                                                                    
                                        PRIORITY(50)           TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(TEST)      GROUP(PCTP8)    DESCRIPTION()          PROGRAM(COBTEST)       TWASIZE(100)                         
                                        PROFILE(XXXXDSNC)      PARTITIONSET()         STATUS(DISABLED)                                
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
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
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(YES)            DUMP(YES)              TRACE(YES)                                      
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
 
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 ************************************************************************************************************************             
                                                                                                                                      
 GROUP NAME: PPTP8                                                                                                                    
 ----------                                                                                                                           
                                                                                                                                      
 MAPSET(DSN8CCD)        GROUP(PPTP8)    DESCRIPTION()          RESIDENT(NO)           USAGE(TRANSIENT)                     
                                        USELPACOPY(NO)         STATUS(ENABLED)                                                        
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 MAPSET(DSN8CCG)        GROUP(PPTP8)    DESCRIPTION()          RESIDENT(NO)           USAGE(TRANSIENT)                     
                                        USELPACOPY(NO)         STATUS(ENABLED)                                                        
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 MAPSET(DSN8CPD)        GROUP(PPTP8)    DESCRIPTION()          RESIDENT(NO)           USAGE(TRANSIENT)                     
                                        USELPACOPY(NO)         STATUS(ENABLED)                                                        
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 MAPSET(DSN8CPE)        GROUP(PPTP8)    DESCRIPTION()          RESIDENT(NO)           USAGE(TRANSIENT)                     
                                        USELPACOPY(NO)         STATUS(ENABLED)                                                        
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 MAPSET(DSN8CPF)        GROUP(PPTP8)    DESCRIPTION()          RESIDENT(NO)           USAGE(TRANSIENT)                     
                                        USELPACOPY(NO)         STATUS(ENABLED)                                                        
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 MAPSET(DSN8CPG)        GROUP(PPTP8)    DESCRIPTION()          RESIDENT(NO)           USAGE(TRANSIENT)                     
                                        USELPACOPY(NO)         STATUS(ENABLED)                                                        
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 MAPSET(DSN8CPL)        GROUP(PPTP8)    DESCRIPTION()          RESIDENT(NO)           USAGE(TRANSIENT)                     
                                        USELPACOPY(NO)         STATUS(ENABLED)                                                        
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 MAPSET(DSN8CPN)        GROUP(PPTP8)    DESCRIPTION()          RESIDENT(NO)           USAGE(TRANSIENT)                     
                                        USELPACOPY(NO)         STATUS(ENABLED)                                                        
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 
 MAPSET(DSN8CPU)        GROUP(PPTP8)    DESCRIPTION()          RESIDENT(NO)           USAGE(TRANSIENT)                     
                                        USELPACOPY(NO)         STATUS(ENABLED)                                                        
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(COBDBG)        GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(COBTEST)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(DFHDBP)        GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(DFHDEB7A)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
 
                                                                                                                                      
 PROGRAM(DFHPEP)        GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(DFHPLTP3)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(DFHPLTP4)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(DFHPLTP5)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(DFHPLTP6)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
 
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(DFHPLT00)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(DFHPLT01)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(DFHTBP)        GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(DFHXLT01)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
 
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(DRSSINTC)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(DRSSSHTC)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(CICS)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(DRSSSHUT)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(DRS1INTC)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(DRS1SHUT)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
 
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(DSN8CC0)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(COBOL)        RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(DSN8CC1)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(COBOL)        RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(DSN8CC2)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(COBOL)        RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(DSN8CP0)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(PLI)          RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
 
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(DSN8CP1)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(PLI)          RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(DSN8CP2)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(PLI)          RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(DSN8CP3)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(PLI)          RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(DSN8CP6)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(PLI)          RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
 
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(DSN8CP7)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(PLI)          RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(DSN8CP8)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(PLI)          RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCACS)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCANE)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCANF)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
 
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCBID)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCBUG)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCCLS)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCCTL)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
 
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCCVB)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCDIF)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCFDP)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCFPW)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 
 PROGRAM(IGZCIDB)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCINS)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCIVL)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCLDR)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCLLM)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
 
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCLNK)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCMSF)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCMST)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCONV)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
 
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCPAC)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCPCC)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCPRC)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCPRS)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCRCL)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
 
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCRSU)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCSCH)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCSFT)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCSMV)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
 
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCSSN)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCSSR)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCSTG)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCUST)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
 
                                                                                                                                      
 PROGRAM(IGZCVLD)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCVMO)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCXDI)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCXMU)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZCXPR)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
 
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZEABN)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZEABX)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZEAFT)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZEBEF)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
 
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZEDBG)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZEDBR)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZEDBW)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZEDXT)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZEINI)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
 
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZELDL)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZEMSG)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZEOPD)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZERID)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
 
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZESAT)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZESNP)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZESPM)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZETCL)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
 
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZETID)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZETRM)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZETSU)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZETUN)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZTBGT)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
 
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZTBMN)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZTRCL)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(IGZTTCL)       GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCCZEDC)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
 
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCCZTDP)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCHAI)        GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCHMN)        GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCHMP)        GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 
 PROGRAM(LPCINQ)        GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCMEN)        GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCZZCMD)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCZZENQ)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(YES)          USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCZZFCN)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
 
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCZZMMA)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCZZMMH)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCZZMMM)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCZZMMN)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
 
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCZZMMP)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCZZMM6)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCZZMM7)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCZZMM8)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCZZNUC)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(YES)          USAGE(NORMAL)          USELPACOPY(NO)                                  
 
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCZZOPR)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCZZOP1)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCZZPSC)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCZZRTY)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
 
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCZZSEC)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCZZXIE)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCZZXIT)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPCZZXSL)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
 
                                                                                                                                      
 PROGRAM(LPCZZXSP)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPC091)        GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPC092)        GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPC094)        GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LPC095)        GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
 
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LTCZZOST)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(YES)          USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(LUSRSION)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(PLQXCT)        GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(COBOL)        RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 PROGRAM(RCHKCICS)      GROUP(PPTP8)    DESCRIPTION()          LANGUAGE(ASSEMBLER)    RELOAD(NO)                           
                                        RESIDENT(NO)           USAGE(NORMAL)          USELPACOPY(NO)                                  
                                        STATUS(ENABLED)        CEDF(YES)              DATALOCATION(BELOW)                             
                                        EXECKEY(USER)          CONCURRENCY(QUASIRENT) API(CICSAPI)                                    
                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()                                    
                                        TRANSID()              EXECUTIONSET(FULLAPI)                                                  
                        JVM-ATTRIBUTES                                                                                                
                                        JVM(NO)                JVMCLASS()             JVMSERVER()                                     
 
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 
