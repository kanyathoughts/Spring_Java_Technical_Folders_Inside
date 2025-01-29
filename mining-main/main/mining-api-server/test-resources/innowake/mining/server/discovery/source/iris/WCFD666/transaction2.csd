 PROGRAM(DFHCMAC)       GROUP(GCCCMAC)                                                                       98.135 16:58
                                        DESCRIPTION(CMAC Transaction Program Definition)
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)
                                        USAGE(NORMAL)          USELPACOPY(YES)        STATUS(ENABLED)
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)
                        REMOTE-ATTRIBUTES
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()
                                        TRANSID()              EXECUTIONSET(FULLAPI)
                        JVM-ATTRIBUTES
                                        JVM(NO)                JVMCLASS()             JVMPROFILE(DFHJVMPR)
                        DEFINITION-SIGNATURE
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()
                                        CHANGEAGENT()          CHANGEAGREL()

 PROGRAM(DFHCMAC)       GROUP(GCCCMAC)                                                                       98.135 16:58
                                        DESCRIPTION(CMAC Transaction Program Definition)
                                        LANGUAGE(ASSEMBLER)    RELOAD(NO)             RESIDENT(NO)
                                        USAGE(NORMAL)          USELPACOPY(YES)        STATUS(ENABLED)
                                        CEDF(NO)               DATALOCATION(ANY)      EXECKEY(CICS)
                                        CONCURRENCY(QUASIRENT) API(CICSAPI)
                        REMOTE-ATTRIBUTES
                                        DYNAMIC(NO)            REMOTESYSTEM()         REMOTENAME()
                                        TRANSID()              EXECUTIONSET(FULLAPI)
                        JVM-ATTRIBUTES
                                        JVM(NO)                JVMCLASS()             JVMPROFILE(DFHJVMPR)
                        DEFINITION-SIGNATURE
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()
                                        CHANGEAGENT()          CHANGEAGREL()

 TRANSACTION(SYMM)      GROUP(DPPCTT#A) DESCRIPTION()          PROGRAM()              TWASIZE(0)                          
                                        PROFILE(DFHCICST)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      

                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM(CIP5)                              
                                        REMOTENAME(SYMM)       TRPROF(DFHCICSS)       LOCALQ(NO)                                      
                        SCHEDULING                                                                                                    
                                        PRIORITY(1)            TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(NO)             DUMP(NO)               TRACE(NO)                                       
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
 TRANSACTION(SYMM)      GROUP(DPPCTT#A) DESCRIPTION()          PROGRAM()              TWASIZE(0)                          
                                        PROFILE(DFHCICST)      PARTITIONSET()         STATUS(ENABLED)                                 
                                        TASKDATALOC(BELOW)     TASKDATAKEY(USER)      STORAGECLEAR(NO)                                
                                        RUNAWAY(SYSTEM)        SHUTDOWN(DISABLED)     ISOLATE(YES)                                    
                                        BREXIT()                                                                                      

                        REMOTE-ATTRIBUTES                                                                                             
                                        DYNAMIC(NO)            ROUTABLE(NO)           REMOTESYSTEM(CIP5)                              
                                        REMOTENAME(SYMM)       TRPROF(DFHCICSS)       LOCALQ(NO)                                      
                        SCHEDULING                                                                                                    
                                        PRIORITY(1)            TRANCLASS(DFHTCL00)                                                    
                        ALIASES                                                                                                       
                                        ALIAS()                TASKREQ()              XTRANID()                                       
                                        TPNAME()               XTPNAME()                                                              
                        RECOVERY                                                                                                      
                                        DTIMOUT(NO)            RESTART(NO)            SPURGE(YES)                                     
                                        TPURGE(NO)             DUMP(NO)               TRACE(NO)                                       
                                        CONFDATA(NO)           OTSTIMEOUT(NO)                                                         
                        INDOUBT-ATTRIBUTES                                                                                            
                                        ACTION(BACKOUT)        WAIT(YES)              WAITTIME(0,0,0)                                 
                        SECURITY                                                                                                      
                                        RESSEC(NO)             CMDSEC(NO)                                                             
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          
                                                                                                                                      
                                                                                                                                      