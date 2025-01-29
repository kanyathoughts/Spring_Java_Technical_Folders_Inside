                                                                                                                                      
 GROUP NAME: UTOTO1F      (IBM PROTECTED)                                                                                              
 ----------                                                                                                                           
                                                                                                                                      
 FILE(VSAMTABL)         GROUP(UTOTO1F)    DESCRIPTION()                                                                      
                        VSAM-PARAMETERS                                                                                               
                                                                                                                                      
                                        DSNAME(AX29UT.XPCDB2Y2.VSAMTABL)                                                             
                                        RLSACCESS(NO)          LSRPOOLNUM(1)          READINTEG(UNCOMMITTED)                          
                                        DSNSHARING(ALLREQS)    STRINGS(4)            NSRGROUP()                                      
                        REMOTE-ATTRIBUTES                                                                                             
                                        REMOTESYSTEM()         REMOTENAME()                                                           
                        REMOTE-AND-CFDATATABLE-PARAMETERS                                                                             
                                        RECORDSIZE()           KEYLENGTH()                                                            
                        INITIAL-STATUS                                                                                                
                                        STATUS(ENABLED)        OPENTIME(STARTUP)     DISPOSITION(SHARE)                              
                        BUFFERS                                                                                                       
                                        DATABUFFERS(5)        INDEXBUFFERS(5)                                                       
                        DATATABLE-PARAMETERS                                                                                          
                                        TABLE(NO)              MAXNUMRECS(NOLIMIT)                                                    
                        CFDATATABLE-PARAMETERS                                                                                        
                                        CFDTPOOL()             TABLENAME()            UPDATEMODEL(LOCKING)                            
                                        LOAD(NO)                                                                                      
                        DATA-FORMAT                                                                                                   
                                        RECORDFORMAT(V)                                                                               
                        OPERATIONS                                                                                                    
                                        ADD(NO)                BROWSE(NO)            DELETE(NO)                                      
                                        READ(YES)              UPDATE(NO)                                                             
                        AUTO-JOURNALLING                                                                                              
                                        JOURNAL(NO)            JNLREAD(NONE)          JNLSYNCREAD(NO)                                 
                                        JNLUPDATE(NO)          JNLADD(NONE)           JNLSYNCWRITE(YES)                               
                        RECOVERY-PARAMETERS                                                                                           
                                        RECOVERY(NONE)         FWDRECOVLOG(NO)        BACKUPTYPE(STATIC)                              
                        DEFINITION-SIGNATURE                                                                                          
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()                                   
                                        CHANGEAGENT()          CHANGEAGREL()                                                          

 ************************************************************************************************************************             
                                                                                                                                      
 GROUP NAME: DFHTPQ     (IBM PROTECTED)                                                                                              
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
 
