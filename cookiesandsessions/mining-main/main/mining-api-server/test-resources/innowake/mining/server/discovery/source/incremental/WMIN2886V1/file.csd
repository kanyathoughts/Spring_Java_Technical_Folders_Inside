 FILE(DFHCMACD)         GROUP(GCCCMAC)                                                                       99.296 11:17
                                        DESCRIPTION(CMAC Messages File - Enter DSName for Dynamic Allocation)
                        VSAM-PARAMETERS
                                        DSNAME()               PASSWORD()             RLSACCESS(NO)
                                        LSRPOOLID(1)           READINTEG(UNCOMMITTED) DSNSHARING(ALLREQS)
                                        STRINGS(1)             NSRGROUP()
                        REMOTE-ATTRIBUTES
                                        REMOTESYSTEM()         REMOTENAME()
                        REMOTE-AND-CFDATATABLE-PARAMETERS
                                        RECORDSIZE()           KEYLENGTH(9)
                        INITIAL-STATUS
                                        STATUS(DISABLED)       OPENTIME(FIRSTREF)     DISPOSITION(SHARE)
                        BUFFERS
                                        DATABUFFERS(2)         INDEXBUFFERS(1)
                        DATATABLE-PARAMETERS
                                        TABLE(NO)              MAXNUMRECS(NOLIMIT)
                        CFDATATABLE-PARAMETERS
                                        CFDTPOOL()             TABLENAME()            UPDATEMODEL(LOCKING)
                                        LOAD(NO)
                        DATA-FORMAT
                                        RECORDFORMAT(V)
                        OPERATIONS
                                        ADD(NO)                BROWSE(NO)             DELETE(NO)
-                                       READ(YES)              UPDATE(NO)
                        AUTO-JOURNALLING
                                        JOURNAL(NO)            JNLREAD(NONE)          JNLSYNCREAD(NO)
                                        JNLUPDATE(NO)          JNLADD(NONE)           JNLSYNCWRITE(NO)
                        RECOVERY-PARAMETERS
                                        RECOVERY(NONE)         FWDRECOVLOG(NO)        BACKUPTYPE(STATIC)
                        DEFINITION-SIGNATURE
                                        DEFINETIME()           CHANGETIME()           CHANGEUSRID()
                                        CHANGEAGENT()          CHANGEAGREL()
