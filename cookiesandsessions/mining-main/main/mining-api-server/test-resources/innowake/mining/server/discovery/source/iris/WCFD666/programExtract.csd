 DEFINE PROGRAM(DCSEL18) GROUP(Y5PPT01P)
        LANGUAGE(ASSEMBLER) RELOAD(NO) RESIDENT(NO) USAGE(NORMAL)
        USELPACOPY(NO) STATUS(ENABLED) CEDF(YES) DATALOCATION(BELOW)
        EXECKEY(USER) CONCURRENCY(QUASIRENT) API(CICSAPI) DYNAMIC(NO)
        EXECUTIONSET(FULLAPI) JVM(NO)

 DEFINE PROGRAM(BHPSPA01) GROUP(UTPLR01P)
        DESCRIPTION(IDMS DATA ACCESS PROGRAM)
        LANGUAGE(COBOL) RELOAD(NO) RESIDENT(NO) USAGE(NORMAL)
        USELPACOPY(NO) STATUS(ENABLED) CEDF(YES) DATALOCATION(BELOW)
        EXECKEY(USER) CONCURRENCY(QUASIRENT) API(CICSAPI) DYNAMIC(NO)
        EXECUTIONSET(FULLAPI) JVM(NO)
