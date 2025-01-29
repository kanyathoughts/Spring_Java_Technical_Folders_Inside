//TESTJOB  JOB (09131,P00,1000,RA001),'TEST JOB            ',CLASS=3,   00000106
//         MSGCLASS=2,NOTIFY=RAISXXX                                    00000206
//**********************************************************************
//DELETE1  EXEC PGM=IDCAMS                                              00000910
//SYSPRINT DD SYSOUT=*                                                  00000920
//SYSIN    DD *                                                         00000930
  DELETE   TEST.JOB.SORT10.TESTI001                             PURGE   00001050
  IF MAXCC EQ 0008 THEN SET MAXCC EQ 0000
/*                                                                      00000940
//**********************************************************************
//SORT10   EXEC PGM=SORT,                                               00000980
//         COND=(0,NE)                                                  00000990
//SYSOUT   DD SYSOUT=*                                                  00001000
//SYSPRINT DD SYSOUT=*                                                  00001010
//SORTIN   DD DISP=SHR,                                                 00001020
//            DSN=TEST.JOB.SORT10.TESTI001                              00001020
//SORTOUT  DD DISP=(,CATLG,DELETE),                                     00001040
//            DSN=TEST.JOB.SORT10.TESTO001,                             00001050
//            UNIT=SMDA,SPACE=(TRK,(40,04),RLSE),                       00001060
//            DCB=(RECFM=FB,LRECL=365,BLKSIZE=0),                       00001080
//            LABEL=RETPD=10                                            00001070
//SYSIN    DD *                                                         00001090
   SORT FIELDS=(01,26,CH,A,57,02,ZD,A,55,02,ZD,A)
   END
/*
//**********************************************************************
//IEFBR10  EXEC PGM=IEFBR14,
//         COND=(0,NE)
//AFDWREJO DD DISP=(,CATLG),
//            DSN=TEST.JOB.SORT10.TESTR001,
//            UNIT=SMDA,SPACE=(CYL,(05,05)),
//            LABEL=RETPD=90,
//            DCB=(RECFM=FB,LRECL=2000,BLKSIZE=0)
//*
//**********************************************************************01120006
//**********************************************************************01120006
//TESTJOB  JOB (09131,P00,1000,RA001),'TEST JOB            ',CLASS=3,   00000106
//         MSGCLASS=2,NOTIFY=RAISXXX                                    00000206
//**********************************************************************
//DELETE1  EXEC PGM=IDCAMS                                              00000910
//SYSPRINT DD SYSOUT=*                                                  00000920
//SYSIN    DD *                                                         00000930
  DELETE   TEST.JOB.SORT10.TESTI002                             PURGE   00001050
  IF MAXCC EQ 0008 THEN SET MAXCC EQ 0000
/*                                                                      00000940
//**********************************************************************
//SORT10   EXEC PGM=SORT,                                               00000980
//         COND=(0,NE)                                                  00000990
//SYSOUT   DD SYSOUT=*                                                  00001000
//SYSPRINT DD SYSOUT=*                                                  00001010
//SORTIN   DD DISP=SHR,                                                 00001020
//            DSN=TEST.JOB.SORT10.TESTI002                              00001020
//SORTOUT  DD DISP=(,CATLG,DELETE),                                     00001040
//            DSN=TEST.JOB.SORT10.TESTO002,                             00001050
//            UNIT=SMDA,SPACE=(TRK,(40,04),RLSE),                       00001060
//            DCB=(RECFM=FB,LRECL=365,BLKSIZE=0),                       00001080
//            LABEL=RETPD=10                                            00001070
//SYSIN    DD *                                                         00001090
   SORT FIELDS=(01,26,CH,A,57,02,ZD,A,55,02,ZD,A)
   END
/*
//**********************************************************************
//IEFBR10  EXEC PGM=IEFBR14,
//         COND=(0,NE)
//AFDWREJO DD DISP=(,CATLG),
//            DSN=TEST.JOB.SORT10.TESTR002,
//            UNIT=SMDA,SPACE=(CYL,(05,05)),
//            LABEL=RETPD=90,
//            DCB=(RECFM=FB,LRECL=2000,BLKSIZE=0)
//*
//**********************************************************************01120006
//**********************************************************************01120006
//TESTJOB  JOB (09131,P00,1000,RA001),'TEST JOB            ',CLASS=3,   00000106
//         MSGCLASS=2,NOTIFY=RAISXXX                                    00000206
//**********************************************************************
//DELETE1  EXEC PGM=IDCAMS                                              00000910
//SYSPRINT DD SYSOUT=*                                                  00000920
//SYSIN    DD *                                                         00000930
  DELETE   TEST.JOB.SORT10.TESTI003                             PURGE   00001050
  IF MAXCC EQ 0008 THEN SET MAXCC EQ 0000
/*                                                                      00000940
//**********************************************************************
//SORT10   EXEC PGM=SORT,                                               00000980
//         COND=(0,NE)                                                  00000990
//SYSOUT   DD SYSOUT=*                                                  00001000
//SYSPRINT DD SYSOUT=*                                                  00001010
//SORTIN   DD DISP=SHR,                                                 00001020
//            DSN=TEST.JOB.SORT10.TESTI003                              00001020
//SORTOUT  DD DISP=(,CATLG,DELETE),                                     00001040
//            DSN=TEST.JOB.SORT10.TESTO003,                             00001050
//            UNIT=SMDA,SPACE=(TRK,(40,04),RLSE),                       00001060
//            DCB=(RECFM=FB,LRECL=365,BLKSIZE=0),                       00001080
//            LABEL=RETPD=10                                            00001070
//SYSIN    DD *                                                         00001090
   SORT FIELDS=(01,26,CH,A,57,02,ZD,A,55,02,ZD,A)
   END
/*
//**********************************************************************
//IEFBR10  EXEC PGM=IEFBR14,
//         COND=(0,NE)
//AFDWREJO DD DISP=(,CATLG),
//            DSN=TEST.JOB.SORT10.TESTR003,
//            UNIT=SMDA,SPACE=(CYL,(05,05)),
//            LABEL=RETPD=90,
//            DCB=(RECFM=FB,LRECL=2000,BLKSIZE=0)
//*