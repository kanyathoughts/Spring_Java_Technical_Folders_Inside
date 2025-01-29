//RPD250SS EXEC PGM=IEFBR14                                             00001000
//RPTRNWS  DD  DSN=BNKCA.IN0.RP50.RPTRNWS,                              00002000
//             DISP=(MOD,DELETE,DELETE),                                00003000
//             UNIT=SYSDA,SPACE=(TRK,(1,1),RLSE)                        00004000
//*                                                                     00005000
//RPD250S  EXEC PGM=RPD250,PARM='ORG=(1)',REGION=1024K                  00006000
//STEPLIB  DD  DSN=BNKCA.RP50.LOADLIB,DISP=SHR
//         DD  DSN=BNKCA.BI15.LOADLIB,DISP=SHR
//         DD  DSN=BNKCA.MI51.LOADLIB,DISP=SHR
//         DD  DSN=BNKCA.RF21.LOADLIB,DISP=SHR
//*** INSERT OTHER SUPPORTING LIBRARIES AS APPROPRIATE ***
//SYSPRINT DD  SYSOUT=*                                                 00008000
//SYSUDUMP DD  SYSOUT=*                                                 00009000
//SYSOUT   DD  SYSOUT=*                                                 00010000
//RPTRNW   DD  DSN=BNKCA.IN0.RP50.RPTRNW,DISP=(OLD,KEEP,KEEP)           00011000
//RPTRNWS  DD  DSN=BNKCA.IN0.RP50.RPTRNWS,                              00012000
//             DISP=(NEW,CATLG,DELETE),                                 00013000
//             DCB=(RECFM=FB,LRECL=43,BLKSIZE=27993),                   00014000
//             SPACE=(CYL,(1,1),RLSE),                                  00015000
//             UNIT=SYSDA                                               00016000
//SORTWK01 DD  UNIT=SYSDA,SPACE=(CYL,(50))
//SORTWK02 DD  UNIT=SYSDA,SPACE=(CYL,(50))
//SORTWK03 DD  UNIT=SYSDA,SPACE=(CYL,(50))
//SORTWK04 DD  UNIT=SYSDA,SPACE=(CYL,(50))
//
