//YDECSD0S JOB (6,DE24A000,PDPB),'TCSES',                               00001000
//             MSGLEVEL=(1,1),       TYPRUN=SCAN,                       00002000
//             MSGCLASS=7,                                              00003000
//             CLASS=A,                                                 00004000
//             RESTART=*,                                               00005000
//             USER=DE01P01,                                            00006000
//             REGION=128M                                              00007000
//********************************************************************* 00008000
//*          VER 14.01                                                  00009000
//********************************************************************* 00009100
//*    UPDATED CA7 01/10/14                                             00009200
//*    TYPRUN SCAN 01/10/14                                             00009300

//********************************************************************* 00035100
//** THIS DETERMINES WHAT DAY OF WEEK IT IS FOR EMAIL PURPOSES       ** 00035200
//********************************************************************* 00035300
//JS001    EXEC PGM=DECSZ027                                            00035400
//STEPLIB  DD  DSN=DE.DE24AIEF.LOAD.PILOT,DISP=SHR                      00035500
//         DD  DSN=SYS1.EASYPLUS,DISP=SHR                               00035600
//         DD  DSN=SYS1.EZTOOLKT,DISP=SHR                               00035700
//         DD  DSN=IMSVS.RESLIB,DISP=SHR                                00035800
//MACDD    DD  DSN=SYS1.EZTOOLKT.MACRO,DISP=SHR                         00035900
//PROMAC   DD  DSN=SYS1.PROMAC,DISP=SHR                                 00036000
//EZTVFM   DD  UNIT=SYSDA,SPACE=(4096,(200,200),RLSE)                   00036100
//PROVFM   DD  UNIT=SYSDA,SPACE=(4096,(200,200),RLSE)                   00036200
//VIOCTL   DD  DSN=ACTIVATE,DISP=SHR,AMP='AMORG',VOL=REF=SYS1.LINKLIB   00036300
//SYSPRINT DD  SYSOUT=*                                                 00036400
//*                                                                     00036500

//JS015    EXEC PGM=EZTPA00                                             00039900
//STEPLIB  DD  DSN=SYS1.EASYPLUS,DISP=SHR                               00040000
//         DD  DSN=SYS1.EZTOOLKT,DISP=SHR                               00040100
//         DD  DSN=IMSVS.RESLIB,DISP=SHR                                00040200
//PROMAC   DD  DSN=SYS1.PROMAC,DISP=SHR                                 00040300
//EZTVFM   DD  UNIT=SYSDA,SPACE=(4096,(200,200),RLSE)                   00040400
//PROVFM   DD  UNIT=SYSDA,SPACE=(4096,(200,200),RLSE)                   00040500
//VIOCTL   DD DSN=ACTIVATE,DISP=SHR,AMP='AMORG',VOL=REF=SYS1.LINKLIB    00040600
//STAREG   DD  DSN=DE.DE24ACS.STCASREG(0),DISP=SHR                      00040700
//EMAIL    DD  DSN=DE.DE24ACS.STAREG.EMAIL,                             00040800
//             DISP=(,CATLG,DELETE),UNIT=SYSDA,                         00040900
//             DCB=(LRECL=80,BLKSIZE=0,RECFM=FB),                       00041000
//             SPACE=(TRK,(5,1),RLSE)                                   00041100
//PRNTOUT1 DD  SYSOUT=*                                                 00041200
//SYSPRINT DD  SYSOUT=*                                                 00041300
//SYSIN    DD  DSN=DE.DE24APDS.PROD.CARD(YD0SC015),DISP=SHR             00041400
//*                                                                     00041500
//*                                                                     00057006
//PS040.SYSUT2 DD DUMMY                                                 00057106
//*                                       DB2 CONTROL CARDS             00057206
//PS020.SYSTSIN DD DSN=DE.DE24APDS.PROD.TSIN(DECS5IJ),DISP=SHR          00057306
//*                                                                     00057406
