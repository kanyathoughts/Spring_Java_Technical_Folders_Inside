       IDENTIFICATION DIVISION.
       PROGRAM-ID. UTPEQ003
       AUTHOR. JEFF NEUGROSCHEL.
       INSTALLATION.  AGENCIES DATABASE - ROSELAND3
       ENVIRONMENT DIVISION.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       01  WORK-AREAS.
      *
           05  DB2-SET                              POINTER.
           05  DB2-LEN                              PIC S9(4) COMP.
       LINKAGE SECTION.
       01  DFHCOMMAREA.
             05  FILLER                        PIC X
                                               OCCURS 1 TO 999 TIMES
                                               DEPENDING ON EIBCALEN.
      *
       PROCEDURE DIVISION.
       0001-CHECK-DB2-SUBSYSTEM.
       
           EXEC CICS
                EXTRACT EXIT
                PROGRAM('DSNCEXT1')
                ENTRYNAME('DSNCSQL')
                GASET(DB2-SET)
                GALENGTH(DB2-LEN)
           END-EXEC.
      *
