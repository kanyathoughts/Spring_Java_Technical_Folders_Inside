		CBL RENT
		 IDENTIFICATION DIVISION.
		 PROGRAM-ID. GETPRML.
		 AUTHOR. EXAMPLE.
		 DATE-WRITTEN.   03/25/98.
		 
		 ENVIRONMENT DIVISION.
		 INPUT-OUTPUT SECTION.
		 FILE-CONTROL.
		 DATA DIVISION.
		 FILE SECTION.
		*
		 WORKING-STORAGE SECTION.
		*
		     EXEC SQL INCLUDE SQLCA END-EXEC.
		*
		***************************************************
		*   DECLARE A HOST VARIABLE TO HOLD INPUT SCHEMA
		***************************************************
		 01  INSCHEMA PIC X(8).
		***************************************************
		*   DECLARE CURSOR FOR RETURNING RESULT SETS
		***************************************************
		*
		     EXEC SQL DECLARE C1 CURSOR WITH RETURN FOR
		       SELECT NAME FROM SYSIBM.SYSTABLES WHERE CREATOR=:INSCHEMA
		     END-EXEC.
		*
		 LINKAGE SECTION.
		***************************************************
		*   DECLARE THE INPUT PARAMETERS FOR THE PROCEDURE
		***************************************************
		 01  PROCNM  PIC X(18).
		 01  SCHEMA  PIC X(8).
		***************************************************
		*   DECLARE THE OUTPUT PARAMETERS FOR THE PROCEDURE
		***************************************************
		 01  OUT-CODE PIC S9(9) USAGE BINARY.
		 01  PARMLST.
		     49 PARMLST-LEN  PIC S9(4) USAGE BINARY.
		     49 PARMLST-TEXT PIC X(254).
		***************************************************
		*   DECLARE THE STRUCTURE CONTAINING THE NULL
		*   INDICATORS FOR THE INPUT AND OUTPUT PARAMETERS.
		***************************************************
		 01  IND-PARM.
		     03 PROCNM-IND   PIC S9(4) USAGE BINARY.
		     03 SCHEMA-IND   PIC S9(4) USAGE BINARY.
		     03 OUT-CODE-IND PIC S9(4) USAGE BINARY.
		     03 PARMLST-IND  PIC S9(4) USAGE BINARY.
		 Copy code
		 PROCEDURE DIVISION USING PROCNM, SCHEMA,
		           OUT-CODE, PARMLST, IND-PARM.
		*******************************************************
		* If any input parameter is null, return a null value
		* for PARMLST and set the output return code to 9999.
		*******************************************************
		      IF PROCNM-IND < 0 OR
		         SCHEMA-IND < 0
		           MOVE 9999 TO OUT-CODE
		           MOVE 0 TO OUT-CODE-IND
		           MOVE -1 TO PARMLST-IND
		      ELSE
		*******************************************************
		* Issue the SQL SELECT against the SYSIBM.SYSROUTINES
		* DB2 catalog table.
		*******************************************************
		     EXEC SQL
		       SELECT RUNOPTS INTO :PARMLST
		       FROM SYSIBM.SYSROUTINES
		       WHERE NAME=:PROCNM AND
		       SCHEMA=:SCHEMA
		     END-EXEC
		           MOVE 0 TO PARMLST-IND
		*******************************************************
		*  COPY SQLCODE INTO THE OUTPUT PARAMETER AREA
		*******************************************************
		           MOVE SQLCODE TO OUT-CODE
		           MOVE 0 TO OUT-CODE-IND.
		*
		*******************************************************
		* OPEN CURSOR C1 TO CAUSE DB2 TO RETURN A RESULT SET
		* TO THE CALLER.
		*******************************************************
		     EXEC SQL OPEN C1
		     END-EXEC.
		 PROG-END.
		     GOBACK.