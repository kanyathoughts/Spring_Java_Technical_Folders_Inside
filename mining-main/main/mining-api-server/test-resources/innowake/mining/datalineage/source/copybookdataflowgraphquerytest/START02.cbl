        IDENTIFICATION DIVISION.
        PROGRAM-ID. START02.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
         
         01 WS-FIELD PIC X.
         
        LINKAGE SECTION.
         
         01 G1.
        	05 G1-F1 PIC X.
        	05 G1-F2 PIC X.
        	05 G1-F3 PIC X.
        	05 G1-F4 PIC X.
         
         
        PROCEDURE DIVISION USING G1.
         
        	MOVE G1-F1 TO G1-F4
         
        	DISPLAY G1.
        	DISPLAY WS-FIELD.
         
        END PROGRAM START02.