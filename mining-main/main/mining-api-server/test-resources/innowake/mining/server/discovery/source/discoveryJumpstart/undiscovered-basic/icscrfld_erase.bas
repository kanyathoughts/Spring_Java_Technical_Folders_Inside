	SUB ICSCRFLD_ERASE ( FLD% )

	%INCLUDE	"INC:ICSCREEN.INC"
	%INCLUDE	"INC:ICISTANDARDS.INC"

	DECLARE	LONG	Disp_Type

	EXIT SUB	UNLESS FLD%

	Disp_Type	= Disp_Normal

	SELECT FLD%
	  CASE >= 1%
		I% = FLD%
		GOSUB Erase_1_Field
	  CASE = -1%
		GOSUB Erase_1_Field	FOR I%=1% TO SCR.FLD.N%
	  CASE = -2%
		Disp_Type	= Disp_Reverse
		GOSUB Erase_1_Field	FOR I%=1% TO SCR.FLD.N%

	END SELECT

	EXIT SUB


 Erase_1_Field:
	SELECT SCR.FLD.TYP$( I% )
	  CASE "AD"
		CALL PUT_CHARS_TO_SCR				&
		( SPACE$( 25% )					&
		, SCR.FLD.ROW%( I% ) + R%			&
		, SCR.FLD.COL%( I% )				&
		, Disp_Type					&
		)	FOR R% = 0% TO 1%

		CALL PUT_CHARS_TO_SCR				&
		( SPACE$( 31% )					&
		, SCR.FLD.ROW%( I% ) + 2%			&
		, SCR.FLD.COL%( I% )				&
		, Disp_Type					&
			)

	    CASE ELSE
		CALL PUT_CHARS_TO_SCR				&
		( SPACE$( SCR.FLD.LEN% (I%) ) 			&
		,         SCR.FLD.ROW% (I%)			&
		,         SCR.FLD.COL% (I%)			&
		, Disp_Type					&
		)

	  END SELECT

	RETURN

	END SUB
