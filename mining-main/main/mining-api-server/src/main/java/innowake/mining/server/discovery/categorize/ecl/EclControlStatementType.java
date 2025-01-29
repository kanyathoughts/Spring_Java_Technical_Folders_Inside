/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.categorize.ecl;

import java.util.Arrays;
import innowake.ndt.core.parsing.IToken;

/**
 * Enum for control statement types for ECL.
 */
public enum EclControlStatementType {

	ADD("@ADD"),
	ASG("@ASG"),
	ASG1("@@ASG"),	
	BRKPT("@BRKPT"),
	BRKPT1("@@BRKPT"),
	CAT("@CAT"),
	CAT1("@@CAT"),
	CHG("@CHG"),
	CKPT("@CKPT"),
	CLOSE("@CLOSE"),
	CM("@@CM"),
	CONS("@@CONS"),
	CONT("@@CONT"),
	COPIN("@COPYIN"),
	COPOUT("@COPYOUT"),
	COPY("@COPY"),
	CQUE("@@CQUE"),
	CYCLE("@CYCLE"),
	DCT("@@DCT"),
	DELETE("@DELETE"),
	ENABLE("@ENABLE"),
	END("@@END"),
	ENDF("@ENDF"),
	EOF("@EOF"),
	ERS("@ERS"),
	ESC("@@ESC"),
	FILE("@FILE"),
	FIN("@FIN"),
	FIND("@FIND"),
	FREE("@FREE"),
	FREE1("@@FREE"),
	FUL("@@FUL"),
	HDG("@HDG"),
	HDG1("@@HDG"),
	HOLD("@@HOLD"),
	INQ("@@INQ"),
	INS("@@INS"),
	JUMP("@JUMP"),
	LOG("@LOG"),
	LOG1("@@LOG"),
	MARK("@MARK"),
	MODE("@MODE"),
	MODE1("@@MODE"),
	MOVE("@MOVE"),
	MSG("@MSG"),
	MSG1("@@MSG"),
	NOPR("@@NPR"),
	PACK("@PACK"),
	PASSWD("@@PASSWD"),
	PCH("@PCH"),
	PERFEV("@PERFEV"),
	PMOD("@@PMOD"),
	PREP("@PREP"),
	PRNT("@@PRNT"),
	PRT("@PRT"),
	QUAL("@QUAL"),
	QUAL1("@@QUAL"),
	REWIND("@REWIND"),
	RLD("@RLD"),
	RLU("@@RLU"),
	RQUE("@@RQUE"),
	RSTRT("@RSTRT"),
	RUN("@RUN"),
	SEND("@@SEND"),
	SETC("@SETC"),
	SKIP("@@SKIP"),
	SOLI("@@SOLI"),
	START("@START"),
	START1("@@START"),
	SYM("@SYM"),
	SYM1("@@SYM"),
	TEST("@TEST"),
	TERM("@@TERM"),
	TM("@@TM"),
	TOUT("@@TOUT"),
	TRNS("@@TRNS"),
	TTY("@@TTY"),
	USE("@@USE"),
	X("@@X"),
	XQT("@XQT");

	private final String controlStatement;

	EclControlStatementType(final String controlStatement) {
		this.controlStatement = controlStatement;
	}

	/**
	 * Returns type of control statement in ECL. 
	 *
	 * @return control statement type in ECL
	 */
	public String getControlStatement() {
		return controlStatement;
	}

	/**
	 * Check if a given token is a valid control statement type in ECL.
	 *
	 * @param token type of token to be checked
	 * @return true if token is a valid control statement
	 */
	public static boolean isValidControlStatement(final IToken token) {
		return Arrays.stream(EclControlStatementType.values())
				.map(EclControlStatementType::getControlStatement)
				.anyMatch(token.getText().toString()::equalsIgnoreCase);
	}
}
