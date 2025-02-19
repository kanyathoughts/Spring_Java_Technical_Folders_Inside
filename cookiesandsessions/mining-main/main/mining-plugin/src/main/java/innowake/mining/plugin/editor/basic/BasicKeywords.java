/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.editor.basic;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

/**
 * Class to provide a list of keywords in BASIC.
 */
public class BasicKeywords {

	private static final List<String> WORDS = new LinkedList<>();
	
	/**
	 * Returns the list of keywords in BASIC.
	 *
	 * @return BASIC language keywords
	 */
	public static List<String> getKeywords() {
		return Collections.unmodifiableList(WORDS);
	}

	private static void add(final String word) {
		WORDS.add(word);
	}

	static {
		add("%ABORT");
		add("%CROSS");
		add("%DECLARED");
		add("%DEFINE");
		add("%IDENT");
		add("%IF");
		add("%THEN");
		add("%ELSE");
		add("%END");
		add("%IF");
		add("%INCLUDE");
		add("%LET");
		add("%LIST");
		add("%NOCROSS");
		add("%NOLIST");
		add("%PAGE");
		add("%PRINT");
		add("%REPORT");
		add("%SBTTL");
		add("%TITLE");
		add("%UNDEFINE");
		add("%VARIANT");
		add("ABS");
		add("ABS%");
		add("ASCII");
		add("ATN");
		add("BUFSIZ");
		add("CALL");
		add("CAUSE");
		add("ERROR");
		add("CCPOS");
		add("CHAIN");
		add("CHANGE");
		add("CHR$");
		add("CLOSE");
		add("COMMON");
		add("COMP%");
		add("CONTINUE");
		add("COS");
		add("CTRLC");
		add("CVT$$");
		add("CVTxx");
		add("DATA");
		add("DATE$");
		add("DATE");
		add("DATE4$");
		add("DECIMAL");
		add("DECLARE");
		add("DEF");
		add("DEF*");
		add("DELETE");
		add("DET");
		add("DIF$");
		add("DIMENSION");
		add("ECHO");
		add("EDIT$");
		add("END");
		add("ERL");
		add("ERN$");
		add("ERR");
		add("ERT$");
		add("EXIT");
		add("EXP");
		add("EXTERNAL");
		add("FIELD");
		add("FIND");
		add("FIX");
		add("FNEND");
		add("FNEXIT");
		add("FOR");
		add("FORMAT$");
		add("FREE");
		add("FSP$");
		add("FUNCTION");
		add("FUNCTIONEND");
		add("FUNCTIONEXIT");
		add("GET");
		add("GETRFA");
		add("GOSUB");
		add("GOTO");
		add("HANDLER");
		add("IF");
		add("INKEY$");
		add("INPUT");
		add("INPUT");
		add("LINE");
		add("INSTR");
		add("INT");
		add("INTEGER");
		add("ITERATE");
		add("KILL");
		add("LBOUND");
		add("LEFT$");
		add("LEN");
		add("LET");
		add("LINPUT");
		add("LOC");
		add("LOG");
		add("LOG");
		add("LSET");
		add("MAG");
		add("MAGTAPE");
		add("MAP");
		add("MAP");
		add("DYNAMIC");
		add("MAR");
		add("MARGIN");
		add("MAT");
		add("MAT");
		add("INPUT");
		add("MAT");
		add("LINPUT");
		add("MAT");
		add("PRINT");
		add("MAT");
		add("READ");
		add("MAX");
		add("MID$");
		add("MIN");
		add("MOD");
		add("MOVE");
		add("NAME");
		add("AS");
		add("NEXT");
		add("NOECHO");
		add("NOMARGIN");
		add("NUM");
		add("NUM2");
		add("NUM$");
		add("NUM1$");
		add("ON");
		add("ERROR");
		add("GO");
		add("BACK");
		add("ON");
		add("ERROR");
		add("GOTO");
		add("ON");
		add("ERROR");
		add("GOTO");
		add("ON");
		add("GOSUB");
		add("ON");
		add("GOTO");
		add("OPEN");
		add("OPTION");
		add("PLACE$");
		add("POS");
		add("PRINT");
		add("PRINT");
		add("USING");
		add("PROD$");
		add("PROGRAM");
		add("PUT");
		add("QUO$");
		add("RAD$");
		add("RANDOMIZE");
		add("RCTRLC");
		add("RCTRLO");
		add("READ");
		add("REAL");
		add("RECORD");
		add("RECOUNT");
		add("REM");
		add("REMAP");
		add("RESET");
		add("RESTORE");
		add("RESUME");
		add("RETRY");
		add("RETURN");
		add("RIGHT$");
		add("RMSSTATUS");
		add("RND");
		add("RSET");
		add("SCRATCH");
		add("SEG$");
		add("SELECT");
		add("SET");
		add("PROMPT");
		add("SGN");
		add("SIN");
		add("SLEEP");
		add("SPACE$");
		add("SQR");
		add("STATUS");
		add("STOP");
		add("STR$");
		add("STRING$");
		add("SUB");
		add("SUBEND");
		add("SUBEXIT");
		add("SUM$");
		add("SWAP%");
		add("TAB");
		add("TAN");
		add("TIME");
		add("TIME$");
		add("TRM$");
		add("UBOUND");
		add("UNLESS");
		add("UNLOCK");
		add("UNTIL");
		add("UPDATE");
		add("VAL");
		add("VAL%");
		add("VMSSTATUS");
		add("WAIT");
		add("WHEN");
		add("ERROR");
		add("WHILE");
		add("XLATE$");
	}
	
}
