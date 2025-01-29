/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.cobol;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.StringJoiner;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.SourcePojo;
import innowake.ndt.cobol.parser.CobolToken;
import innowake.ndt.cobol.parser.ast.model.CobolExpression;
import innowake.ndt.cobol.parser.ast.model.CobolNode;
import innowake.ndt.parsing.parser.sql.preprocess.db2.Db2JclPreprocessor;

/**
 * Utility class for processing Cobol statements
 */
public class CobolStatementUtility {
	private static final String SPLITTER = "\n";
	private static final Pattern WHITESPACE = Pattern.compile("\\s+");
	private static final String BLANK = " ";
	private static final Db2JclPreprocessor JCL_PREPROCESSOR = new Db2JclPreprocessor();
	private static final String ID_DIV = "IDENTIFICATION DIVISION";
	
	public static String flattenConditions(final List<CobolExpression> conditions) {
		return conditions.stream()
				.filter(Objects::nonNull)
				.map(CobolNode::toString)
				.collect(Collectors.joining(" "));
	}
	
	/**
	 * Return the CobolStatement without sequence and description. Use Sql-Parser Cobol preprocessor for an SQL statement.
	 *
	 * @param node the given node
	 * @return the String of CobolStatement
	 */
	public static String getCobolStatement(final CobolNode node) {
		/* take the line from the beginning so that the handling of line numbers section is consistent */
		String contents = "";
		final int col = node.getStartToken().getColumn();
		final int start = node.getStartToken().getOffset() - col;
		final int length = getLength(node) + col;
		/* Get the original statement */
		contents = node.getStartToken().getContent().subSequence(start, start + length).toString();
		return   getCobolStatement(contents);
	}
	
	/**
	 * Return the CobolStatement without sequence and description. 
	 *
	 * @param statement the given Cobol statement
	 * @return the String of CobolStatement without sequence and description
	 */
	private static String getCobolStatement(String statement) {
		final List<String> lines = Arrays.asList(getLines(statement));
		final StringJoiner result = new StringJoiner(" ");
		/* remove the sequence and description */
		lines.stream()
				.filter(line -> ! isComment(line))
				.map(CobolStatementUtility::trimDescription)
				.map(CobolStatementUtility::trimSequence)
				.forEach(result::add);
		/* remove the whitespace */
		return  WHITESPACE.matcher(result.toString()).replaceAll(BLANK);
	}
	
	/**
	 * Return single SQL statements in JCL.
	 *
	 * @param statement is SQL in JCL statement
	 * @return SQL string list in JCL statement
	 */
	public static List<String> getSingleBatchSqlStatementList(String statement) {
		return JCL_PREPROCESSOR.getSingleSqlStringList(JCL_PREPROCESSOR.getSqlString(statement));
	}

	
	/**
	 * Calculates the length of a given node.
	 *
	 * @param node the given node
	 * @return the length of the given node
	 */
	private static int getLength(CobolNode node) {
		final CobolToken startToken = node.getStartToken();
		final CobolToken endToken = node.getEndToken();

		return endToken != null 
				? endToken.getOffset()
				+ endToken.getLength()
				- startToken.getOffset()
				: startToken.getLength();
	}
	/**
	 * Check if a line of Cobol code is comment
	 *
	 * @param line the given code line
	 * @return true if the given line is comment
	 */
	public static boolean isComment(final String line) {
		/* a comment can be represented either by * or /* */
		return (line.length() >= 7 && line.charAt(6) == '*') || (line.length() >= 8 && line.charAt(6) == '/' && line.charAt(7) == '*');
	}
	
	public static boolean isIdentificationDivision(final String line) {
		final String lineWithSingleSpace = WHITESPACE.matcher(line).replaceAll(BLANK);
		return lineWithSingleSpace.contains(ID_DIV);
	}
	
	/**
	 * Check if a line of Cobol code has sequence
	 *
	 * @param line the given code line
	 * @return true if the given line has sequence
	 */
	private static boolean hasSequence(final String line){
		/* line number section can be anything */
		return line.length() >= 6;
	}
	
	/**
	 * Trim the the sequence of a line if it has sequence
	 *
	 * @param line the given code line
	 * @return code line without sequence
	 */
	public static String trimSequence(final String line){

		if(hasSequence(line)){
			return line.substring(6).trim();
		}
		else{
			return line.trim();
		}
	}
	
	/**
	 * Check if a line of Cobol code has description
	 *
	 * @param line the given code line
	 * @return true if the given line has description
	 */
	private static boolean hasDescription(final String line){
		return (line.length() >= 72);
	}
	
	/**
	 * Trim the the sequence of a line if it has sequence
	 *
	 * @param line the given code line
	 * @return code line without sequence
	 */
	public static String trimDescription(final String line){
		/* don't trim the line in order to correctly handle start line number section!!! */
		if(hasDescription(line)){
			return line.substring(0,72);
			
		}
		else{
			return line;
		}
	}
	
	
	/**
	 * Return Cobol code in lines
	 *
	 * @param cob Cobol object
	 * @return code lines
	 * @throws DiscoveryException errors when get contents from cobol object
	 */
	public static String[] getLines(final SourcePojo cob) throws DiscoveryException {
		return getLines(cob.getContent().toString());
	}
	
	/**
	 * Return Cobol code in lines
	 *
	 * @param body Cobol code body
	 * @return code lines
	 */
	public static String[] getLines(final String body){
		/* don't trim the line in order to correctly handle start line number section!!! */
		return body.split(SPLITTER);
	}
	
	private CobolStatementUtility() {
		
	}
}