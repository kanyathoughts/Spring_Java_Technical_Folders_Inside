/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl.natural;

import java.util.regex.Pattern;

import org.apache.commons.lang.StringUtils;

import innowake.mining.data.core.moduledescription.api.CommentPrettyPrinter;
import innowake.mining.data.core.moduledescription.api.PrettyPrinter;

/**
 * Pretty printer for Natural comments removing the start and end markers.
 */
public class NaturalCommentPrettyPrinter extends CommentPrettyPrinter {
	
	/** The {@link NaturalCommentPrettyPrinter} instance. */
	@SuppressWarnings("hiding")
	public static final PrettyPrinter INSTANCE = new NaturalCommentPrettyPrinter();
	
	/** single line comment: * comment */
	private static final Pattern SINGLE_LINE_COMMENT_1 = Pattern.compile("^(\\*)(.*)");
	/** single line comment: /* comment *&#47; */
	private static final Pattern SINGLE_LINE_COMMENT_2 = Pattern.compile("^(\\/\\*)(.*)(\\*\\/)");
	/** single line comment: /* comment */
	private static final Pattern SINGLE_LINE_COMMENT_3 = Pattern.compile("^(\\/\\*)(.*)");
	/** start of multiline comment: /!* comment */
	private static final Pattern MULTI_LINE_COMMENT_1 = Pattern.compile("^(\\/!\\*)(.*)");
	/** end of multiline comment: comment *!/ */
	private static final Pattern MULTI_LINE_COMMENT_2 = Pattern.compile("(.*)(\\*!\\/)$");
	
	@Override
	protected String processComment(final String comment) {
		String result = SINGLE_LINE_COMMENT_1.matcher(comment).replaceFirst("$2");
		result = SINGLE_LINE_COMMENT_2.matcher(result).replaceFirst("$2");
		result = SINGLE_LINE_COMMENT_3.matcher(result).replaceFirst("$2");
		result = MULTI_LINE_COMMENT_1.matcher(result).replaceFirst("$2");
		result = MULTI_LINE_COMMENT_2.matcher(result).replaceFirst("$1");
		return StringUtils.stripEnd(result, null);
	}
}
