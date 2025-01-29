/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl.jcl;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang.StringUtils;

import innowake.mining.data.core.moduledescription.api.CommentPrettyPrinter;
import innowake.mining.data.core.moduledescription.api.PrettyPrinter;

/**
 * Pretty printer for JCL comments removing the start marker.
 */
public class JclCommentPrettyPrinter extends CommentPrettyPrinter {
	
	/** The {@link JclCommentPrettyPrinter} instance. */
	@SuppressWarnings("hiding")
	public static final PrettyPrinter INSTANCE = new JclCommentPrettyPrinter();
	
	/** matches the content between "//*" and an optional single or multiple ending "*" */
	private static final Pattern COMMENT_CONTENT = Pattern.compile("(//\\*+)(.*?)(\\*+$|$)");
	
	@Override
	protected String processComment(final String comment) {
		final Matcher matcher = COMMENT_CONTENT.matcher(comment);
		if (matcher.find()) {
			return StringUtils.stripEnd(matcher.group(2), null);
		}
		return comment;
	}
}
