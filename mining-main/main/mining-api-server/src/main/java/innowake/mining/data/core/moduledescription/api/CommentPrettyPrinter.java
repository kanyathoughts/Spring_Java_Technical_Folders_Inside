/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.api;

import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Basic pretty printer for comments. Lines with no words will be skipped.
 */
public class CommentPrettyPrinter implements PrettyPrinter {
	
	/**
	 * Singleton instance.
	 */
	public static final PrettyPrinter INSTANCE = new CommentPrettyPrinter();

	private static final Pattern HAS_A_WORD = Pattern.compile(".*\\w+.*", Pattern.DOTALL);
	
	@Override
	public String print(final Stream<String> lines) {
		return lines
				.filter(line -> HAS_A_WORD.matcher(line).matches())
				.map(this::processComment)
				.collect(Collectors.joining("\n"));
	}
	
	/**
	 * Called for every line of comment and can be used to post-process the comment further
	 * by i.e. removing comment start and end markers.
	 * 
	 * @param comment the comment
	 * @return the processed comment
	 */
	protected String processComment(final String comment) {
		return comment;
	}
	
}
