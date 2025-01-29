/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl.basic;

import java.io.BufferedReader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang.StringUtils;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.core.moduledescription.api.Extractor;

/**
 * {@link Extractor} implementation for BASIC Modules based on block comments.
 * <p>
 * Greedily searches for the first comment block exceeding a certain threshold with regards to the number of lines.
 * If a "Start Documentation" line is found, everything preceding this comment is ignored.
 * If a "End Documentation" line is found, everything after this comment is ignored.
 * The greedy behavior may lead to the incorrect comment block when there is a block exceeding the threshold but a subsequent
 * block would lead to a better description.
 */
public class BasicBlockCommentDescriptionExtractor implements Extractor {
	
	/**
	 * The extractor instance.
	 */
	public static final Extractor INSTANCE = new BasicBlockCommentDescriptionExtractor();

	/**
	 * Threshold for ignoring comment blocks not exceeding the number of lines.
	 * <p>
	 * This is often the case for "banners", which only consist of 3 lines.
	 */
	private static final int COMMENT_LINE_THRESHOLD = 3;

	/**
	 * Matches:
	 * - start of line
	 * - zero or more tabs/spaces
	 * - optional line numbers (captured)
	 * - zero or more tabs/spaces
	 * - exclamation mark indicating a comment
	 * - zero or more chars, excluding \r\n (captured)
	 */
	private static final Pattern BLOCK_COMMENT_PATTERN = Pattern.compile("^[ \\t]*+(\\d*+)[ \\t]*+!([^\\r\\n]*+)");
	
	private static final Logger LOG = LoggerFactory.getLogger(BasicBlockCommentDescriptionExtractor.class);

	@Override
	public Stream<String> extract(final String content) {
		final List<String> comments = new ArrayList<>();
		final List<String> commentBuffer = new ArrayList<>();
		final List<String> lines = new BufferedReader(new StringReader(content)).lines().collect(Collectors.toList());
		
		boolean insideDocumentation = false;
		for (final String line : lines) {
			/* Skip empty lines, consisting only of whitespace */
			if (StringUtils.trimToEmpty(line).isEmpty()) {
				continue;
			}
			
			if (isCommentLine(line)) {
				if (startsDocumentation(line)) {
					LOG.trace(() -> "Start documentation comment found");
					commentBuffer.clear();
					insideDocumentation = true;
				} else if (insideDocumentation && endsDocumentation(line)) {
					LOG.trace(() -> "End documentation comment found");
					insideDocumentation = false;
				} else if (insideDocumentation) {
					LOG.trace(() -> String.format("Adding comment: %s", line));
					comments.add(line);
				}
				commentBuffer.add(line);
			} else {
				insideDocumentation = false;
				if (commentBuffer.size() > COMMENT_LINE_THRESHOLD && comments.isEmpty()) {
					comments.addAll(commentBuffer);
					return comments.stream();
				} else if (commentBuffer.size() <= COMMENT_LINE_THRESHOLD) {
					LOG.trace(() -> "End of comment block but threshold not exceeded");
					commentBuffer.clear();
				}
			}
		}
		
		return comments.stream();
	}
	
	private boolean isCommentLine(final String line) {
		return BLOCK_COMMENT_PATTERN.matcher(line).matches();
	}
	
	private boolean startsDocumentation(final String line) {
		return line.toUpperCase().contains("START") && line.toUpperCase().contains("DOCUMENTATION");
	}
	
	private boolean endsDocumentation(final String line) {
		LOG.trace(() -> String.format("Checking: %s", line));
		return line.toUpperCase().contains("END") && line.toUpperCase().contains("DOCUMENTATION");
	}

}
