/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.categorize.pl1;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang.StringUtils;

import innowake.mining.server.discovery.categorize.Tokenizer;
import innowake.ndt.core.parsing.util.ParseUtil;

/**
 * Creates tokens from string content.
 * Compared to {@link Tokenizer} this class removes line delimiters and empty strings.
 */
public class Pl1Tokenizer extends Tokenizer {
	
	/**
	 * Creates an instance.
	 * 
	 * @param value the string content to tokenize
	 */
	public Pl1Tokenizer(final String value) {
		super(value);
	}

	@Override
	public List<String> getTokens(final String terminals) {
		final List<String> result = new ArrayList<>();
		for (String token = getToken(terminals); token != null; token = getToken(terminals)) {
			if (StringUtils.isNotBlank(token)) {
				result.add(token);
			}
			/* consume terminal */
			final String terminal = Character.toString(getChar());
			if (StringUtils.isNotBlank(terminal)) {
				result.add(terminal);
			}
		}
		return Collections.unmodifiableList(result);
	}
	
	@Override
	protected String preInit(final String value) {
		final String result;
		final String lineDelimiter = ParseUtil.determineLineDelimiter(value);
		if (StringUtils.isNotEmpty(lineDelimiter)) {
			result = StringUtils.replace(value, lineDelimiter, " ");
		} else {
			result = value;
		}
		return result;
	}
	
}