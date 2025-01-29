/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.categorize;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import innowake.lib.core.lang.Nullable;

/**
 * Class used to create tokens from string content
 */
public class Tokenizer {
	
	private final char[] value;
	private final int max;
	private int cursor = 0;

	public Tokenizer(final String value) {
		this.value = preInit(value).toCharArray();
		max = this.value.length;
	}

	public @Nullable String getToken(final String terminals) {
		int cur = cursor;
		if (cur >= max) {
			return null;			
		}
		
		final char[] val = value;
		final StringBuilder sb = new StringBuilder();
		char c;
		for (; cur < max; cur++) {
			c = val[cur];
			/* this is an escaped char */
			if (c == '\\') {
				cur++; /* skip the escape char */
				if (cur == max) {
					break;
				}
				c = val[cur]; /* include the escaped char */
			} else if (terminals.indexOf(c) != -1) {
				break;
			}
			sb.append(c);
		}

		cursor = cur;
		return sb.toString();
	}

	public List<String> getTokens(final String terminals) {
		final List<String> result = new ArrayList<>();
		for (String token = getToken(terminals); token != null; token = getToken(terminals)) {
			result.add(token);
			getChar(); /* consume terminal */
		}
		return Collections.unmodifiableList(result);
	}

	public char getChar() {
		final int cur = cursor;
		if (cur < max) {
			cursor = cur + 1;
			return value[cur];
		}
		return '\0'; /* end of value */
	}

	public boolean hasMoreTokens() {
		return cursor < max;
	}

	protected String preInit(final String value) {
		return value;
	}
	
}