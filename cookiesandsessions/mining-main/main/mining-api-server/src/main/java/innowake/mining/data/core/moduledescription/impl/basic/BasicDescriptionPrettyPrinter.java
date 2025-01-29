/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.moduledescription.impl.basic;

import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang.StringUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.moduledescription.api.PrettyPrinter;

/**
 * Pretty prints the BASIC Module Description by removing the following from the comment lines:
 * <ul>
 *  <li>everything in front of the line until and including the exclamation mark
 *  <li>any trailing whitespace
 *  <li>an optional exclamation mark at the end of the line
 *  
 */
public class BasicDescriptionPrettyPrinter implements PrettyPrinter {

	/**
	 * A singleton instance of the  pretty printer.
	 */
	public static final PrettyPrinter INSTANCE = new BasicDescriptionPrettyPrinter();
	
	private static final Pattern HAS_A_WORD = Pattern.compile("[^\\w]*?\\w++.*");
	
	private BasicDescriptionPrettyPrinter() {}

	@Override
	public String print(final Stream<String> lines) {
		return lines
				.filter(line -> HAS_A_WORD.matcher(line).matches())
				.map(BasicDescriptionPrettyPrinter::stripLeadingUntilExclamationMark)
				.map(BasicDescriptionPrettyPrinter::stripTrailingWhitespace)
				.map(BasicDescriptionPrettyPrinter::removeOptionalExclamationMark)
				.collect(Collectors.joining("\n"));
	}
	
	@Nullable
	private static String stripLeadingUntilExclamationMark(@Nullable final String string) {
		if (string == null) {
			return null;
		}
		int length = string.length();
		int index = 0;
		for (; index < length; index++) {
			if (string.charAt(index) == '!') {
				break;
			}
		}
		/* The '!' is also removed */
		return string.substring(index + 1, length);
	}

	@Nullable
	private static String stripTrailingWhitespace(@Nullable final String string) {
		if (string == null) {
			return null;
		}
		int length = string.length();
		for (; length > 0; length--) {
			if ( ! Character.isWhitespace(string.charAt(length - 1))) {
				break;
			}
		}
		return string.substring(0, length);	
	}
	
	private static String removeOptionalExclamationMark(final String string) {
		return StringUtils.removeEnd(string, "!");
	}
}
