/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.shared.model;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;

import innowake.lib.core.api.lang.Nullable;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.DateTimeParseException;

public class FlexibleLocalDateTimeDeserializer extends JsonDeserializer<LocalDateTime> {

	private static final String YEAR_MONTH_DATE = "yyyy-MM-dd";
	private static final String SEPERATOR = "'T'";
	private static final String HOURS = "HH";
	private static final String MINUTES = ":mm";
	private static final String SECONDS = ":ss";
	private static final String MILL_SECONDS = ".SSS";
	private static final String OFFSET = "+HH:mm";
	private static final DateTimeFormatter FORMATTER = new DateTimeFormatterBuilder()
			.appendPattern(YEAR_MONTH_DATE)
			.optionalStart().appendPattern(SEPERATOR).optionalEnd()
			.optionalStart().appendPattern(HOURS).optionalEnd()
			.optionalStart().appendPattern(MINUTES).optionalEnd()
			.optionalStart().appendPattern(SECONDS).optionalEnd()
			.optionalStart().appendPattern(MILL_SECONDS).optionalEnd()
			.optionalStart().appendOffset(OFFSET, "Z").optionalEnd()
			.toFormatter();

	@Override
	public @Nullable LocalDateTime deserialize(final @Nullable JsonParser jsonParser, final @Nullable DeserializationContext deserializationContext)
			throws IOException {
		final @Nullable String dateString = jsonParser != null ? jsonParser.getText() : null;
		if (dateString != null) {
			try {
				return LocalDateTime.parse(dateString, DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
			} catch (final DateTimeParseException e) {
				try {
					return LocalDateTime.parse(dateString, FORMATTER);
				} catch (final DateTimeParseException ignored) {
					/* Both parsing attempts failed, return null */
					return null;
				}
			}
		}
		return null;
	}

}
	
