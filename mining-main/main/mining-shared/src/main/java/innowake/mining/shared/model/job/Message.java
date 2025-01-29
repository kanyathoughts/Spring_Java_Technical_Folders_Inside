/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model.job;

import java.io.Serializable;
import java.util.Collection;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;

/**
 * A message that has been written during the execution of a {@code Job} or {@code Task}.
 */
public class Message implements Serializable {
	
	private static final long serialVersionUID = 1L;

	/**
	 * The message severity.
	 */
	public enum Severity {
		INFO,
		WARNING,
		ERROR;
		
		/**
		 * Returns the highest {@link Severity} with {@link #ERROR} being the highest and {@link #INFO} the lowest.
		 * 
		 * @param severities the collection of severities
		 * @return the highest {@link Severity} or {@code null} if {@code severities} is empty
		 */
		@Nullable
		public static Severity getHighest(final Collection<Severity> severities) {
			Severity highestSeverity = null;
			for (final Severity severity : severities) {
				if (severity == Severity.ERROR) {
					return Severity.ERROR;
				}
				if (highestSeverity == null || severity == Severity.WARNING && highestSeverity == Severity.INFO) {
					highestSeverity = severity;
				}
			}
			return highestSeverity;
		}
	}
	
	private final Severity severity;
	private final String text;
	private final int ordinal;
	
	/**
	 * Constructor.
	 * 
	 * @param severity the {@link Severity} of the message
	 * @param text the actual message text
	 * @param ordinal the ordinal number of the message
	 */
	@JsonCreator
	public Message(@JsonProperty("severity") final Severity severity, @JsonProperty("text") final String text,
					@JsonProperty("ordinal") final int ordinal) {
		this.severity = severity;
		this.text = text;
		this.ordinal = ordinal;
	}

	/**
	 * Constructor.
	 * 
	 * @param severity the {@link Severity} of the message
	 * @param text the actual message text
	 */
	public Message(final Severity severity, final String text) {
		this.severity = severity;
		this.text = text;
		this.ordinal = -1;
	}
	
	/**
	 * @return the {@link Severity} of the message
	 */
	public Severity getSeverity() {
		return severity;
	}

	/**
	 * @return the text of the message
	 */
	public String getText() {
		return text;
	}

	/**
	 * @return the ordinal number of the message
	 */
	public int getOrdinal() {
		return ordinal;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(severity).append(text).append(ordinal).toHashCode();
	}
	
	@Override
	public boolean equals(@Nullable final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final Message other = (Message) obj;
		return new EqualsBuilder()
				.append(ordinal, other.ordinal)
				.append(severity, other.severity)
				.append(text, other.text)
				.isEquals();
	}
}
