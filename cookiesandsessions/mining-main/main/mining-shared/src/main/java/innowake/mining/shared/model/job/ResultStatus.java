/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model.job;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;

/**
 * Generic status of the result of a {@code Job}.
 */
public class ResultStatus {

	private final Severity severity;
	@Nullable
	private final String message;
	@Nullable
	private final String stackTrace;
	@JsonProperty /* For whatever reason, this has to be here to avoid that the field is being excluded from the JSON */
	private final boolean hasCollectableResult;
	@JsonProperty /* For whatever reason, this has to be here to avoid that the field is being excluded from the JSON */
	private final boolean internalResult;

	/**
	 * Constructor.
	 *
	 * @param severity the {@link Severity} applied to this status
	 * @param message the message of the {@link Throwable} encountered during job or task execution
	 * @param stackTrace the stackTrace of the {@link Throwable} encountered during job or task execution
	 * @param hasCollectableResult {@code true} if the job produced a result that can be requested for further processing; {@code false} otherwise
	 * @param internalResult {@code true} if the job result is application internal and should e.g. not be made downloadable in the UI
	 */
	@JsonCreator
	public ResultStatus(
			@JsonProperty("severity") final Severity severity,
			@JsonProperty("message") @Nullable final String message,
			@JsonProperty("stackTrace") @Nullable final String stackTrace,
			@JsonProperty("hasCollectableResult") final boolean hasCollectableResult,
			@JsonProperty("internalResult") final boolean internalResult) {
		this.severity = severity;
		this.message = message;
		this.stackTrace = stackTrace;
		this.hasCollectableResult = hasCollectableResult;
		this.internalResult = internalResult;
	}

	/**
	 * @return the {@link Severity}
	 */
	public final Severity getSeverity() {
		return severity;
	}

	/**
	 * @return the message of the {@link Throwable} in case an error has occurred
	 */
	@Nullable
	public final String getMessage() {
		return message;
	}

	/**
	 * @return the stack trace of the {@link Throwable} in case an error has occurred
	 */
	@Nullable
	public final String getStackTrace() {
		return stackTrace;
	}

	/**
	 * @return {@code true} if the job produced a result that can be requested for further processing; {@code false} otherwise
	 */
	public final boolean hasCollectableResult() {
		return hasCollectableResult;
	}

	/**
	 * @return {@code true} if the job result is application internal; {@code false} otherwise
	 */
	public final boolean isInternalResult() {
		return internalResult;
	}
	
	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(severity).append(message).append(stackTrace).append(hasCollectableResult).toHashCode();
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
		final ResultStatus other = (ResultStatus) obj;
		return new EqualsBuilder().append(severity, other.severity)
				.append(message, other.message)
				.append(stackTrace, other.stackTrace)
				.append(hasCollectableResult, other.hasCollectableResult)
				.isEquals();
	}

	/**
	 * The severity of a {@code Job} execution status provided with the {@link ResultStatus}.
	 */
	public enum Severity {

		/** The job finished with errors. */
		ERROR,

		/** The job has been canceled. */
		CANCELED,

		/** The job finished with warnings. */
		WARNING,

		/** The job finished successfully without any warnings or errors. */
		OK,

		/** The job finished with an undefined severity. */
		UNDEFINED;

	}

}
