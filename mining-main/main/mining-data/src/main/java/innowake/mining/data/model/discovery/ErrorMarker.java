/* Copyright (c) 2018 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.data.model.discovery;


import java.io.Serializable;
import java.util.Objects;
import java.util.UUID;

import innowake.mining.shared.model.AstNodeLocation;
import org.apache.commons.lang.StringUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ErrorMarkerPojoPrototype;
import innowake.mining.shared.entities.ModuleDeadCodePojoPrototype;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;

/**
 * Represent an error that belongs to a model artifact.
 */
public class ErrorMarker implements Serializable {

	@Nullable
	private UUID moduleId;
	protected Severity severity = Severity.WARNING;
	@Nullable
	protected AstNodeLocation moduleLocation;
	protected ErrorKey key = ErrorKey.MODULE_ABORT;
	@Nullable 
	protected String cause;

	public ErrorMarker() {
		this.moduleLocation = new AstNodeLocation();
	}

	public ErrorMarker(final Severity severity, final ErrorKey errorKey, final String cause, @Nullable final AstNodeLocation moduleLocation) {
		this.severity = severity;
		this.key = errorKey;
		this.cause = cause;
		this.moduleLocation = (moduleLocation != null) ? moduleLocation : new AstNodeLocation();
		validate();
	}
	
	public ErrorMarker(final ErrorMarker error) {
		this.setFromError(error);
	}
	@Override
	public String toString() {
		return new StringBuilder().append("\n")
				.append("module: ").append(moduleId).append("\n")
				.append("severity: ").append(severity).append("\n")
				.append("key: ").append(key).append("\n")
				.append("Cause: ").append(cause)
				.append("Module location: ").append(moduleLocation)
				.toString();
	}
	
	@Override
	public boolean equals(@Nullable final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}
		
		final ErrorMarker other = (ErrorMarker) obj;
		return key == other.key &&
				severity == other.severity && 
				Objects.equals(cause, other.cause)
				&& Objects.equals(moduleLocation, other.moduleLocation);
	}

	@Override
	public int hashCode() {
		return Objects.hash(severity, moduleLocation, key, cause);
	}
	
	/**
	 * Get the {@link Severity} of the error.
	 * <br>One of
	 * <ul>
	 * <li>{@link Severity#ERROR}
	 * <li>{@link Severity#WARNING}
	 * </ul>
	 *
	 * @return {@link Severity}
	 */
	public Severity getSeverity() {
		return severity;
	}

	
	/**
	 * Get the {@link ModuleLocation} of the module assigned to this errorlength and 
	 *
	 * @return {@link ModuleLocation}
	 */
	@Nullable
	public AstNodeLocation getModuleLocation() {
		validate();
		return moduleLocation;
	}

	
	/**
	 * Get the {@link ErrorKey} of this error. Keys are used to group errors of the same origin.
	 *
	 * @return {@link ErrorKey}
	 */
	public ErrorKey getKey() {
		return key;
	}

	
	/**
	 * Get the error cause message. This cause contain the detail error information but no stack trace.
	 *
	 * @return The error cause
	 */
	public String getCause() {
		validate();
		return Assert.assertNotNull(cause, "Cause must not be NULL");
	}
	
	public ErrorMarker setFromError(final ErrorMarker error) {
		error.validate();
		this.cause = error.cause;
		this.key = error.key;
		this.severity = error.severity;
		return this;
	}
	
	/**
	 * Set the severity of this error.
	 * Mandatory setting.
	 *
	 * @param severity The severity.
	 * @return {@link ErrorMarker}
	 */
	public ErrorMarker setSeverity(final Severity severity) {
		this.severity = severity;
		return this;
	}
	
	/**
	 * Set the severity to {@link Severity#ERROR}.
	 *
	 * @return {@link ErrorMarker}
	 */
	public ErrorMarker setErrorSeverity() {
		setSeverity(Severity.ERROR);
		return this;
	}
	
	/**
	 * Set the severity to {@link Severity#WARNING}.
	 *
	 * @return {@link ErrorMarker}
	 */
	public ErrorMarker setWarningSeverity() {
		setSeverity(Severity.WARNING);
		return this;
	}
	
	/**
	 * Set the {@link ModuleLocation} where the error occurred.
	 * The moduleLocation is initial at {@code new ModuleLocation(-1, -1)} for not defined.
	 *
	 * @param moduleLocation {@link ModuleLocation}. length and offset must be > -1
	 * @return {@link ErrorMarker}
	 */
	public ErrorMarker setModuleLocation(final AstNodeLocation moduleLocation) {
		this.moduleLocation = moduleLocation;
		return this;
	}

	/**
	 * Sets the {@link ErrorKey}.
	 * Should be the same key for errors with the same origin.
	 * Mandatory setting.
	 *
	 * @param key {@link ErrorKey}
	 * @return {@link ErrorMarker}
	 */
	public ErrorMarker setKey(final ErrorKey key) {
		this.key = key;
		return this;
	}
	
	/**
	 * Set the error cause.
	 * Mandatory setting.
	 *
	 * @param cause the error cause
	 * @return {@link ErrorMarker}
	 */
	public ErrorMarker setCause(final String cause) {
		this.cause = cause;
		return this;
	}

	/**
	 * Set the error cause from a {@link Throwable#getLocalizedMessage()}.
	 * Mandatory setting.
	 *
	 * @param throwable The instance of the {@link Throwable} to set the cause from.
	 * @return {@link ErrorMarker}
	 */
	public ErrorMarker setCause(final Throwable throwable) {
		this.cause = throwable.getLocalizedMessage();
		return this;
	}
	
	/**
	 * Used in conjunction with builder pattern, this validate method can
	 * be used to ensure the object has all the necessary members set and
	 * in addition will assign appropriate default values as needed.
	 * <br>
	 * Should only be used when all builder methods have been called and object
	 * is appropriately "built".
	 *
	 * @return a reference to this object
	 */
	public ErrorMarker validate() {
		if (moduleLocation != null && moduleLocation.getAssembledLength().isPresent() && moduleLocation.getAssembledLength().get() < -1) {
			throw new IllegalArgumentException("The argument 'moduleLocation' length must be >= -1.");
		}
		if (moduleLocation != null && moduleLocation.getAssembledOffset().isPresent() && moduleLocation.getRetracedOffset().get() < -1) {
			throw new IllegalArgumentException("The argument 'moduleLocation' offset must be >= -1.");
		}
		if (StringUtils.isBlank(cause)) {
			throw new IllegalArgumentException("The argument 'cause' must be set.");
		}

		return this;
	}

	/**
	 * Sets the {@link UUID} of the module for which this error got created.
	 *
	 * @param moduleId the module {@link UUID}
	 * @return this instance
	 */
	public ErrorMarker setModuleId(final UUID moduleId) {
		this.moduleId = moduleId;
		return this;
	}

	/**
	 * @return the {@link UUID} of the module for which this error got created
	 */
	@Nullable
	public UUID getModuleId() {
		return moduleId;
	}

	/**
	 * @return a new {@link ModuleDeadCodePojoPrototype} instance containing all values of this {@link ErrorMarker}.
	 */
	public ErrorMarkerPojoPrototype convertToPojoPrototype() {
		final var prototype = new ErrorMarkerPojoPrototype();
		prototype.setSeverity(severity);
		prototype.setKey(key);
		prototype.setCause(Objects.requireNonNull(cause, "Cause must not be null"));

		if (moduleId != null) {
			prototype.setModule(EntityId.of(moduleId));
		}

		final var location = moduleLocation;
		if (location != null && (location.getRootRelativeLength().isPresent() || location.getRootRelativeOffset().isPresent())) {
			prototype.setLocation(location);
		}
		return prototype;
	}
}
