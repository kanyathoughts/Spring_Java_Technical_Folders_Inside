/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.entities.functionalblocks;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataType;

import java.time.Instant;
import java.util.Objects;
import java.util.Optional;

/**
 * Stores links to the entities a functional block was generated from.
 */
@MiningDataType(name = "GeneratedFrom")
public class GeneratedFrom {

	@Nullable
	private final String moduleLinkHash;
	@Nullable
	private final String moduleContentHash;
	@Nullable
	private final String moduleDependencyHash;
	@Nullable
	private final EntityId annotationId;
	@Nullable
	private final Instant contentChanged;
	@Nullable
	private final Instant missingSince;
	@Nullable
	private final Instant dependencyChanged;

	/**
	 * Creates {@code GeneratedFrom} information for a block created from a Module.
	 * @param moduleLinkHash the "link hash" of the module
	 * @param moduleContentHash the "content hash" of the module at the time the block was created
	 * @param moduleDependencyHash the "dependency hash" of the module at the time the block was created
	 * @return {@code GeneratedFrom} data for the block
	 */
	public static GeneratedFrom fromModule(final String moduleLinkHash, final String moduleContentHash, final String moduleDependencyHash) {
		return new GeneratedFrom(moduleLinkHash, moduleContentHash, null, null, null, moduleDependencyHash, null);
	}

	/**
	 * Creates {@code GeneratedFrom} information for a block created from an Annotation.
	 * @param annotationId the id of the Annotation the block was created from
	 * @return {@code GeneratedFrom} data for the block
	 */
	public static GeneratedFrom fromAnnotation(final EntityId annotationId) {
		return new GeneratedFrom(null, null, annotationId, null, null, null, null);
	}

	@JsonCreator
	public GeneratedFrom(@JsonProperty("moduleLinkHash") @Nullable final String moduleLinkHash,
			@JsonProperty("moduleContentHash") @Nullable final String moduleContentHash,
			@JsonProperty("annotationId") @Nullable final EntityId annotationId,
			@JsonProperty("contentChanged") @Nullable final Instant contentChanged,
			@JsonProperty("missingSince") @Nullable final Instant missingSince,
			@JsonProperty("moduleDependencyHash") @Nullable final String moduleDependencyHash,
			@JsonProperty("dependencyChanged") @Nullable final Instant dependencyChanged) {
		this.moduleLinkHash = moduleLinkHash;
		this.moduleContentHash = moduleContentHash;
		this.annotationId = annotationId;
		this.contentChanged = contentChanged;
		this.missingSince = missingSince;
		this.moduleDependencyHash = moduleDependencyHash;
		this.dependencyChanged = dependencyChanged;
	}

	public GeneratedFrom withModuleContentHash(final String moduleContentHash) {
		return new GeneratedFrom(moduleLinkHash, moduleContentHash, annotationId, contentChanged, missingSince, moduleDependencyHash, dependencyChanged);
	}

	public GeneratedFrom withContentChanged(final Instant contentChanged) {
		return new GeneratedFrom(moduleLinkHash, moduleContentHash, annotationId, contentChanged, missingSince, moduleDependencyHash, dependencyChanged);
	}

	public GeneratedFrom withMissingSince(final Instant missingSince) {
		return new GeneratedFrom(moduleLinkHash, moduleContentHash, annotationId, contentChanged, missingSince, moduleDependencyHash, dependencyChanged);
	}

	public GeneratedFrom withModuleDependencyHash(final String moduleDependencyHash) {
		return new GeneratedFrom(moduleLinkHash, moduleContentHash, annotationId, contentChanged, missingSince, moduleDependencyHash, dependencyChanged);
	}

	public GeneratedFrom withDependencyChanged(final Instant dependencyChanged) {
		return new GeneratedFrom(moduleLinkHash, moduleContentHash, annotationId, contentChanged, missingSince, moduleDependencyHash, dependencyChanged);
	}

	/**
	 * Returns the link hash of the module the block was generated from.
	 * @return the link hash of the module
	 */
	public Optional<String> getModuleLinkHash() {
		return Optional.ofNullable(moduleLinkHash);
	}

	/**
	 * Returns the content hash of the module that the block was generated from, at the time when the block was generated.
	 * @return the content hash of the module at the time the block was generated
	 */
	public Optional<String> getModuleContentHash() {
		return Optional.ofNullable(moduleContentHash);
	}

	/**
	 * Returns the id of the Annotation from which this block was generated.
	 * @return the id of the Annotation
	 */
	public Optional<EntityId> getAnnotationId() {
		return Optional.ofNullable(annotationId);
	}

	/**
	 * Returns the timestamp when the content of the module was last changed since the block was generated. Returns {@link Optional#empty()} if the content
	 * hasn't changed since the block was generated.
	 * @return the timestamp of the last content change or empty
	 */
	public Optional<Instant> getContentChanged() {
		return Optional.ofNullable(contentChanged);
	}

	/**
	 * Returns the timestamp since when the module was detected to be missing. Returns {@link Optional#empty()} if the module is not missing.
	 * @return the timestamp since the module went missing or {@code null}
	 */
	public Optional<Instant> getMissingSince() {
		return Optional.ofNullable(missingSince);
	}

	/**
	 * Returns the dependency hash of the module that the block was generated from, at the time when the block was generated.
	 * @return the dependency hash of the module at the time the block was generated
	 */
	public Optional<String> getModuleDependencyHash() {
		return Optional.ofNullable(moduleDependencyHash);
	}

	/**
	 * Returns the timestamp when the dependency of the module was last changed since the block was generated. Returns {@link Optional#empty()} if the content
	 * hasn't changed since the block was generated.
	 * @return the timestamp of the last content change or empty
	 */
	public Optional<Instant> getDependencyChanged() {
		return Optional.ofNullable(dependencyChanged);
	}

	/**
	 * Returns whether the module from which the block was generated is currently missing from the project.
	 * @return {@code true} if the module from which the block was generated is missing
	 */
	public boolean isMissing() {
		return missingSince != null;
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) {
			return true;
		}
		if ( ! (o instanceof GeneratedFrom)) {
			return false;
		}
		final GeneratedFrom that = (GeneratedFrom) o;
		return Objects.equals(moduleLinkHash, that.moduleLinkHash)
				&& Objects.equals(moduleContentHash, that.moduleContentHash)
				&& Objects.equals(annotationId, that.annotationId)
				&& Objects.equals(contentChanged, that.contentChanged)
				&& Objects.equals(missingSince, that.missingSince)
				&& Objects.equals(moduleDependencyHash, that.moduleDependencyHash)
				&& Objects.equals(dependencyChanged, that.dependencyChanged);
	}

	@Override
	public int hashCode() {
		return Objects.hash(moduleLinkHash, moduleContentHash, annotationId, contentChanged, missingSince, moduleDependencyHash, dependencyChanged);
	}

	@Override
	public String toString() {
		return "GeneratedFrom{" +
				"moduleLinkHash='" + moduleLinkHash + '\'' +
				", moduleContentHash='" + moduleContentHash + '\'' +
				", annotationId=" + annotationId +
				", contentChanged=" + contentChanged +
				", missingSince=" + missingSince +
				", moduleDependencyHash='" + moduleDependencyHash + '\'' +
				", dependencyChanged=" + dependencyChanged +
				'}';
	}
}
