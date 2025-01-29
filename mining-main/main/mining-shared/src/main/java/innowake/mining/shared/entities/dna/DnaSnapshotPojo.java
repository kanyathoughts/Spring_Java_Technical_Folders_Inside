/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.dna;

import java.time.Instant;
import java.util.Map;
import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.mining.shared.access.EntityId;

/**
 * {@code dna_snapshot} entity class.
 */
public class DnaSnapshotPojo {
	
	private final UUID id;
	private final EntityId project;
	private final String name;
	private final Instant updated;
	private final int totalModuleCount;
	private final Map<String, Object> dnaConfig;

	/**
	 * Constructor.
	 * 
	 * @param id the id of this {@code dna_snapshot}
	 * @param project the @code project} this {@code dna_snapshot} was created for
	 * @param name the name of this {@code dna_snapshot}
	 * @param updated the timestamp when this {@code dna_snapshot} got updated
	 * @param totalModuleCount the total number of modules that are included in this {@code dna_snapshot}
	 * @param dnaConfig DNA configuration that was used for generating this {@code dna_snapshot}
	 */
	@JsonCreator
	public DnaSnapshotPojo(@JsonProperty final UUID id,
						   @JsonProperty final EntityId project,
						   @JsonProperty final String name,
						   @JsonProperty final Instant updated,
						   @JsonProperty final int totalModuleCount,
						   @JsonProperty final Map<String, Object> dnaConfig) {
		this.id = id;
		this.project = project;
		this.name = name;
		this.updated = updated;
		this.totalModuleCount = totalModuleCount;
		this.dnaConfig = dnaConfig;
	}

	/**
	 * @return the ID of the {@code dna_snapshot}
	 */
	public UUID getId() {
		return id;
	}

	/**
	 * @return the ID of the {@code project} this {@code dna_snapshot} was created for
	 */
	public EntityId getProject() {
		return project;
	}

	/**
	 * @return the name of this {@code dna_snapshot}
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return the timestamp when this {@code dna_snapshot} got updated
	 */
	public Instant getUpdated() {
		return updated;
	}

	/**
	 * @return the total number of {@code modules} that are included in this {@code dna_snapshot}
	 */
	public int getTotalModuleCount() {
		return totalModuleCount;
	}

	/**
	 * @return the DNA configuration that was used for generating this {@code dna_snapshot}.
	 */
	public Map<String, Object> getDnaConfig() {
		return dnaConfig;
	}

}
