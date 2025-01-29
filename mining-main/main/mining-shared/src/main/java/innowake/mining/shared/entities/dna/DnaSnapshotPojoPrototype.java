/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.dna;

import java.time.Instant;
import java.util.Map;
import java.util.UUID;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.PojoPrototype;

/**
 * {@code dna_snapshot} entity request class.
 */
public class DnaSnapshotPojoPrototype implements PojoPrototype {
	
	public final Definable<UUID> id = new Definable<>(false, "DnaSnapshot.id");
	public final Definable<EntityId> project = new Definable<>(false, "DnaSnapshot.project");
	public final Definable<String> name = new Definable<>(false, "DnaSnapshot.name");
	public final Definable<Instant> updated = new Definable<>(false, "DnaSnapshot.updated");
	public final Definable<Integer> totalModuleCount = new Definable<>(false, "DnaSnapshot.totalModuleCount");
	public final Definable<Map<String, Object>> dnaConfig = new Definable<>(false, "DnaSnapshot.dnaConfig");

	/**
	 * Sets the ID of the {@code dna_snapshot}
	 *
	 * @param id the {@link UUID}
	 * @return this instance for method chaining
	 */
	public DnaSnapshotPojoPrototype setId(final UUID id) {
		this.id.set(id);
		return this;
	}

	/**
	 * Sets the ID of the {@code project} this {@code dna_snapshot} was created for.
	 *
	 * @param projectId the project ID
	 * @return this instance for method chaining
	 */
	public DnaSnapshotPojoPrototype setProjectId(final EntityId projectId) {
		this.project.set(projectId);
		return this;
	}

	/**
	 * Sets the name of this {@code dna_snapshot}.
	 *
	 * @param name the name
	 * @return this instance for method chaining
	 */
	public DnaSnapshotPojoPrototype setName(final String name) {
		this.name.set(name);
		return this;
	}

	/**
	 * Sets the timestamp when this {@code dna_snapshot} got updated.
	 *
	 * @param updated the update timestamp
	 * @return this instance for method chaining
	 */
	public DnaSnapshotPojoPrototype setUpdated(final Instant updated) {
		this.updated.set(updated);
		return this;
	}

	/**
	 * Sets the total number of {@code modules} that are included in this {@code dna_snapshot}.
	 *
	 * @param totalModuleCount the total number of modules
	 * @return this instance for method chaining
	 */
	public DnaSnapshotPojoPrototype setTotalModuleCount(final Integer totalModuleCount) {
		this.totalModuleCount.set(totalModuleCount);
		return this;
	}

	/**
	 * Sets the DNA configuration that was used for generating this {@code dna_snapshot}.
	 *
	 * @param dnaConfig the DNA configuration as map
	 * @return this instance for method chaining
	 */
	public DnaSnapshotPojoPrototype setDnaConfig(final Map<String, Object> dnaConfig) {
		this.dnaConfig.set(dnaConfig);
		return this;
	}
	
}
