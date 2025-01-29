/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonAlias;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;

/*
	Taxonomy
	+	EntityId		project		project,projectId
	+	TaxonomyTypePojo	type
	+	String			name
 */

/**
 * Taxonomy entity request class.
 */
public class TaxonomyPojoPrototype extends MiningSequentialPojoPrototype<TaxonomyPojoPrototype> {
	
	public final Definable<EntityId> project = new Definable<>(false, "Taxonomy.project");
	public final Definable<UUID> type = new Definable<>(false, "Taxonomy.type");
	public final Definable<String> name = new Definable<>(false, "Taxonomy.name");
	
	public TaxonomyPojoPrototype() {
		super("Taxonomy");
	}
	
	@JsonAlias("projectId")
	public TaxonomyPojoPrototype setProject(final EntityId project) {
		this.project.set(this.project.orElseNonNull(EntityId.VOID).merge(project));
		return this;
	}
	
	public TaxonomyPojoPrototype setType(final UUID typeId) {
		this.type.set(typeId);
		return this;
	}
	
	public TaxonomyPojoPrototype setName(final String name) {
		this.name.set(name);
		return this;
	}
}
