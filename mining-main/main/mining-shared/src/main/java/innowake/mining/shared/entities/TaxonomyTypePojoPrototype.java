/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonAlias;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;

/**
 * TaxonomyTypePojoPrototype entity request class.
 */
public class TaxonomyTypePojoPrototype implements PojoPrototype {

	public final Definable<UUID> id = new Definable<>(false, "taxonomy_category.id");
	public final Definable<EntityId> project = new Definable<>(false, "taxonomy_type.project");
	public final Definable<Long> categoryId = new Definable<>(false, "taxonomy_type.category");
	public final Definable<String> name = new Definable<>(false, "taxonomy_type.name");
	
	public TaxonomyTypePojoPrototype setId(final UUID id) {
		this.id.set(id);
		return this;
	}
	
	@JsonAlias("projectId")
	public TaxonomyTypePojoPrototype setProject(final EntityId project) {
		this.project.set(this.project.orElseNonNull(EntityId.VOID).merge(project));
		return this;
	}

	public TaxonomyTypePojoPrototype setCategoryId(final Long categoryId) {
		this.categoryId.set(categoryId);
		return this;
	}
	
	public TaxonomyTypePojoPrototype setName(final String name) {
		this.name.set(name);
		return this;
	}
}
