/* Copyright (c) 2023 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.entities;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonAlias;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;

/**
 * TaxonomyCategoryPojoPrototype entity request class.
 */
public class TaxonomyCategoryPojoPrototype implements PojoPrototype {

	public final Definable<Long> id = new Definable<>(false, "taxonomy_category.id");
	public final Definable<EntityId> project = new Definable<>(false, "taxonomy_category.project");
	public final Definable<String> name = new Definable<>(false, "taxonomy_category.name");

	public TaxonomyCategoryPojoPrototype setId(final Long id) {
		this.id.set(id);
		return this;
	}

	@JsonAlias("projectId")
	public TaxonomyCategoryPojoPrototype setProject(final EntityId project) {
		this.project.set(this.project.orElseNonNull(EntityId.VOID).merge(project));
		return this;
	}

	public TaxonomyCategoryPojoPrototype setName(final String name) {
		this.name.set(name);
		return this;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("project", project)
				.append("name", name)
				.toString();
	}
}
