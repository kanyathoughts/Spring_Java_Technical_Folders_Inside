/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.Map;

import com.fasterxml.jackson.annotation.JsonAlias;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.StatementType;
import innowake.mining.shared.model.Technology;

/**
 * {@code statement} entity request class.
 */
public final class StatementPojoPrototype extends MiningSequentialPojoPrototype<StatementPojoPrototype> {

	public final Definable<EntityId> module = new Definable<>(false, "Statement.module");
	public final Definable<Technology> technology = new Definable<>(false, "Statement.technology");
	public final Definable<StatementType> type = new Definable<>(false, "Statement.type");
	public final Definable<String> text = new Definable<>(false, "Statement.type");
	public final Definable<Map<String, Object>> properties = new Definable<>(true, "Statement.properties");

	public StatementPojoPrototype() {
		super("Statement");
	}

	public StatementPojoPrototype setId(final Long nid) {
		this.nid.set(nid);
		return this;
	}

	@JsonAlias("moduleId")
	public StatementPojoPrototype setModule(final EntityId module) {
		this.module.set(this.module.orElseNonNull(EntityId.VOID).merge(module));
		return this;
	}

	public StatementPojoPrototype setTechnology(final Technology technology) {
		this.technology.set(technology);
		return this;
	}

	public StatementPojoPrototype setType(final StatementType type) {
		this.type.set(type);
		return this;
	}

	public StatementPojoPrototype setText(final String text) {
		this.text.set(text);
		return this;
	}

	public StatementPojoPrototype setProperties(final Map<String, Object> properties) {
		this.properties.set(properties);
		return this;
	}
}
