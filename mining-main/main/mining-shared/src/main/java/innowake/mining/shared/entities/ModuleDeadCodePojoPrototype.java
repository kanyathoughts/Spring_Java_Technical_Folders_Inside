/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonAlias;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;

/**
 * {@code module_dead_code} entity request class. Formerly know as {@code ExcelSheetDeadCode}.
 */
public final class ModuleDeadCodePojoPrototype implements PojoPrototype {

	public final Definable<EntityId> module = new Definable<>(false, "ModuleDeadCode.module");
	public final Definable<Integer> startingLine = new Definable<>(true, "ModuleDeadCode.startingLine");
	public final Definable<Integer> numberOfLines = new Definable<>(true, "ModuleDeadCode.numberOfLines");
	public final Definable<String> deadCode = new Definable<>(true, "ModuleDeadCode.deadCode");

	@JsonAlias("moduleId")
	public ModuleDeadCodePojoPrototype setModule(final EntityId module) {
		this.module.set(this.module.orElseNonNull(EntityId.VOID).merge(module));
		return this;
	}

	public ModuleDeadCodePojoPrototype setStartingLine(final Integer startingLine) {
		this.startingLine.set(startingLine);
		return this;
	}

	public ModuleDeadCodePojoPrototype setNumberOfLines(final Integer numberOfLines) {
		this.numberOfLines.set(numberOfLines);
		return this;
	}

	public ModuleDeadCodePojoPrototype setDeadCode(final String deadCode) {
		this.deadCode.set(deadCode);
		return this;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("module", module.orElse(null))
				.append("startingLine", startingLine.orElse(null))
				.append("numberOfLines", numberOfLines.orElse(null))
				.append("deadCode", deadCode.orElse(null))
				.toString();
	}
}
