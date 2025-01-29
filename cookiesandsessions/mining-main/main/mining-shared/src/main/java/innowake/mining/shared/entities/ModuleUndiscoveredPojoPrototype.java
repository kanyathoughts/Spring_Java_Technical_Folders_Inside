package innowake.mining.shared.entities;

import org.apache.commons.lang3.builder.ToStringBuilder;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.EntityId;

/**
 * {@code module_undiscovered} entity request class. Formerly know as {@code ExcelSheetUndiscovered}.
 */
public class ModuleUndiscoveredPojoPrototype extends MiningPojoPrototype<ModuleUndiscoveredPojoPrototype> {

	public final Definable<EntityId> project = new Definable<>(false, "ModuleUndiscovered.project");
	public final Definable<String> name = new Definable<>(false, "ModuleUndiscovered.name");
	public final Definable<String> path = new Definable<>(false, "ModuleUndiscovered.path");

	public ModuleUndiscoveredPojoPrototype() {
		super("ModuleUndiscovered");
	}

	public ModuleUndiscoveredPojoPrototype setProject(final EntityId project) {
		this.project.set(this.project.orElseNonNull(EntityId.VOID).merge(project));
		return this;
	}

	public ModuleUndiscoveredPojoPrototype setName(final String name) {
		this.name.set(name);
		return this;
	}

	public ModuleUndiscoveredPojoPrototype setPath(final String path) {
		this.path.set(path);
		return this;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("project", project.orElse(null))
				.append("name", name.orElse(null))
				.append("path", path.orElse(null))
				.toString();
	}
}