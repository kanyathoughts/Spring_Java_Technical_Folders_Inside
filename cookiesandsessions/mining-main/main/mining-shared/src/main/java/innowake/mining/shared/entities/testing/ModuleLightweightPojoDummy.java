/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.testing;

import java.util.UUID;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * ModuleLightweight for testing purposes.
 */
public class ModuleLightweightPojoDummy extends ModulePojoPrototype {
	
	public ModuleLightweightPojoDummy prepare(final BuildingConsumer<ModuleLightweightPojoDummy> builder) {
		return builder.prepare(this);
	}
	
	public ModuleLightweightPojo build() {
		return build(this);
	}
	
	public static ModuleLightweightPojo build(final ModulePojoPrototype proto) {
		final Representation representation = proto.representation.orElse(null);
		final Identification identification = proto.identification.orElseNonNull(Identification.IDENTIFIED);
		return new ModuleLightweightPojo(
			proto.uid.orElseNonNull(UUID::randomUUID),											/* uid */
			proto.nid.orElseNonNull(-1L), 														/* nid */
			proto.project.orElse(EntityId.VOID), null, null,									/* project */
			proto.name.orElseNonNull("DUMMY"),													/* name */
			proto.path.orElseNonNull("DUMMY.UNKNOWN"),											/* path */
			proto.technology.orElseNonNull(Technology.UNKNOWN),									/* technology */
			proto.type.orElseNonNull(Type.UNKNOWN),												/* type */
			proto.linkHash.orElseNonNull(""),													/* linkHash */
			representation == null ? null : representation.name(),								/* representation */
			identification == Identification.IDENTIFIED,										/* identification */
			null, null, null,																	/* parent */
			null																				/* parentPath */
		);
	}
}
