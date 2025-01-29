/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.testing;

import java.nio.ByteOrder;
import java.util.UUID;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.BinaryValue;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.hashing.CityHash;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Source object for testing purposes.
 */
public class SourcePojoDummy extends SourcePojoPrototype {
	
	public final Definable<Long> nid = new Definable<>(false, "Source.nid");
	
	public SourcePojoDummy setNid(Long nid) {
		this.nid.set(nid);
		return this;
	}
	
	public static SourcePojo build(final BuildingConsumer<SourcePojoDummy> builder) {
		return builder.prepare(new SourcePojoDummy()).build();
	}
	
	public SourcePojo build() {
		return new SourcePojo(
				uid.orElseNonNull(UUID::randomUUID),
				nid.orElseNonNull(-1l),
				project.orElseNonNull(() -> EntityId.of(-1l)),
				name.orElseNonNull("DUMMY"),
				path.orElseNonNull("DUMMY.UNKNOWN"),
				technology.orElseNonNull(Technology.UNKNOWN),
				type.orElseNonNull(Type.UNKNOWN),
				metaDataRevision.orElseNonNull(1L),
				contentRevision.orElseNonNull(1L),
				contentHash.orElseNonNull(() -> content.isDefined()
						? new BinaryValue(CityHash.cityHash128(ByteOrder.BIG_ENDIAN, content.getNonNull().get())) : BinaryValue.EMPTY),
				() -> content.orElseNonNull(BinaryString.EMPTY),
				new CustomPropertiesMap(customProperties.orElse(null)));
	}
	
}
