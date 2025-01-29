/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared;

import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.module.paramnames.ParameterNamesModule;

import innowake.mining.shared.entities.PojoPrototype;
import innowake.mining.shared.model.IdentifiableEntity;

/**
 * Utility class for JSON handling.
 * @deprecated Use for {@link IdentifiableEntity IdentifiableEntities} only. Will be removed with deletion of {@link IdentifiableEntity}.
 */
@Deprecated(forRemoval = true)
public final class LegacyJsonUtil {
	
	private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper()
			.setSerializationInclusion(Include.NON_NULL)
			.setVisibility(PropertyAccessor.ALL, Visibility.NONE)
			.setVisibility(PropertyAccessor.FIELD, Visibility.ANY)
			.registerModule(new ParameterNamesModule())
			.registerModule(new Jdk8Module())
			.registerModule(new JavaTimeModule())
			.registerModule(new SimpleModule().addSerializer(PojoPrototype.class, new PojoPrototypeSerializer()));
	
	private LegacyJsonUtil() { }

	/**
	 * Returns the {@link ObjectMapper} instance to handle JSON serialization and deserialization.
	 *
	 * @return the {@link ObjectMapper} instance
	 */
	public static ObjectMapper getMapper() {
		return OBJECT_MAPPER;
	}
}
