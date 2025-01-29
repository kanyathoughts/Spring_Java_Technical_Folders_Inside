/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared;

import com.fasterxml.jackson.annotation.PropertyAccessor;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectReader;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.fasterxml.jackson.module.paramnames.ParameterNamesModule;

import innowake.mining.shared.entities.PojoPrototype;
import innowake.mining.shared.model.FlexibleLocalDateTimeDeserializer;

public final class PojoMapper {
	
	private static final CachingSupplier<ObjectMapper> DEFAULT_MAPPER = new CachingSupplier<>(() -> new ObjectMapper()
			.setSerializationInclusion(Include.NON_ABSENT)
			.setVisibility(PropertyAccessor.ALL, Visibility.NONE)
			.setVisibility(PropertyAccessor.GETTER, Visibility.PUBLIC_ONLY)
			.setVisibility(PropertyAccessor.IS_GETTER, Visibility.PUBLIC_ONLY)
			.setVisibility(PropertyAccessor.CREATOR, Visibility.PUBLIC_ONLY)
			.setVisibility(PropertyAccessor.SETTER, Visibility.PUBLIC_ONLY)
			.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
			.registerModule(new ParameterNamesModule())
			.registerModule(new Jdk8Module())
			.registerModule(new JavaTimeModule()
					.addDeserializer(LocalDateTime.class, new FlexibleLocalDateTimeDeserializer()))
			.registerModule(new SimpleModule()
					.addSerializer(PojoPrototype.class, new PojoPrototypeSerializer())));
	
	private PojoMapper() { }
	
	@SuppressWarnings("unchecked")
	public static Map<String, Object> convert(final Object obj) {
		return DEFAULT_MAPPER.get().convertValue(obj, Map.class);
	}
	
	public static <T> T convert(final Map<String, Object> map, Class<T> cls) {
		return DEFAULT_MAPPER.get().convertValue(map, cls);
	}
	
	public static ObjectWriter jsonWriter() {
		return DEFAULT_MAPPER.get().writer();
	}
	
	public static ObjectReader jsonReader() {
		return DEFAULT_MAPPER.get().reader();
	}
	
	public static ObjectReader jsonReaderFor(final Class<?> cls) {
		return DEFAULT_MAPPER.get().readerFor(cls);
	}
	
	public static ObjectReader jsonReaderFor(final TypeReference<?> type) {
		return DEFAULT_MAPPER.get().readerFor(type);
	}
	
	public static ObjectReader jsonListReaderFor(final Class<?> elementClass) {
		final var mapper = DEFAULT_MAPPER.get();
		return mapper.readerFor(mapper.getTypeFactory().constructCollectionType(List.class, elementClass));
	}
	
	public static <T> List<T> readList(final Class<T> elementClass, final String json) {
		try {
			return jsonListReaderFor(elementClass).readValue(json);
		} catch (Exception e) {
			throw new IllegalStateException(e);
		}
	}
	
	@SuppressWarnings("unchecked")
	public static <T> Map<String, T> readMap(final String json) {
		try {
			return DEFAULT_MAPPER.get().readValue(json, Map.class);
		} catch (JsonProcessingException e) {
			throw new IllegalArgumentException(e);
		}
	}
	
}
