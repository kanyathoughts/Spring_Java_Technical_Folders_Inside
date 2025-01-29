/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.shared;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.PojoPrototype;

/**
 * Jackson Serializer for the {@link Definable} values of a {@link PojoPrototype}.
 * Attributes are placed in the output only if they have a value or are explicitly set {@code null}.
 */
public class PojoPrototypeSerializer extends JsonSerializer<PojoPrototype> {
	
	@Override
	public void serialize(@Nullable final PojoPrototype obj, @Nullable final JsonGenerator gen,
			@Nullable final SerializerProvider serializers) throws IOException {
		final JsonGenerator json = Objects.requireNonNull(gen);
		if (obj != null) {
			json.writeStartObject();
			for (final Field field : obj.getClass().getFields()) {
				if (field.getType().isAssignableFrom(Definable.class)) {
					final Definable<?> value;
					try {
						value = (Definable<?>) field.get(obj);
					} catch (final IllegalArgumentException e) {
						throw e;
					} catch (final IllegalAccessException e) {
						throw new IllegalArgumentException(e);
					}
					if (value.isDefined()) {
						final JsonProperty prop = field.getDeclaredAnnotation(JsonProperty.class);
						json.writeFieldName(prop != null ? prop.value() : field.getName());
						json.writeObject(value.get());
					}
				}
			}
			json.writeEndObject();
		} else {
			json.writeNull();
		}
	}
	
}
