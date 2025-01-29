/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.test;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.core.JsonProcessingException;

import innowake.mining.shared.Definable;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.PojoPrototypeSerializer;
import innowake.mining.shared.entities.PojoPrototype;

/**
 * Tests for {@link Definable} and {@link PojoPrototypeSerializer}.
 */
class DefinableSerializationTest {

	public static class DummyPojoPrototype implements PojoPrototype {
		public final Definable<Integer> num = new Definable<>(false, "num");
		public final Definable<String> str = new Definable<>(true, "str");
		public final Definable<List<Object>> lst = new Definable<>(false, "lst");
		public final Definable<Map<String, Object>> map = new Definable<>(false, "map");
	}
	
	private String serialize(final Object o) {
		try {
			return PojoMapper.jsonWriter().writeValueAsString(o);
		} catch (JsonProcessingException e) {
			throw new IllegalStateException(e);
		}
	}
	
	@Test
	void testNotNullable() {
		final DummyPojoPrototype p = new DummyPojoPrototype();
		assertThrows(Definable.ValueNotNullableException.class, () -> p.num.set(null));
	}
	
	@Test
	void testEmpty() {
		final DummyPojoPrototype p = new DummyPojoPrototype();
		assertEquals("{}", serialize(p));
	}
	
	@Test
	void testDefinedOnly() {
		final DummyPojoPrototype p = new DummyPojoPrototype();
		p.num.set(123);
		assertEquals("{\"num\":123}", serialize(p));
	}
	
	@Test
	void testExplicitNull() {
		final DummyPojoPrototype p = new DummyPojoPrototype();
		p.str.set(null);
		assertEquals("{\"str\":null}", serialize(p));
	}
	
	@Test
	void testNestedObjects() {
		final DummyPojoPrototype p = new DummyPojoPrototype();
		p.num.set(123);
		p.str.set("Text");
		p.lst.set(Arrays.asList("abc", 123));
		p.map.set(new HashMap<String, Object>() {{ put("a", "b"); put("c", 123); }});
		assertEquals("{\"num\":123,\"str\":\"Text\",\"lst\":[\"abc\",123],\"map\":{\"a\":\"b\",\"c\":123}}", serialize(p));
	}

}
