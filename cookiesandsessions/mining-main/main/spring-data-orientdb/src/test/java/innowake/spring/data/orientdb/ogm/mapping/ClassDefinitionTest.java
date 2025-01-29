/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.spring.data.orientdb.ogm.mapping;

import static org.junit.jupiter.api.Assertions.assertEquals;
import java.util.stream.Stream;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import innowake.mining.shared.springdata.annotations.Embedded;
import innowake.mining.shared.springdata.annotations.Entity;

/**
 * Tests the {@link ClassDefinition} for entity classes
 */
class ClassDefinitionTest {

	/**
	 * Names of the references to test.
	 *
	 * @return the reference names as stream
	 */
	public static Stream<Arguments> getEntityNameTestCases() {
		return Stream.of(
				Arguments.of(Test1Plain.class, "Test1Plain", "No annotations"),
				Arguments.of(Test1Entity.class, "Test1Entity", "@Entity"),
				Arguments.of(Test1Embedded.class, "Test1Embedded", "@Embedded"),
				Arguments.of(Test1EntityEmbedded.class, "Test1EntityEmbedded", "@Entity & @Embedded"),
				Arguments.of(Test2Entity.class, "Test2EntityV1", "@Entity(name)"),
				Arguments.of(Test2Embedded.class, "Test2EmbeddedV1", "@Embedded(name)"),
				Arguments.of(Test3EntityEmbeddedName.class, "Test3EntityEmbeddedName", "@Entity & @Embedded(name)"),
				Arguments.of(Test3EntityNameEmbedded.class, "Test3EntityNameEmbeddedV1", "@Entity(name) & @Embedded"),
				Arguments.of(Test3EntityNameEmbeddedName.class, "Test3EntityNameEmbeddedNameV1", "@Entity(name) & @Embedded(name)")
		);
	}
	
	/**
	 * Tests the {@link ClassDefinition#getEntityName()} method for various entity class with and without the {@link Embedded} or {@link Entity} annotations.
	 *
	 * @param entityClass The entity class to test
	 * @param expectedName The expected entity name
	 * @param description The test description
	 */
	@DisplayName("Test getEntityName()")
	@ParameterizedTest(name = "{2}")
	@MethodSource("getEntityNameTestCases")
	void testGetEntityName(final Class<?> entityClass, final String expectedName, final String description) {
		final ClassDefinition definition = ClassDefinitionMapper.getClassDefinitionMapper().getClassDefinition(entityClass);
		assertEquals(expectedName, definition.getEntityName(), String.format("The entity class name of: %s must match", entityClass.getSimpleName()));
	}

	private static class Test1Plain { }

	@Entity
	private static class Test1Entity { }

	@Embedded
	private static class Test1Embedded { }

	@Entity
	@Embedded
	private static class Test1EntityEmbedded { }
	
	@Entity(name = "Test2EntityV1")
	private static class Test2Entity { }

	@Embedded(name = "Test2EmbeddedV1")
	private static class Test2Embedded { }

	@Entity
	@Embedded(name = "Test3EmbeddedV1")
	private static class Test3EntityEmbeddedName { }

	@Entity(name = "Test3EntityNameEmbeddedV1")
	@Embedded
	private static class Test3EntityNameEmbedded { }

	@Entity(name = "Test3EntityNameEmbeddedNameV1")
	@Embedded(name = "Test3EntityNameEmbeddedNameV2")
	private static class Test3EntityNameEmbeddedName { }
}
