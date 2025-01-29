/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.ogm.mapping.util;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.lang.reflect.Field;

import org.junit.Test;

import innowake.spring.data.orientdb.integration.repository.domain.Employee;

/**
 * Test cases to validate if the edges are created with value passed to @Relationship annotation.
 */
public class RelationshipUtilsTests {

	/**
	 * Tests custom relationship name, HasReportsTo when passed to @Relationship annotation.
	 * 
	 * @throws NoSuchFieldException if field is not present
	 * @throws SecurityException indicates a security violation
	 */
	@Test
	public void testGetGraphRelationshipNameIfExists() throws NoSuchFieldException {
		final Field field = Employee.class.getDeclaredField("reportsTo");
		assertEquals("HasReportsTo", RelationshipUtils.getGraphRelationshipName(field), "doesn't match");
	}

	/**
	 * Tests default relationship name when not value is passed to @Relaionship annotation 
	 * or if annotation is not present.
	 * 
	 * @throws NoSuchFieldException if field is not present
	 * @throws SecurityException indicates a security violation
	 */
	@Test
	public void testGetGraphRelationshipNameIfDeosntExist() throws NoSuchFieldException {
		final Field field2 = Employee.class.getDeclaredField("projects");
		assertEquals("Employee_projects", RelationshipUtils.getGraphRelationshipName(field2), "doesn't match");
	}
}
