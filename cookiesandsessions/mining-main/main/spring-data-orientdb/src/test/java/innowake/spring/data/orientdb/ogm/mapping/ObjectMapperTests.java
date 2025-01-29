/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.ogm.mapping;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.Arrays;

import org.junit.Test;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.springdata.annotations.Entity;
import innowake.spring.data.orientdb.commons.exception.AccessorMethodNotFoundException;
import innowake.spring.data.orientdb.integration.repository.domain.Employee;
import innowake.spring.data.orientdb.integration.repository.domain.Employee_projects;
import innowake.spring.data.orientdb.integration.repository.domain.Project;

/**
 * Test cases for {@link ObjectMapper} class.
 */
public class ObjectMapperTests {
	
	private static final String LINK_LIST_ERROR = "link list size is wrong";
	private static final String LINKS_SIZE_ERROR = "links size is wrong";

	/**
	 * Test case to validate the object structure creation.
	 */
	@Test
	public void testComputeObjectStruct() {
		final Employee employee = getTestEmployee();

		ObjectStruct actualObjectStruct = ObjectMapper.getObjectMapper().computeObjectStruct(employee);
		assertNotNull("object struct not created", actualObjectStruct);
		assertEquals("edges to remove size is wrong", 0, actualObjectStruct.getEdgesToRemove().size());
		assertEquals("fields size is wrong", 3, actualObjectStruct.getFields().size());
		assertEquals(LINK_LIST_ERROR, 1, actualObjectStruct.getEdgeLinkLists().size());
		assertEquals(LINKS_SIZE_ERROR, 1, actualObjectStruct.getEdgeLinks().size());
	}
	
	/**
	 * Test case to validate the object structure creation.
	 */
	@Test(expected = AccessorMethodNotFoundException.class)
	public void testComputeObjectStructWithError() {
		final User user = new User();
		ObjectMapper.getObjectMapper().computeObjectStruct(user);
	}
	
	@Entity
	private static class User {
		@Nullable private String userName;

		@SuppressWarnings("unused")
		@Nullable String getName() {
			return userName;
		}

		@SuppressWarnings("unused")
		void setUserName(final String userName) {
			this.userName = userName;
		}
	}
	
	private Employee getTestEmployee() {
		final Employee manager = new Employee("user_7", "admin_7", "user_7@deloitte.com");
		final Employee employee = new Employee("user_8", "admin_8", "user_8@deloitte.com", manager);
		final Project project1 = new Project("project_1");
		final Project project2 = new Project("project_2");
		final Employee_projects projects1  =  new Employee_projects(employee, project1);
		final Employee_projects projects2  =  new Employee_projects(employee, project2);
		employee.setProjects(Arrays.asList(projects1, projects2));
		return employee;
	}
}

