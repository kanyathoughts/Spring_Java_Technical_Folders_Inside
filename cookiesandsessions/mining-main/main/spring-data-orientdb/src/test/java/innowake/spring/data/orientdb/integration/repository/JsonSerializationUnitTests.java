/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository;


import static innowake.lib.core.lang.Assert.assertEqual;
import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

import org.apache.commons.io.FileUtils;
import org.junit.Test;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import innowake.spring.data.orientdb.integration.repository.domain.Employee;
import innowake.spring.data.orientdb.integration.repository.domain.Employee_coaches;
import innowake.spring.data.orientdb.integration.repository.domain.Employee_projectStatus;
import innowake.spring.data.orientdb.integration.repository.domain.Employee_projects;
import innowake.spring.data.orientdb.integration.repository.domain.Employee.Access;
import innowake.spring.data.orientdb.integration.repository.domain.Employee.Designation;
import innowake.spring.data.orientdb.integration.repository.domain.Project;
import innowake.spring.data.orientdb.ogm.proxy.IEntityProxy;

/**
 * Test cases for JSON serialization of the entity objects.
 */
public class JsonSerializationUnitTests extends AbstractEmployeeRepositoryIntegrationTests {
	
	private static final ObjectMapper JSON_MAPPER = new ObjectMapper();
	private static final Path DIRECTORY_PATH = Paths.get("src", "test", "resources", "serialization");
	
	/**
	 * Test case for serialization of an employee object returned.
	 * 
	 * @throws JsonProcessingException exception while converting to JSON string
	 */
	@Test
	public void testJsonSerializationOfEmployee() throws JsonProcessingException {
		final Employee employee = new Employee("user_1", "admin_1", "user_1@deloitte.com");
		final IEntityProxy savedEmployee = (IEntityProxy) assertNotNull(employeeRepository.save(employee));
		final Employee fetchedEmployee = employeeRepository.findById(savedEmployee.__getRid()).orElseThrow(RuntimeException::new);
		final String expectedJson = "{\"first\":\"user_1\",\"last\":\"admin_1\",\"email\":\"user_1@deloitte.com\"}";
		assertNotNull(fetchedEmployee.getRid());
		assertEqual(expectedJson, JSON_MAPPER.writeValueAsString(fetchedEmployee));
	}
	
	/**
	 * Test case for serialization of collection of employee objects returned.
	 * 
	 * @throws IOException exception while converting to JSON string
	 */
	@Test
	public void testJsonSerializationOfEmployees() throws IOException {
		final Employee employee1 = new Employee("user_4", "admin_4", "user_4@deloitte.com");
		final Employee employee2 = new Employee("user_5", "admin_5", "user_5@deloitte.com");
		final Employee employee3 = new Employee("user_6", "admin_6", "user_6@deloitte.com");
		final List<Employee> employees = Arrays.asList(employee1 , employee2, employee3);
		assertNotNull(employeeRepository.saveAll(employees));
		final List<Employee> fetchedEmployees = (List<Employee>) employeeRepository.findAll();
		final File file = new File(DIRECTORY_PATH + File.separator +  "expected-employee-list.txt");
		final String expectedJson = FileUtils.readFileToString(file, StandardCharsets.UTF_8);
		assertEqual(expectedJson, JSON_MAPPER.writeValueAsString(new TreeSet<>(fetchedEmployees)));
	}
	
	/**
	 * Test case for serialization of collection of employee objects returned.
	 * 
	 * @throws IOException exception while converting to JSON string
	 */
	@Test
	public void testJsonSerializationOfComplexEmployees() throws IOException {
		final Employee employee1 = new Employee("user_7", "admin_7", "user_7@deloitte.com");
		final Employee employee2 = new Employee("user_8", "admin_8", "user_8@deloitte.com");
		final Employee employee3 = new Employee("user_9", "admin_9", "user_9@deloitte.com");
		employee1.setEmpDesignation(Designation.CONSULTANT);
		employee3.setEmpDesignation(Designation.MANAGER);
		final Employee_coaches coachee1 = new Employee_coaches(employee1, employee3);
		final Employee_coaches coachee2 = new Employee_coaches(employee2, employee3);
		employee3.setCoaches(Arrays.asList(coachee1, coachee2));
		employee3.setEmpAccess(Arrays.asList(Access.USER, Access.ADMIN));
		final Project project1 = new Project("Mining");
		final Project project2 = new Project("Discovery");
		final Map<String, Employee_projectStatus> projectStatus = new HashMap<>();
		final Employee_projectStatus employee_projects = new Employee_projectStatus(employee2, project1);
		final Employee_projectStatus employee_project2 = new Employee_projectStatus(employee2, project2);
		projectStatus.put("Completed", employee_projects);
		projectStatus.put("Started", employee_project2);
		employee2.setProjectStatus(projectStatus);
		final Employee_projects projects1 = new Employee_projects(employee3, project1);
		final Employee_projects projects2 = new Employee_projects(employee3, project2);
		employee3.setProjects(Arrays.asList(projects1, projects2));
		assertNotNull(employeeRepository.save(employee3));
		final List<Employee> fetchedEmployees = (List<Employee>) employeeRepository.findAll();
		final File file = new File(DIRECTORY_PATH + File.separator +  "expected-complex-employee-list.txt");
		final String expectedJson = FileUtils.readFileToString(file, StandardCharsets.UTF_8);
			assertEqual(expectedJson, JSON_MAPPER.writeValueAsString(new TreeSet<>(fetchedEmployees)));
	}
	
}
