/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;
import java.util.List;

import org.apache.commons.collections4.IteratorUtils;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import com.orientechnologies.orient.core.record.ODirection;
import com.orientechnologies.orient.core.record.OEdge;
import com.orientechnologies.orient.core.record.OVertex;
import com.orientechnologies.orient.core.sql.executor.OResultSet;

import innowake.lib.core.lang.Assert;
import innowake.spring.data.orientdb.integration.repository.domain.Employee;
import innowake.spring.data.orientdb.integration.repository.domain.Employee_coaches;
import innowake.spring.data.orientdb.integration.repository.domain.HasReportsTo;
import innowake.spring.data.orientdb.integration.repository.domain.Project;
import innowake.spring.data.orientdb.ogm.proxy.IEntityProxy;

/**
 * Test cases for all the save and update operations on {@link Employee} entity.
 */
public class EmployeeRepositorySaveTests extends AbstractEmployeeRepositoryIntegrationTests {

	@Autowired
	private EmployeeRepositoryTestService employeeRepositoryTestService;
	
	@Value("${spring.datasource.hikari.maximum-pool-size}") 
	private int maxPoolSize;
	
	/**
	 * Tests simple save operation.
	 */
	@Test
	public void saveEmployees() {
		final Employee employee1 = new Employee("user_1", "admin_1", "user_1@deloitte.com");
		final Employee employee2 = new Employee("user_2", "admin_2", "user_2@deloitte.com");
		final Employee employee3 = new Employee("user_3", "admin_3", "user_3@deloitte.com");
		employeeRepository.save(employee1);
		assertNotNull(employeeRepository.save(employee2));
		assertNotNull(employeeRepository.save(employee3));
		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved  = getVertices(resultSet);
		assertTrue(verticesSaved.size() == 3);
		assertTrue(getAllEdges(verticesSaved).isEmpty());
	}

	/**
	 * Tests save all operation.
	 */
	@Test
	public void saveAllEmployees() {
		final Employee employee1 = new Employee("user_4", "admin_4", "user_4@deloitte.com");
		final Employee employee2 = new Employee("user_5", "admin_5", "user_5@deloitte.com");
		final Employee employee3 = new Employee("user_6", "admin_6", "user_6@deloitte.com");
		final List<Employee> employees = Arrays.asList(employee1, employee2, employee3);
		assertNotNull(employeeRepository.saveAll(employees));
		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved = getVertices(resultSet);
		assertTrue(verticesSaved.size() == 3);
		assertTrue(getAllEdges(verticesSaved).isEmpty());
		assertEquals(3, employeeRepository.count());
	}

	/**
	 * Tests save operation with for an entity which contains 'has-a' relationship with another entity.
	 */
	@Test
	public void saveEmployeeWithRelation() {
		Employee manager = new Employee("user_7", "admin_7", "user_7@deloitte.com");
		Employee employee = new Employee("user_8", "admin_8", "user_8@deloitte.com", manager);
		assertNotNull(employeeRepository.save(employee));
		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved = getVertices(resultSet);
		assertTrue(verticesSaved.size() == 2);
		final List<OEdge> edges = getAllEdges(verticesSaved);
		assertEquals(1, edges.size());
		assertEquals("user_8", edges.get(0).getFrom().getProperty(PROPERTY_1).toString());
		assertEquals("user_7", edges.get(0).getTo().getProperty(PROPERTY_1).toString());
	}

	/**
	 * Tests save operation with for an entity which contains list of 'has-a' relationship with another entity.
	 */
	@Test
	public void saveEmployeeWithProjects() {
		final Project project1 = new Project("project_1");
		final Project project2 = new Project("project_2");
		final Employee employee = new Employee("user_9", "admin_9", "user_9@deloitte.com", Arrays.asList(project1, project2));
		assertNotNull(employeeRepository.save(employee));
		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final OResultSet resultSet2 = queryWithEntityName("Project");
		final List<OVertex> verticesSaved = getVertices(resultSet);
		final List<OVertex> projectsSaved = getVertices(resultSet2);
		assertNotNull(getVertexFromProjectName("project_1", projectsSaved));
		assertNotNull(getVertexFromProjectName("project_2", projectsSaved));
		assertTrue(verticesSaved.size() == 1);
		assertFalse(getAllEdges(verticesSaved).isEmpty());
		assertEquals("user_9", verticesSaved.get(0).getProperty(PROPERTY_1).toString());
	}


	/**
	 * Tests save operation with for an entity which contains 'has-a' relationship with another entity.
	 */
	@Test
	public void saveEmployeeWithEmployees() {
		final Employee employee1 = new Employee("user_11", "admin_11", "user_11@deloitte.com");
		final Employee employee2 = new Employee("user_12", "admin_12", "user_12@deloitte.com");
		final Employee employee3 = new Employee("user_13", "admin_13", "user_13@deloitte.com");
		final Employee employee = new Employee("user_10", "admin_10", "user_10@deloitte.com");
		final Employee_coaches coachee1 = new Employee_coaches(employee1, employee);
		final Employee_coaches coachee2 = new Employee_coaches(employee2, employee);
		final Employee_coaches coachee3 = new Employee_coaches(employee3, employee);
		employee.setCoaches(Arrays.asList(coachee1, coachee2, coachee3));
		assertNotNull(employeeRepository.save(employee));
		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved = getVertices(resultSet);
		final List<OEdge> edgesSaved = getAllEdges(verticesSaved);
		final List<OEdge> edgesToCoaches = getEdgeFromVertex(getVertexFromUserName("user_10", verticesSaved).orElseThrow(RuntimeException::new));
		assertEquals("user_10", edgesToCoaches.get(0).getFrom().getProperty(PROPERTY_1).toString());
		assertEquals("user_11", edgesToCoaches.get(0).getTo().getProperty(PROPERTY_1).toString());
		assertEquals("user_12", edgesToCoaches.get(1).getTo().getProperty(PROPERTY_1).toString());
		assertEquals("user_13", edgesToCoaches.get(2).getTo().getProperty(PROPERTY_1).toString());
		assertEquals(4, verticesSaved.size());
		assertEquals(3, edgesSaved.size());
		assertEquals(3, edgesToCoaches.size());
		assertEquals(4, employeeRepository.count());
	}


	/**
	 * Test case for simple update functionality.
	 */
	@Test
	public void testUpdateSimpleTest() {
		final Employee emp1 = new Employee("user_11", "admin_11", "user_11@deloitte.com");
		final Employee savedEmployee1 = employeeRepository.save(emp1);
		savedEmployee1.setFirst("user_15");
		final Employee updatedEmployee = employeeRepository.save(savedEmployee1);
		assertEquals("user_15", updatedEmployee.getFirst());
	}

	/**
	 * Tests update by setting a single has-a relationship link to null.
	 */
	@Test
	public void testUpdate() {
		final Employee coach1 = new Employee("user_11", "admin_11", "user_11@deloitte.com");
		final Employee mainEmployee = new Employee("user_10", "admin_10", "user_10@deloitte.com");
		final HasReportsTo reports = new HasReportsTo(coach1, mainEmployee);
		mainEmployee.setReportsTo(reports);

		final Employee employeeSaved = Assert.assertNotNull(employeeRepository.save(mainEmployee));

		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved = getVertices(resultSet);

		final String ridOfMainEmployee = getVertexFromUserName(employeeSaved.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();
		final String ridOfCoach1 = getVertexFromUserName(coach1.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();

		assertTrue(employeeRepository.existsById(ridOfMainEmployee));
		assertTrue(employeeRepository.existsById(ridOfCoach1));
		Assert.assertNotNull(((IEntityProxy) employeeSaved).__getElement());

		employeeSaved.setReportsTo(null);
		employeeRepository.save(employeeSaved);

		final OResultSet resultSetUpdated = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSavedUpdated = getVertices(resultSetUpdated);
		final OVertex vertexOfMainEmployee = getVertexFromUserName(employeeSaved.getFirst(), verticesSavedUpdated).orElseThrow(RuntimeException::new);

		assertTrue(IteratorUtils.toList(vertexOfMainEmployee.getEdges(ODirection.OUT).iterator()).isEmpty());
	}

	/**
	 * Tests update by updating a link list of has-a relationship to null.
	 */
	@Test
	public void testUpdate2() {
		final Employee coach1 = new Employee("user_11", "admin_11", "user_11@deloitte.com");
		final Employee coach2 = new Employee("user_12", "admin_12", "user_12@deloitte.com");
		final Employee mainEmployee = new Employee("user_10", "admin_10", "user_10@deloitte.com");
		final Employee_coaches coachee1 = new Employee_coaches(coach1, mainEmployee);
		final Employee_coaches coachee2 = new Employee_coaches(coach2, mainEmployee);
		mainEmployee.setCoaches(Arrays.asList(coachee1, coachee2));

		final Employee employeeSaved = Assert.assertNotNull(employeeRepository.save(mainEmployee));

		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved = getVertices(resultSet);

		final String ridOfMainEmployee = getVertexFromUserName(employeeSaved.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();
		final String ridOfCoach1 = getVertexFromUserName(coach1.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();

		assertTrue(employeeRepository.existsById(ridOfMainEmployee));
		assertTrue(employeeRepository.existsById(ridOfCoach1));
		final OVertex vertex = (OVertex) Assert.assertNotNull(((IEntityProxy) employeeSaved).__getElement());
		assertFalse(IteratorUtils.toList(vertex.getEdges(ODirection.OUT).iterator()).isEmpty());

		employeeSaved.setCoaches(null);
		employeeRepository.save(employeeSaved);

		final OResultSet resultSetUpdated = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSavedUpdated = getVertices(resultSetUpdated);
		final OVertex vertexOfMainEmployee = getVertexFromUserName(employeeSaved.getFirst(), verticesSavedUpdated).orElseThrow(RuntimeException::new);

		assertTrue(IteratorUtils.toList(vertexOfMainEmployee.getEdges(ODirection.OUT).iterator()).isEmpty());
	}

	/**
	 * Tests update by updating a the property of has-a relationship link.
	 */
	@Test
	public void testUpdate3() {
		final Employee coach1 = new Employee("user_11", "admin_11", "user_11@deloitte.com");
		final Employee mainEmployee = new Employee("user_10", "admin_10", "user_10@deloitte.com");
		final HasReportsTo reports = new HasReportsTo(mainEmployee, coach1);
		mainEmployee.setReportsTo(reports);
		final Employee employeeSaved = Assert.assertNotNull(employeeRepository.save(mainEmployee));
		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved = getVertices(resultSet);
		final String ridOfMainEmployee = getVertexFromUserName(employeeSaved.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();
		final String ridOfCoach1 = getVertexFromUserName(coach1.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();
		assertTrue(employeeRepository.existsById(ridOfMainEmployee));
		assertTrue(employeeRepository.existsById(ridOfCoach1));
		final Employee reportsTo = Assert.assertNotNull(employeeSaved.getReportsTo()).getOut();
		Assert.assertNotNull(reportsTo).setFirst("user_13");
		employeeRepository.save(employeeSaved);
		final OResultSet resultSetUpdated = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSavedUpdated = getVertices(resultSetUpdated);
		assertTrue(getVertexFromUserName(Assert.assertNotNull(reportsTo).getFirst(), verticesSavedUpdated).isPresent());
		assertFalse(getVertexFromUserName("user_11", verticesSavedUpdated).isPresent());
	}

	/**
	 * Tests update by updating the link item relationship of entity.
	 */
	@Test
	public void testUpdate4() {
		final Employee coach1 = new Employee("user_11", "admin_11", "user_11@deloitte.com");
		final Employee coach2 = new Employee("user_13", "admin_13", "user_11@deloitte.com");
		final Employee mainEmployee = new Employee("user_10", "admin_10", "user_10@deloitte.com");
		final HasReportsTo reports = new HasReportsTo(mainEmployee, coach1);
		mainEmployee.setReportsTo(reports);
		final Employee employeeSaved = Assert.assertNotNull(employeeRepository.save(mainEmployee));
		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved = getVertices(resultSet);
		final String ridOfMainEmployee = getVertexFromUserName(employeeSaved.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();
		final String ridOfCoach1 = getVertexFromUserName(coach1.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();
		assertTrue(employeeRepository.existsById(ridOfMainEmployee));
		assertTrue(employeeRepository.existsById(ridOfCoach1));

		final HasReportsTo reports2 = new HasReportsTo(employeeSaved, coach2);
		employeeSaved.setReportsTo(reports2);
		employeeRepository.save(employeeSaved);

		final OResultSet resultSetUpdated = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSavedUpdated = getVertices(resultSetUpdated);
		assertTrue(getVertexFromUserName("user_13", verticesSavedUpdated).isPresent());
		assertTrue(getVertexFromUserName("user_11", verticesSavedUpdated).isPresent());
	}

	/**
	 * Tests update by updating the list items relationship of entity.
	 */
	@Test
	public void testUpdate5() {
		final Employee coach1 = new Employee("user_11", "admin_11", "user_11@deloitte.com");
		final Employee coach2 = new Employee("user_13", "admin_13", "user_11@deloitte.com");
		final Employee coach3 = new Employee("user_14", "admin_14", "user_11@deloitte.com");
		final Employee coach4 = new Employee("user_15", "admin_15", "user_11@deloitte.com");
		final Employee mainEmployee = new Employee("user_10", "admin_10", "user_10@deloitte.com");
		final Employee_coaches coachee1 = new Employee_coaches(coach1, mainEmployee);
		final Employee_coaches coachee2 = new Employee_coaches(coach2, mainEmployee);
		mainEmployee.setCoaches(Arrays.asList(coachee1, coachee2));
		final Employee employeeSaved = Assert.assertNotNull(employeeRepository.save(mainEmployee));
		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved = getVertices(resultSet);
		final String ridOfMainEmployee = getVertexFromUserName(employeeSaved.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();
		final String ridOfCoach1 = getVertexFromUserName(coach1.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();
		final String ridOfCoach2 = getVertexFromUserName(coach2.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();

		assertTrue(employeeRepository.existsById(ridOfMainEmployee));
		assertTrue(employeeRepository.existsById(ridOfCoach1));
		assertTrue(employeeRepository.existsById(ridOfCoach2));

		final Employee_coaches coachee3 = new Employee_coaches(coach3, employeeSaved);
		final Employee_coaches coachee4 = new Employee_coaches(coach4, employeeSaved);
		employeeSaved.setCoaches(Arrays.asList(coachee3, coachee4));
		employeeRepository.save(employeeSaved);

		final OResultSet resultSetUpdated = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSavedUpdated = getVertices(resultSetUpdated);
		final String ridOfCoach3 = getVertexFromUserName(coach3.getFirst(), verticesSavedUpdated).orElseThrow(RuntimeException::new).getIdentity().toString();
		final String ridOfCoach4 = getVertexFromUserName(coach4.getFirst(), verticesSavedUpdated).orElseThrow(RuntimeException::new).getIdentity().toString();
		assertFalse(employeeRepository.existsById(ridOfCoach1));
		assertFalse(employeeRepository.existsById(ridOfCoach2));
		assertTrue(employeeRepository.existsById(ridOfCoach3));
		assertTrue(employeeRepository.existsById(ridOfCoach4));
	}

	/**
	 * Test custom relationship annotation on field of entity.
	 */
	@Test
	public void testUpdate6() {
		final Employee coach1 = new Employee("user_11", "admin_11", "user_11@deloitte.com");
		final Employee mainEmployee = new Employee("user_10", "admin_10", "user_10@deloitte.com");
		final HasReportsTo coachee1 = new HasReportsTo(mainEmployee, coach1);
		mainEmployee.setReportsTo(coachee1);
		final Employee employeeSaved = Assert.assertNotNull(employeeRepository.save(mainEmployee));
		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved = getVertices(resultSet);
		final String ridOfMainEmployee = getVertexFromUserName(employeeSaved.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();
		final String ridOfCoach1 = getVertexFromUserName(coach1.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();
		assertTrue(employeeRepository.existsById(ridOfMainEmployee));
		assertTrue(employeeRepository.existsById(ridOfCoach1));
		final Employee reportTo = Assert.assertNotNull(employeeSaved.getReportsTo()).getOut();
		Assert.assertNotNull(reportTo).setFirst("user_13");
		employeeRepository.save(employeeSaved);
		final Iterable<OEdge> edges = getVertexFromUserName(employeeSaved.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getEdges(ODirection.OUT);
		for (final OEdge oEdge : edges) {
			assertEquals( "HasReportsTo", oEdge.getSchemaType().orElseThrow(RuntimeException::new).getName());
		}
		final OResultSet resultSetUpdated = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSavedUpdated = getVertices(resultSetUpdated);
		assertTrue(getVertexFromUserName(Assert.assertNotNull(reportTo).getFirst(), verticesSavedUpdated).isPresent());
		assertFalse(getVertexFromUserName("user_11", verticesSavedUpdated).isPresent());
	}
	
	/**
	 * Tests update of a list item which has a 'has-a' relationship with entity.
	 */
	@Test
	public void testUpdate7() {
		final Employee coach1 = new Employee("user_11", "admin_11", "user_11@deloitte.com");
		final Employee mainEmployee = new Employee("user_10", "admin_10", "user_10@deloitte.com");
		final Employee_coaches coachee1 = new Employee_coaches(coach1, mainEmployee);
		mainEmployee.setCoaches(Arrays.asList(coachee1));
		final Employee employeeSaved = Assert.assertNotNull(employeeRepository.save(mainEmployee));
		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved = getVertices(resultSet);
		final String ridOfMainEmployee = getVertexFromUserName(employeeSaved.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();
		final String ridOfCoach1 = getVertexFromUserName(coach1.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();

		assertTrue(employeeRepository.existsById(ridOfMainEmployee));
		assertTrue(employeeRepository.existsById(ridOfCoach1));
		
		Assert.assertNotNull(Assert.assertNotNull(employeeSaved.getCoaches()).get(0).getIn()).setFirst("UpdatedCoach");

		employeeRepository.save(employeeSaved);

		final OResultSet resultSetUpdated = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSavedUpdated = getVertices(resultSetUpdated);
		final String ridOfUpdatedCoach = getVertexFromUserName("UpdatedCoach", verticesSavedUpdated).orElseThrow(RuntimeException::new).getIdentity().toString();
		assertTrue(employeeRepository.existsById(ridOfUpdatedCoach));
	}
	
	/**
	 * Test that save is successful after a previous find has thrown NoRecordFoundException.
	 */
	@Test
	public void testSaveAfterFind() {
		employeeRepositoryTestService.saveAfterFindTransaction();
		/* employee should exist now */
		assertNotNull(employeeRepository.findByFirst("user_save_after_find"));
	}
	
	/**
	 * 
	 * Test to ensure no stale connections are left when accessing list elements of EntityProxy out side transaction. Assertions not needed.
	 *
	 * @throws InterruptedException thrown from concurrent implementation
	 */
	@Test
	public void testUpdateCollectionConcurrency() throws InterruptedException {
		final Employee coach1 = new Employee("user_11", "admin_11", "user_11@deloitte.com");
		final Employee mainEmployee = new Employee("user_10", "admin_10", "user_10@deloitte.com");
		final Employee_coaches coachee1 = new Employee_coaches(coach1, mainEmployee);
		mainEmployee.setCoaches(Arrays.asList(coachee1));
		final Employee[] employees = new Employee[1];
		employees[0] = Assert.assertNotNull(employeeRepository.save(mainEmployee));
		final RuntimeException[] thrownExceptions = new RuntimeException[1];
		/* exhausting all the available connections */
		for(int i=0; i < maxPoolSize; i++) {
			final Thread thread = new Thread(() ->  {
				try {
					Assert.assertNotNull(Assert.assertNotNull(employees[0].getCoaches()).get(0));
				} catch (final Exception e) {
					thrownExceptions[0] = (RuntimeException) e;
				}
			});
			thread.start();
			thread.join();
			if(thrownExceptions[0] != null) {
				throw thrownExceptions[0];
			}
			employees[0] = Assert.assertNotNull(employeeRepository.save(employees[0]));
		}
		
	}
	
	/**
	 * 
	 * Test to ensure no stale connections are left when accessing lazy properties of EntityProxy out side transaction. Assertions not needed.
	 *
	 * @throws InterruptedException thrown from concurrent implementation
	 */
	@Test
	public void testUpdateEdgeCollectionConcurrencyWithIn() throws InterruptedException {
		final Employee coach1 = new Employee("user_11", "admin_11", "user_11@deloitte.com");
		final Employee mainEmployee = new Employee("user_10", "admin_10", "user_10@deloitte.com");
		final Employee_coaches coachee1 = new Employee_coaches(coach1, mainEmployee);
		mainEmployee.setCoaches(Arrays.asList(coachee1));
		final Employee[] employees = new Employee[1];
		employees[0] = Assert.assertNotNull(employeeRepository.save(mainEmployee));
		final RuntimeException[] thrownExceptions = new RuntimeException[1];
		/* exhausting all the available connections */
		for(int i=0; i < maxPoolSize; i++) {
			final Thread thread = new Thread(() ->  {
				try {
					Assert.assertNotNull(Assert.assertNotNull(employees[0].getCoaches()).get(0).getIn());
				} catch (final Exception e) {
					thrownExceptions[0] = (RuntimeException) e;
				}
			});
			thread.start();
			thread.join();
			if(thrownExceptions[0] != null) {
				throw thrownExceptions[0];
			}
			employees[0] = Assert.assertNotNull(employeeRepository.save(employees[0]));
		}
		
	}
	
	

}