package innowake.spring.data.orientdb.integration.repository;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.junit.Test;
import org.mockito.internal.matchers.apachecommons.ReflectionEquals;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.orientechnologies.orient.core.id.ORID;
import com.orientechnologies.orient.core.record.OVertex;
import com.orientechnologies.orient.core.sql.executor.OResultSet;

import innowake.lib.core.lang.Assert;
import innowake.spring.data.orientdb.api.query.clause.OrientClause;
import innowake.spring.data.orientdb.api.query.clause.OrientOperator;
import innowake.spring.data.orientdb.commons.exception.NoRecordFoundException;
import innowake.spring.data.orientdb.integration.repository.domain.Employee;
import innowake.spring.data.orientdb.integration.repository.domain.Employee.Access;
import innowake.spring.data.orientdb.integration.repository.domain.Employee.Designation;
import innowake.spring.data.orientdb.integration.repository.domain.Employee_coaches;
import innowake.spring.data.orientdb.integration.repository.domain.Employee_projectStatus;
import innowake.spring.data.orientdb.integration.repository.domain.Employee_projects;
import innowake.spring.data.orientdb.integration.repository.domain.Project;
import innowake.spring.data.orientdb.repository.EmployeeRepository;

/**
 * Test cases for all the find methods on {@link Employee} entity.
 */
public class EmployeeRepositoryFindTests extends AbstractEmployeeRepositoryIntegrationTests {

	/**
	 * Test {@link EmployeeRepository#findById(String)} when the id is empty
	 */
	@Test(expected = IllegalArgumentException.class)
	public void findByIdWhenIdIsEmpty() {
		employeeRepository.findById("");
	}

	/**
	 * Test {@link EmployeeRepository#findById(String)} when the id is not valid
	 */
	@Test(expected = NoRecordFoundException.class)
	public void findByIdWhenSchemaIsNotFoundForTheGivenEntity() {
		employeeRepository.findById("#14:17");
	}

	/**
	 * Test {@link EmployeeRepository#findById(String)} with primitive data fields
	 */
	@Test
	public void findEmployeeByRidWithPrimitiveFields() {
		final Employee employee = new Employee("Alice", "John", "alice_john@deloitte.com");
		final Employee employeeSaved = Assert.assertNotNull(employeeRepository.save(employee));

		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved = getVertices(resultSet);
		final String employeeId = getVertexFromUserName(employeeSaved.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();

		final Employee employeeAlice = employeeRepository.findById(employeeId).orElse(null);
		assertNotNull(employeeAlice);
		assertTrue(new ReflectionEquals(employee, "rid", "id").matches(employeeAlice));
	}

	/**
	 * Test {@link EmployeeRepository#findById(String)} with list data fields
	 */
	@Test
	public void findEmployeeByRidWithPrimitiveListFields() {
		final String[] skills = { "java", "angular"};
		final Employee employee = new Employee("Alice", "John", "alice_john@deloitte.com");
		employee.setEmpSkills(Arrays.asList(skills));
		final Employee savedemployee = Assert.assertNotNull(employeeRepository.save(employee));

		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved = getVertices(resultSet);
		final String employeeId = getVertexFromUserName(savedemployee.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();

		final Employee employeeAlice = Assert.assertNotNull(employeeRepository.findById(employeeId).orElse(null));
		assertTrue(new ReflectionEquals(employee, "rid", "skills", "id").matches(employeeAlice));
		assertTrue(Assert.assertNotNull(employeeAlice.getEmpSkills()).containsAll(employee.getEmpSkills()));
	}

	/**
	 * Test {@link EmployeeRepository#findById(String)} with map data fields
	 */
	@Test
	public void findEmployeeByRidWithPrimitiveMapFields() {
		final Map<String, String> trainings =  new HashMap<>();
		trainings.put("testing", "enrolled");
		trainings.put("bootstrap", "completed");
		final Employee employee = new Employee("Alice", "John", "alice_john@deloitte.com");
		employee.setTrainingStatus(trainings);
		final Employee savedemployee = Assert.assertNotNull(employeeRepository.save(employee));

		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved = getVertices(resultSet);
		final String employeeId = getVertexFromUserName(savedemployee.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();

		final Employee employeeAlice = Assert.assertNotNull(employeeRepository.findById(employeeId).orElse(null));
		assertTrue(new ReflectionEquals(employee, "rid", "trainings", "id").matches(employeeAlice));
		assertTrue(Assert.assertNotNull(employee.getTrainingStatus()).equals(employeeAlice.getTrainingStatus()));
	}

	/**
	 * Test {@link EmployeeRepository#findById(String)} with "has-a" relation data field
	 */
	@Test
	public void findEmployeeByRidWithRelationshipFields() {
		final Project[] projects = {new Project("project_1"), new Project("project_2")};
		final Employee manager = new Employee("Bob", "Marley", "bob_marley@deloitte.com", Arrays.asList(projects));
		final Employee employee = new Employee("Alice", "John", "alice_john@deloitte.com", manager);
		final Employee savedemployee = Assert.assertNotNull(employeeRepository.save(employee));

		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved = getVertices(resultSet);
		final String employeeId = getVertexFromUserName(savedemployee.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();

		final Employee employeeAlice = Assert.assertNotNull(employeeRepository.findById(employeeId).orElse(null));
		assertTrue(new ReflectionEquals(employee, "rid", "reportsTo", "id").matches(employeeAlice));
		assertTrue(new ReflectionEquals(employee.getReportsTo(), "rid", "out", "in").matches(employeeAlice.getReportsTo()));
		final Employee reportsTo = assertNotNull(employeeAlice.getReportsTo()).getIn();
		final List<Project> projectList = assertNotNull(manager.getProjects()).stream().map(employeeProjects -> employeeProjects.getIn()).collect(Collectors.toList());
		final List<Project> fetchedProjectList = assertNotNull(assertNotNull(reportsTo).getProjects()).stream().map(employeeProjects -> employeeProjects.getIn()).collect(Collectors.toList());
		assertCollectionObjects(assertNotNull(projectList), assertNotNull(fetchedProjectList), "rid", "id");
	}

	/**
	 * Test {@link EmployeeRepository#findById(String)} with "has-a" list data field
	 */
	@Test
	public void findEmployeeByRidWithRelationshipFieldsAsList() {
		final Project[] projects = {new Project("project_1"), new Project("project_2")};
		final Employee manager = new Employee("Bob", "Marley", "bob_marley@deloitte.com", Arrays.asList(projects));
		final Employee alice = new Employee("Alice", "John", "alice_john@deloitte.com", manager);
		final Employee harry = new Employee("Harry", "Potter", "harry_potter@deloitte.com", manager);
		final Employee_coaches coachee1 = new Employee_coaches(alice, manager);
		final Employee_coaches coachee2 = new Employee_coaches(harry, manager);
		final List<Employee_coaches> employees = Arrays.asList(coachee1, coachee2);
		manager.setCoaches(employees);
		final Employee savedManager = Assert.assertNotNull(employeeRepository.save(manager));

		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved = getVertices(resultSet);
		final String managerId = getVertexFromUserName(savedManager.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();
		final Employee employeeManager = Assert.assertNotNull(employeeRepository.findById(managerId).orElse(null));
		assertTrue(new ReflectionEquals(manager, "rid", "id", "coaches", "projects", "reportsTo").matches(employeeManager));
	}

	/**
	 * Test {@link EmployeeRepository#findById(String)} with enum data field
	 */
	@Test
	public void findEmployeeByRidWithRelationshipFieldsAsEnum() {
		final Employee manager = new Employee("Bob", "Marley", "bob_marley@deloitte.com");
		manager.setEmpDesignation(Designation.MANAGER);
		final Employee savedManager = Assert.assertNotNull(employeeRepository.save(manager));

		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved = getVertices(resultSet);
		final String managerId = getVertexFromUserName(savedManager.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();

		final Employee employeeManager = employeeRepository.findById(managerId).orElse(null);
		assertNotNull(employeeManager);
		assertTrue(new ReflectionEquals(manager, "rid", "id").matches(employeeManager));
	}

	/**
	 * Test {@link EmployeeRepository#findById(String)} with enum list data field
	 */
	@Test
	public void findEmployeeByRidWithRelationshipFieldsAsEnumCollection() {
		final Employee manager = new Employee("Bob", "Marley", "bob_marley@deloitte.com");
		final Access[] access = {Access.ADMIN, Access.DEVELOPER, Access.USER};
		manager.setEmpAccess(Arrays.asList(access));
		final Employee savedManager = Assert.assertNotNull(employeeRepository.save(manager));

		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved = getVertices(resultSet);
		final ORID managerId = getVertexFromUserName(savedManager.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity();

		final Employee employeeManager = employeeRepository.findById(managerId).orElse(null);
		assertNotNull(employeeManager);
		assertTrue(new ReflectionEquals(manager, "rid", "id").matches(employeeManager));
	}



	/**
	 * Test {@link EmployeeRepository#findAllById(Iterable)} to find list of records by their id's.
	 */
	@Test
	public void findAllEmployeesByIds() {
		final Employee employee1 = new Employee("user_4", "admin_4", "user_4@deloitte.com");
		final Employee employee2 = new Employee("user_5", "admin_5", "user_5@deloitte.com");
		final Employee employee3 = new Employee("user_6", "admin_6", "user_6@deloitte.com");
		final List<Employee> employees = Arrays.asList(employee1, employee2, employee3);
		final Iterable<Employee> savedEmployees = Assert.assertNotNull(employeeRepository.saveAll(employees));
		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved = getVertices(resultSet);
		final List<String> ids = new ArrayList<>();
		for(final Employee employee: savedEmployees) {
			getVertexFromUserName(employee.getFirst(), verticesSaved).ifPresent(data -> ids.add(data.getIdentity().toString()));
		}

		final List<Employee> fetchedEmployees = (List<Employee>) employeeRepository.findAllById(ids);
		assertCollectionObjects(employees, fetchedEmployees, "rid", "id");
	}

	/**
	 * Test {@link EmployeeRepository#findAll()} to find all employee records.
	 */
	@Test
	public void findAllEmployees() {
		final Employee employee1 = new Employee("user_4", "admin_4", "user_4@deloitte.com");
		final Employee employee2 = new Employee("user_5", "admin_5", "user_5@deloitte.com");
		final Employee employee3 = new Employee("user_6", "admin_6", "user_6@deloitte.com");
		final TreeSet<Employee> employees = new TreeSet<>(Arrays.asList(employee1 , employee2, employee3));
		assertNotNull(employeeRepository.saveAll(employees));
		final List<Employee> fetchedEmployees = (List<Employee>) employeeRepository.findAll();
		assertCollectionObjects(employees, new TreeSet<>(fetchedEmployees), "rid", "id");
	}

	/**
	 * Test {@link EmployeeRepository#findAll()} to find all employee record with {@link List} data type.
	 */
	@Test
	public void findAllEmployeesWithListLazyLoading() {
		final Employee employee = new Employee("user_4", "admin_4", "user_4@deloitte.com");
		final Employee_projects project1 = new Employee_projects(employee, new Project("Mining"));
		final Employee_projects project2 = new Employee_projects(employee, new Project("Discovery"));
		employee.setProjects(Arrays.asList(project1, project2));
		assertNotNull(employeeRepository.save(employee));
		final List<Employee> fetchedEmployee = (List<Employee>) employeeRepository.findAll();
		assertTrue(new ReflectionEquals(employee, "rid", "projects", "id").matches(fetchedEmployee.get(0)));
		ArrayList<Employee_projects> fetchedProjects = (ArrayList<Employee_projects>) Assert.assertNotNull(fetchedEmployee.get(0).getProjects());
		@SuppressWarnings("unchecked")
		ArrayList<Employee_projects> clonedProject = (ArrayList<Employee_projects>) fetchedProjects.clone();
		assertEquals(clonedProject.size(), fetchedProjects.size());
		assertTrue(clonedProject.containsAll(fetchedProjects));
	}

	/**
	 * Test {@link EmployeeRepository#findAll()} to find all employee record with lazily loaded map.
	 */
	@Test
	public void findAllEmployeesWithMapLazyLoading() {
		final Employee employee1 = new Employee("user_4", "admin_4", "user_4@deloitte.com");
		final Project subProject1 = new Project("Cobol Parser");
		final Project subProject2 = new Project("Mining");
		final Map<String, Employee_projectStatus> projectStatus = new HashMap<>();
		final Employee_projectStatus employee_projects = new Employee_projectStatus(employee1, subProject1);
		final Employee_projectStatus employee_project2 = new Employee_projectStatus(employee1, subProject2);
		projectStatus.put("Completed", employee_projects);
		projectStatus.put("Started", employee_project2);
		employee1.setProjectStatus(projectStatus);
		assertNotNull(employeeRepository.save(employee1));
		final List<Employee> fetchedEmployees = (List<Employee>) employeeRepository.findAll();
		final HashMap<String, Employee_projectStatus> fetchedProjectStatus = (HashMap<String, Employee_projectStatus>) Assert.assertNotNull(fetchedEmployees.get(0).getProjectStatus());
		@SuppressWarnings("unchecked")
		final HashMap<String, Employee_projectStatus> clonedData = (HashMap<String, Employee_projectStatus>) fetchedProjectStatus.clone();
		assertTrue(new ReflectionEquals(fetchedProjectStatus.get("Completed")).matches(clonedData.get("Completed")));
	}
	
	/**
	 * Test {@link EmployeeRepository#findAll()} to find all employee record with lazily loaded map.
	 */
	@Test
	public void findAllEmployeesWithDirectMapLazyLoading() {
		final Employee employee1 = new Employee("user_4", "admin_4", "user_4@deloitte.com");
		final Project subProject1 = new Project("Cobol Parser");
		final Project subProject2 = new Project("Mining");
		final Map<String, Project> projectStatus = new HashMap<>();
		projectStatus.put("Completed", subProject1);
		projectStatus.put("Started", subProject2);
		employee1.setProjectsMap(projectStatus);
		assertNotNull(employeeRepository.save(employee1));
		final List<Employee> fetchedEmployees = (List<Employee>) employeeRepository.findAll();
		final HashMap<String, Project> fetchedProjectStatus = (HashMap<String, Project>) Assert.assertNotNull(fetchedEmployees.get(0).getProjectsMap());
		assertEquals("Cobol Parser", assertNotNull(fetchedProjectStatus.get("Completed")).getName());
	}
	
	/**
	 * Filter test for enum values.
	 */
	@Test
	public void findEmployeeUsingFilter() {
		final Employee employee1 = new Employee("user_4", "admin_4", "user_4@deloitte.com");
		final Employee employee2 = new Employee("user_5", "admin_5", "user_5@deloitte.com");
		final Employee employee3 = new Employee("user_6", "admin_6", "user_6@deloitte.com");
		final Employee employee4 = new Employee("user_7", "admin_7", "user_7@deloitte.com");
		employee1.setEmpDesignation(Designation.MANAGER);
		employee2.setEmpDesignation(Designation.CONSULTANT);
		employee3.setEmpDesignation(Designation.CONSULTANT);
		employee4.setEmpDesignation(Designation.CONSULTANT);
		
		final TreeSet<Employee> employees = new TreeSet<>(Arrays.asList(employee1 , employee2, employee3));
		assertNotNull(employeeRepository.saveAll(employees));
		
		/* Clause for enum */
		final OrientClause clause = OrientClause.clause(Employee.class, "designation.name", OrientOperator.EQ, Designation.CONSULTANT.toString());
		final PageRequest oneElementPageRequest = PageRequest.of(0, 2);
		final Page<Employee> page = employeeRepository.findAll(clause, oneElementPageRequest);
		assertEquals(Integer.valueOf(2), Integer.valueOf(page.getContent().size()));
	}
	
	/**
	 * Filter test without pagination.
	 */
	@Test
	public void findEmployeeUsingFilterUnPaged() {
		final Employee employee1 = new Employee("user_4", "admin_4", "user_4@deloitte.com");
		final Employee employee2 = new Employee("user_5", "admin_5", "user_5@deloitte.com");
		final Employee employee3 = new Employee("user_6", "admin_6", "user_6@deloitte.com");
		final Employee employee4 = new Employee("user_7", "admin_7", "user_7@deloitte.com");
		employee1.setEmpDesignation(Designation.MANAGER);
		employee2.setEmpDesignation(Designation.CONSULTANT);
		employee3.setEmpDesignation(Designation.CONSULTANT);
		employee4.setEmpDesignation(Designation.CONSULTANT);
		
		final TreeSet<Employee> employees = new TreeSet<>(Arrays.asList(employee1 , employee2, employee3, employee4));
		assertNotNull(employeeRepository.saveAll(employees));
		
		/* Clause for enum */
		final OrientClause clause = OrientClause.clause(Employee.class, "designation.name", OrientOperator.LIKE, Designation.CONSULTANT.toString());
		final Page<Employee> page = employeeRepository.findAll(clause, Pageable.unpaged());
		assertEquals(Integer.valueOf(3), Integer.valueOf(page.getContent().size()));
	}
	

}
