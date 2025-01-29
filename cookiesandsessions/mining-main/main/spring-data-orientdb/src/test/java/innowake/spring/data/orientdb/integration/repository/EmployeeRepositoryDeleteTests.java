package innowake.spring.data.orientdb.integration.repository;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import com.orientechnologies.orient.core.id.ORID;
import com.orientechnologies.orient.core.record.OVertex;
import com.orientechnologies.orient.core.sql.executor.OResultSet;

import innowake.lib.core.lang.Assert;
import innowake.spring.data.orientdb.integration.repository.domain.Employee;
import innowake.spring.data.orientdb.integration.repository.domain.Employee_coaches;

/**
 * Test cases for all the delete operations on {@link Employee} entity.
 */
public class EmployeeRepositoryDeleteTests extends AbstractEmployeeRepositoryIntegrationTests {
	
	
	private final Employee coach1 = new Employee("user_11", "admin_11", "user_11@deloitte.com");
	private final Employee coach2 = new Employee("user_12", "admin_12", "user_12@deloitte.com");
	private final Employee coach3 = new Employee("user_13", "admin_13", "user_13@deloitte.com");
	private final Employee mainEmployee = new Employee("user_10", "admin_10", "user_10@deloitte.com");
	private final Employee managerEmployee = new Employee("user_14", "admin_14", "user_14@deloitte.com");

	/**
	 * Tests delete by id CRUD operation.
	 */
	@Test 
	public void testDeleteEntityByRID() {
		final Employee_coaches coaches1 = new Employee_coaches(coach1, mainEmployee);
		final Employee_coaches coaches2 = new Employee_coaches(coach2, mainEmployee);
		final Employee_coaches coaches3 = new Employee_coaches(coach3, mainEmployee);
		mainEmployee.setCoaches(Arrays.asList(coaches1, coaches2, coaches3));

		final Employee_coaches mainCoachee = new Employee_coaches(mainEmployee, managerEmployee);
		managerEmployee.setCoaches(Arrays.asList(mainCoachee));

		final Employee savedManager = employeeRepository.save(managerEmployee);
		assertNotNull(savedManager);

		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved = getVertices(resultSet);

		final String ridOfMainEmployee = getVertexFromUserName(mainEmployee.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();

		final String ridOfCoach1 = getVertexFromUserName(coach1.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();
		final String ridOfCoach2 = getVertexFromUserName(coach2.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();
		final String ridOfCoach3 = getVertexFromUserName(coach3.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();

		final String ridOfManagerEmployee = getVertexFromUserName(managerEmployee.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();

		assertTrue(employeeRepository.existsById(ridOfMainEmployee));
		assertTrue(employeeRepository.existsById(ridOfCoach1));
		assertTrue(employeeRepository.existsById(ridOfCoach2));
		assertTrue(employeeRepository.existsById(ridOfCoach3));
		assertTrue(employeeRepository.existsById(ridOfManagerEmployee));

		employeeRepository.deleteById(ridOfMainEmployee);

		assertFalse(employeeRepository.existsById(ridOfMainEmployee));
		assertFalse(employeeRepository.existsById(ridOfCoach1));
		assertFalse(employeeRepository.existsById(ridOfCoach2));
		assertFalse(employeeRepository.existsById(ridOfCoach3));
		assertTrue(employeeRepository.existsById(ridOfManagerEmployee));
	}

	/**
	 * Tests simple delete CRUD operation.
	 */
	@Test 
	public void testDeletingEntityWithSavedInstance() {
		final Employee_coaches coaches1 = new Employee_coaches(coach1, mainEmployee);
		final Employee_coaches coaches2 = new Employee_coaches(coach2, mainEmployee);
		final Employee_coaches coaches3 = new Employee_coaches(coach3, mainEmployee);
		mainEmployee.setCoaches(Arrays.asList(coaches1, coaches2, coaches3));

		final Employee_coaches mainCoachee = new Employee_coaches(mainEmployee, managerEmployee);
		managerEmployee.setCoaches(Arrays.asList(mainCoachee));

		final Employee savedManager = employeeRepository.save(managerEmployee);
		assertNotNull(savedManager);

		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved = getVertices(resultSet);
		final Employee mainEmployeeSaved = Assert.assertNotNull(savedManager.getCoaches()).get(0).getIn();
		final String ridOfMainEmployee = getVertexFromUserName(assertNotNull(mainEmployeeSaved).getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();

		final String ridOfCoach1 = getVertexFromUserName(coach1.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();
		final String ridOfCoach2 = getVertexFromUserName(coach2.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();
		final String ridOfCoach3 = getVertexFromUserName(coach3.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();

		final String ridOfManagerEmployee = getVertexFromUserName(managerEmployee.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();

		assertTrue(employeeRepository.existsById(ridOfMainEmployee));
		assertTrue(employeeRepository.existsById(ridOfCoach1));
		assertTrue(employeeRepository.existsById(ridOfCoach2));
		assertTrue(employeeRepository.existsById(ridOfCoach3));
		assertTrue(employeeRepository.existsById(ridOfManagerEmployee));

		employeeRepository.delete(assertNotNull(mainEmployeeSaved));

		assertFalse(employeeRepository.existsById(ridOfMainEmployee));
		assertFalse(employeeRepository.existsById(ridOfCoach1));
		assertFalse(employeeRepository.existsById(ridOfCoach2));
		assertFalse(employeeRepository.existsById(ridOfCoach3));
		assertTrue(employeeRepository.existsById(ridOfManagerEmployee));
	}
	
	/**
	 * Tests negative scenario for delete Operation.
	 */
	@Test 
	public void testDeleteEntityNegativeScenario() {
		employeeRepository.save(mainEmployee);
		final Exception runtimeException = assertThrows(IllegalArgumentException.class, () -> employeeRepository.delete(mainEmployee));
		assertEquals("Entity should be an instance of IEntityProxy", runtimeException.getMessage());
	}

	/**
	 * Tests delete all CRUD operation with list of entity parameter.
	 */
	@Test 
	public void testDeleteAll() {
		final Employee_coaches coaches1 = new Employee_coaches(coach1, mainEmployee);
		final Employee_coaches coaches2 = new Employee_coaches(coach2, mainEmployee);
		final Employee_coaches coaches3 = new Employee_coaches(coach3, mainEmployee);
		mainEmployee.setCoaches(Arrays.asList(coaches1, coaches2, coaches3));

		final Employee employeeSaved = Assert.assertNotNull(employeeRepository.save(mainEmployee));

		final Employee managerEmployeeSaved = Assert.assertNotNull(employeeRepository.save(managerEmployee));

		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved = getVertices(resultSet);

		final String ridOfMainEmployee = getVertexFromUserName(mainEmployee.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();

		final String ridOfCoach1 = getVertexFromUserName(coach1.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();
		final String ridOfCoach2 = getVertexFromUserName(coach2.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();
		final String ridOfCoach3 = getVertexFromUserName(coach3.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();

		final String ridOfManagerEmployee = getVertexFromUserName(managerEmployee.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();

		assertTrue(employeeRepository.existsById(ridOfMainEmployee));
		assertTrue(employeeRepository.existsById(ridOfCoach1));
		assertTrue(employeeRepository.existsById(ridOfCoach2));
		assertTrue(employeeRepository.existsById(ridOfCoach3));
		assertTrue(employeeRepository.existsById(ridOfManagerEmployee));

		final List<Employee> employeesToBeDeleted = Arrays.asList(employeeSaved, managerEmployeeSaved);

		employeeRepository.deleteAll(employeesToBeDeleted);

		assertFalse(employeeRepository.existsById(ridOfMainEmployee));
		assertFalse(employeeRepository.existsById(ridOfCoach1));
		assertFalse(employeeRepository.existsById(ridOfCoach2));
		assertFalse(employeeRepository.existsById(ridOfCoach3));
		assertFalse(employeeRepository.existsById(ridOfManagerEmployee));
	}

	/**
	 * Tests delete all CRUD operation with list of entity parameter.
	 */
	@Test 
	public void testDeleteAllUnparameterized() {
		final Employee_coaches coaches1 = new Employee_coaches(coach1, mainEmployee);
		final Employee_coaches coaches2 = new Employee_coaches(coach2, mainEmployee);
		final Employee_coaches coaches3 = new Employee_coaches(coach3, mainEmployee);
		mainEmployee.setCoaches(Arrays.asList(coaches1, coaches2, coaches3));

		final Employee employeeSaved = Assert.assertNotNull(employeeRepository.save(mainEmployee));


		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved = getVertices(resultSet);
		final String ridOfMainEmployee = getVertexFromUserName(employeeSaved.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();
		final String ridOfCoach1 = getVertexFromUserName(coach1.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();
		final String ridOfCoach2 = getVertexFromUserName(coach2.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();
		final String ridOfCoach3 = getVertexFromUserName(coach3.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity().toString();


		assertTrue(employeeRepository.existsById(ridOfMainEmployee));
		assertTrue(employeeRepository.existsById(ridOfCoach1));
		assertTrue(employeeRepository.existsById(ridOfCoach2));
		assertTrue(employeeRepository.existsById(ridOfCoach3));

		employeeRepository.deleteAll();

		assertFalse(employeeRepository.existsById(ridOfMainEmployee));
		assertFalse(employeeRepository.existsById(ridOfCoach1));
		assertFalse(employeeRepository.existsById(ridOfCoach2));
		assertFalse(employeeRepository.existsById(ridOfCoach3));
	}

	/**
	 * Tests exists by id CRUD operation.
	 */
	@Test 
	public void testExistsByIdMethod() {
		final Employee employee1 = new Employee("user_11", "admin_11", "user_11@deloitte.com");
		final Employee employeeSaved = employeeRepository.save(employee1);
		final OResultSet resultSet = queryWithEntityName(ENTITY_NAME);
		final List<OVertex> verticesSaved = getVertices(resultSet);
		final ORID rid = getVertexFromUserName(employeeSaved.getFirst(), verticesSaved).orElseThrow(RuntimeException::new).getIdentity();
		assertTrue(employeeRepository.existsById(rid));
	}

}
