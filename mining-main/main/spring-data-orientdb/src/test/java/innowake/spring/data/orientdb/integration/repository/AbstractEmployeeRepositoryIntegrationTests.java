package innowake.spring.data.orientdb.integration.repository;

import static org.junit.Assert.assertNotNull;

import java.util.List;
import java.util.Optional;

import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import com.orientechnologies.orient.core.record.OVertex;

import innowake.lib.core.api.lang.Nullable;
import innowake.spring.data.orientdb.integration.config.AbstractIntegrationTests;
import innowake.spring.data.orientdb.repository.EmployeeRepository;

/**
 * Abstract class for initializations required for all basic CRUD operations using Employee as an entity.
 */
public abstract class AbstractEmployeeRepositoryIntegrationTests extends AbstractIntegrationTests {

	/**
	 * Inject {@link EmployeeRepository} instance.
	 */
	@Autowired
	protected EmployeeRepository employeeRepository;
	/**
	 * Entity class name.
	 */
	protected static final String ENTITY_NAME = "Employee";
	/**
	 * Entity class name.
	 */
	protected static final String ENTITY_NAME_2 = "Project";
	/**
	 * Entity field name.
	 */
	protected static final String PROPERTY_1 = "firstName";

	/**
	 * Clear data in orient db.
	 */
	@Before
	public void clearData() {
		clearData(ENTITY_NAME, ENTITY_NAME_2);
	}

	/**
	 * Tests if {@link EmployeeRepository} instance is created.
	 */
	@Test
	public void repositoryAutowiring() {
		assertNotNull(employeeRepository);
	}

	/**
	 * Returns the record whose property matches the value passed
	 *
	 * @param propertyValue value of the property
	 * 
	 * @param vertices all the vertices found in the result set
	 * @return vertex whose property values matches.
	 */
	protected Optional<OVertex> getVertexFromUserName(@Nullable final String propertyValue, final List<OVertex> vertices) {
		for (final OVertex vertex : vertices) {
			if (vertex.getProperty(PROPERTY_1).equals(propertyValue)) {
				return Optional.of(vertex);
			}
		}
		return Optional.empty();
	}

	/**
	 * Returns the record whose property matches the value passed
	 *
	 * @param propertyValue value of the property
	 * 
	 * @param vertices all the vertices found in the result set
	 * @return vertex whose property values matches.
	 */
	protected Optional<OVertex> getVertexFromProjectName(final String propertyValue, final List<OVertex> vertices) {
		for (final OVertex vertex : vertices) {
			if (vertex.getProperty("name").equals(propertyValue)) {
				return Optional.of(vertex);
			}
		}
		return Optional.empty();
	}

}
