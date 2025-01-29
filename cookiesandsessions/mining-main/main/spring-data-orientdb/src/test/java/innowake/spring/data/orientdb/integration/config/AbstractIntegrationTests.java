/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.integration.config;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.runner.RunWith;
import org.mockito.internal.matchers.apachecommons.ReflectionEquals;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.orientechnologies.orient.core.db.ODatabaseSession;
import com.orientechnologies.orient.core.record.ODirection;
import com.orientechnologies.orient.core.record.OEdge;
import com.orientechnologies.orient.core.record.OVertex;
import com.orientechnologies.orient.core.sql.executor.OResultSet;

import innowake.spring.data.orientdb.commons.core.SessionManager;
import innowake.spring.data.orientdb.repository.OrientRepository;

/**
 *Base class for tests for {@link OrientRepository}
 */
@RunWith(SpringJUnit4ClassRunner.class)
@SpringBootTest(classes = {
		OrientDbTestConfiguration.class
})
@DirtiesContext
public abstract class AbstractIntegrationTests {

	/**
	 * Instance of orientdb {@link SessionManager}
	 */
	@Autowired
	protected SessionManager sessionManager;

	/**
	 * 
	 * Iterates through each object and compare it's field's values.
	 *
	 * @param collection1 data fetched from the database
	 * @param collection2 data saved to the database
	 * @param ignoredFields fields that needs to be ignored for comparison
	 */
	protected void assertCollectionObjects(final Collection<?> collection1, final Collection<?> collection2, final String... ignoredFields) {
		assertEquals(collection1.size(), (collection2.size()));
		final Iterator<?> iterator1 = collection1.iterator();
		final Iterator<?> iterator2 = collection2.iterator();
		while (iterator1.hasNext()) {
			assertTrue(new ReflectionEquals(iterator1.next(), ignoredFields).matches(iterator2.next()));
		}
	}

	/**
	 * Clears the data for given entity classes in database.
	 *
	 * @param entities entity classes mapped in db
	 */
	protected void clearData(final String... entities) {
		for (final String entity : entities) {
			getSession().execute("sql", "DELETE VERTEX " + entity);
		}
	}
	
	/**
	 * Clears the data for given document entity classes in database.
	 *
	 * @param entities document entity classes mapped in db
	 */
	protected void clearNonVertexData(final String... entities) {
		for (final String entity : entities) {
			getSession().execute("sql", "DELETE FROM " + entity);
		}
	}

	/**
	 * Executes a select query on the given entity class.
	 *
	 * @param entityName entity class mapped in db
	 * @return records saved for the given entity
	 */
	protected OResultSet queryWithEntityName(final String entityName) {
		return getSession().query("select * from " + entityName);
	}

	/**
	 * Returns all the vertices present in a given result set.
	 * 
	 * @param resultSet records retrieved from the database
	 * @return all the vertices
	 */
	protected List<OVertex> getVertices(final OResultSet resultSet) {
		return resultSet.vertexStream().collect(Collectors.toList());
	}

	/**
	 * Returns all the edges present in a given result set.
	 * 
	 * @param vertices all the vertices present in a result set
	 * @return all the edges
	 */
	protected List<OEdge> getAllEdges(final List<OVertex> vertices) {
		final List<OEdge> edgesToCollect = new ArrayList<>();
		for (final OVertex vertex : vertices) {
			final Iterator<OEdge> edges = vertex.getEdges(ODirection.OUT).iterator();
			while (edges.hasNext()) {
				edgesToCollect.add(edges.next());
			}
		}
		return edgesToCollect;
	}

	/**
	 * Returns all the edges for a give vertex class
	 *
	 * @param vertex a vertex class mapped in db
	 * @return edges for a given vertex
	 */
	protected List<OEdge> getEdgeFromVertex(final OVertex vertex) {
		final List<OEdge> edgesToCollect = new ArrayList<>();
		final Iterator<OEdge> edges = vertex.getEdges(ODirection.OUT).iterator();
		while (edges.hasNext()) {
			edgesToCollect.add(edges.next());
		}
		return edgesToCollect;
	}

	private ODatabaseSession getSession() {
		return sessionManager.getThreadDatabase();
	}

}
