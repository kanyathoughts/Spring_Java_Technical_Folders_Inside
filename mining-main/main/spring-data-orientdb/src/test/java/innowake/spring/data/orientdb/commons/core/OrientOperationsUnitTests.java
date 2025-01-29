/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.commons.core;

import static innowake.lib.core.lang.Assert.assertEqual;
import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.lib.core.lang.Assert.assertNull;
import static innowake.lib.core.lang.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import com.orientechnologies.orient.core.db.ODatabaseDocumentInternal;
import com.orientechnologies.orient.core.metadata.OMetadataInternal;
import com.orientechnologies.orient.core.metadata.schema.OClass;
import com.orientechnologies.orient.core.metadata.sequence.OSequence;
import com.orientechnologies.orient.core.metadata.sequence.OSequenceLibrary;
import com.orientechnologies.orient.core.record.OElement;
import com.orientechnologies.orient.core.record.OVertex;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert.AssertionException;
import innowake.mining.shared.springdata.annotations.Entity;
import innowake.mining.shared.springdata.annotations.Id;
import innowake.mining.shared.springdata.annotations.RId;
import innowake.spring.data.orientdb.commons.exception.AccessorMethodNotFoundException;
import innowake.spring.data.orientdb.commons.exception.MetadataException;
import innowake.spring.data.orientdb.integration.repository.domain.Employee;
import innowake.spring.data.orientdb.ogm.mapping.custom.CustomPropertyService;
import innowake.spring.data.orientdb.ogm.proxy.IEntityProxy;

/**
 * Test cases for {@link OrientOperations}.
 */
@RunWith(MockitoJUnitRunner.class)
public class OrientOperationsUnitTests {
	
	@Mock
	@Nullable
	private SessionManager sessionManager;
	@Mock
	@Nullable
	private ODatabaseDocumentInternal session;
	@Mock
	@Nullable
	private CustomPropertyService customPropertyService;
	
	/**
	 * Initialize session.
	 */
	@Before
	public void init() {
		when(assertNotNull(sessionManager).getThreadDatabase()).thenReturn(session);
	}
	
	/**
	 * Test entity without schema.
	 */
	@Test(expected = MetadataException.class)
	public void testEntityWithoutSchema() {
		final OrientOperations<NewEntity> operations = new SortingAndPaginationImpl<>(assertNotNull(sessionManager), assertNotNull(customPropertyService));
		final NewEntity newEntity = new NewEntity();
		operations.save(newEntity);
	}
	
	/**
	 * Test entity without id field.
	 */
	@Test
	public void testEntityWithoutId() {
		final OrientOperations<NewEntity> operations = new SortingAndPaginationImpl<>(assertNotNull(sessionManager), assertNotNull(customPropertyService));
		final OClass clazz = mock(OClass.class);
		when(assertNotNull(session).getClass("NewEntity")).thenReturn(clazz);
		when(clazz.getName()).thenReturn("NewEntity");
		when(assertNotNull(session).newElement("NewEntity")).thenReturn(mock(OElement.class));
		final NewEntity newEntity = new NewEntity();
		final IEntityProxy proxy = operations.save(newEntity);
		assertTrue(proxy instanceof NewEntity);
	}
	
	/**
	 * Test entity without setter and getter methods.
	 */
	@Test(expected = AccessorMethodNotFoundException.class)
	public void testEntityWithSetterAndGetter() {
		final OrientOperations<User> operations = new SortingAndPaginationImpl<>(assertNotNull(sessionManager), assertNotNull(customPropertyService));
		when(assertNotNull(session).getClass("User")).thenReturn(mock(OClass.class));
		final User user = new User();
		operations.save(user);
	}
	
	/**
	 * Test empty sequence name.
	 */
	@Test(expected = AssertionException.class)
	public void testEmptySequence() {
		final OrientOperations<Employee> operations = new SortingAndPaginationImpl<>(assertNotNull(sessionManager), assertNotNull(customPropertyService));
		operations.getSequenceValue("");
	}
	
	/**
	 * Test invalid sequence name.
	 */
	@Test(expected = MetadataException.class)
	public void testInvalidSequence() {
		final OrientOperations<Employee> operations = new SortingAndPaginationImpl<>(assertNotNull(sessionManager), assertNotNull(customPropertyService));
		final OMetadataInternal metadata = mock(OMetadataInternal.class);
		when(assertNotNull(session).getMetadata()).thenReturn(metadata);
		final OSequenceLibrary library = mock(OSequenceLibrary.class);
		when(metadata.getSequenceLibrary()).thenReturn(library);
		operations.getSequenceValue("New sequence");
	}
	
	/**
	 * Test null with sequence library.
	 */
	@Test(expected = MetadataException.class)
	public void testSequenceLibraryError() {
		final OrientOperations<Employee> operations = new SortingAndPaginationImpl<>(assertNotNull(sessionManager), assertNotNull(customPropertyService));
		final OMetadataInternal metadata = mock(OMetadataInternal.class);
		when(assertNotNull(session).getMetadata()).thenReturn(metadata);
		when(metadata.getSequenceLibrary()).thenReturn(null);
		operations.getSequenceValue("New sequence");
	}
	
	/**
	 * Test valid entity.
	 */
	@Test
	public void testEntity() {
		final OrientOperations<Employee> operations = new SortingAndPaginationImpl<>(assertNotNull(sessionManager), assertNotNull(customPropertyService));
		final OClass schemaClass = mock(OClass.class);
		final OMetadataInternal metadata = mock(OMetadataInternal.class);
		final OSequenceLibrary library = mock(OSequenceLibrary.class);
		final OSequence sequence = mock(OSequence.class);
		final OVertex vertex = mock(OVertex.class);
		when(assertNotNull(session).getMetadata()).thenReturn(metadata);
		when(assertNotNull(session).getClass("Employee")).thenReturn(schemaClass);
		when(Boolean.valueOf(schemaClass.isVertexType())).thenReturn(Boolean.TRUE);
		when(schemaClass.getName()).thenReturn("Employee");
		when(assertNotNull(session).newVertex("Employee")).thenReturn(vertex);
		when(Boolean.valueOf(vertex.isVertex())).thenReturn(Boolean.TRUE);
		when(metadata.getSequenceLibrary()).thenReturn(library);
		when(library.getSequence("Employee_Sequence")).thenReturn(sequence);
		when(Long.valueOf(sequence.next())).thenReturn(Long.valueOf(25));
		final Employee employee = new Employee();
		IEntityProxy proxy = operations.save(employee);
		assertTrue(proxy instanceof Employee);
		assertNull(proxy.__getRid());
		assertEqual(((Employee) proxy).getId(), Long.valueOf(25));
	}
	
	
	@Entity
	static class User {
		
		@RId
		@Nullable private String rid;
		@Id
		@Nullable private Long id;
	}
	
	@Entity
	static class NewEntity {
		
		@RId
		@Nullable private String rid;
	}
	

}
