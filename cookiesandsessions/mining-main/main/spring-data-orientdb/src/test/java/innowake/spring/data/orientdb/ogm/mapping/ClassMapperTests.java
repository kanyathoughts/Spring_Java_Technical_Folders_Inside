/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.ogm.mapping;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;

import java.lang.reflect.Field;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import com.orientechnologies.orient.core.db.ODatabaseDocumentInternal;
import com.orientechnologies.orient.core.record.OElement;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.shared.springdata.annotations.Entity;
import innowake.spring.data.orientdb.commons.core.OrientOperations;
import innowake.spring.data.orientdb.commons.core.SessionManager;
import innowake.spring.data.orientdb.integration.repository.domain.Employee;
import innowake.spring.data.orientdb.ogm.mapping.custom.CustomPropertyService;
import innowake.spring.data.orientdb.ogm.proxy.EntityProxyFactory;
import innowake.spring.data.orientdb.ogm.proxy.IEntityProxy;

/**
 * Test class for {@link ClassDefinitionMapper}.
 */
@RunWith(MockitoJUnitRunner.class)
public class ClassMapperTests {
	
	private static final String LINK_LIST_ERROR = "link list size is wrong";
	private static final String LINKS_SIZE_ERROR = "links size is wrong";

	@Mock
	@Nullable
	private SessionManager sessionManager;
	@Mock
	@Nullable
	private ODatabaseDocumentInternal session;
	@Mock
	@Nullable
	private CustomPropertyService customPropertyService;
	@Mock
	@Nullable
	private OrientOperations<?> orientOperations;
	
	/**
	 * Test case to validate the class definition creation.
	 */
	@Test
	public void testComputeClassDefinition() {
		final Object object = new Employee();
		final ClassDefinition actualClassDefinition = ClassDefinitionMapper.getClassDefinitionMapper().getClassDefinition(object);
		assertNotNull(actualClassDefinition, "class definition not created");
		assertEquals("entityName is wrong", Employee.class.getSimpleName(), actualClassDefinition.getEntityName());
		assertEquals("enum collection field size is wrong", 1 , actualClassDefinition.getEnumCollectionFields().size());
		assertEquals("enum field size is wrong", 1 , actualClassDefinition.getEnumFields().size());
		assertEquals(LINK_LIST_ERROR, 5 , actualClassDefinition.getCollectionOfEntityEdges().size());
		assertEquals(LINKS_SIZE_ERROR, 1 , actualClassDefinition.getEntityEdges().size());
		assertEquals("object fields size is wrong", 17 , actualClassDefinition.getObjectFields().size());
		assertEquals("primitive fields size is wrong", 9 , actualClassDefinition.getPrimitiveFields().size());
		assertEquals("rid field name is wrong", "rid" , Assert.assertNotNull(actualClassDefinition.getRidField()).getName());
	}
	
	/**
	 * Test case to validate the class definition creation with proxy object.
	 *
	 * @throws NoSuchFieldException exception accessing field from Employee class
	 */
	@Test
	public void testComputeClassDefinitionWithIEntityProxy() throws NoSuchFieldException {
		final Object object = new Employee();
		final IEntityProxy iEntityProxy = EntityProxyFactory.create(object, mock(OElement.class),
				assertNotNull(orientOperations), assertNotNull(sessionManager));
		final Field field = Employee.class.getDeclaredField("reportsTo");
		iEntityProxy.__setFieldValue(null, field );
		ClassDefinition actualClassDefinition = ClassDefinitionMapper.getClassDefinitionMapper().getClassDefinition(iEntityProxy);
		assertNotNull(actualClassDefinition, "class definition not created");
		assertEquals("entityName is wrong", Employee.class.getSimpleName(), actualClassDefinition.getEntityName());
		assertEquals("enum collection field size is wrong", 1 , actualClassDefinition.getEnumCollectionFields().size());
		assertEquals("enum field size is wrong", 1 , actualClassDefinition.getEnumFields().size());
		assertEquals(LINK_LIST_ERROR, 5 , actualClassDefinition.getCollectionOfEntityEdges().size());
		assertEquals(LINKS_SIZE_ERROR, 1 , actualClassDefinition.getEntityEdges().size());
		assertEquals("object fields size is wrong", 17 , actualClassDefinition.getObjectFields().size());
		assertEquals("primitive fields size is wrong", 9 , actualClassDefinition.getPrimitiveFields().size());
		assertEquals("rid field name is wrong", "rid" , Assert.assertNotNull(actualClassDefinition.getRidField()).getName());
	}
	
	/**
	 * Only byte array is supported as binary data.
	 *
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testUnSupportedArrayType() {
		final UnSupportedArray unSupportedArray = new UnSupportedArray();
		ClassDefinitionMapper.getClassDefinitionMapper().getClassDefinition(unSupportedArray);
	}
	

	@Entity
	private static class UnSupportedArray {
		@Nullable private String[] stringArray;

		@SuppressWarnings("unused")
		@Nullable
		public String[] getStringArray() {
			return stringArray;
		}

		@SuppressWarnings("unused")
		public void setStringArray(String[] stringArray) {
			this.stringArray = stringArray;
		}
		
	}

}
