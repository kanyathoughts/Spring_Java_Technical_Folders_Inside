/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.commons.core;

import static innowake.lib.core.lang.Assert.assertNotNull;

import org.junit.Test;
import org.springframework.cglib.reflect.FastClass;

import innowake.spring.data.orientdb.integration.repository.domain.Employee;
import innowake.spring.data.orientdb.ogm.proxy.IEntityProxy;

/**
 * Test class for {@link IEntityProxy}.
 */
public class EntityClassUnitTests {
	
	/**
	 * Test case  to invoke method.
	 */
	@Test
	public void testInvokeMethod() {
		final Employee employee = new Employee("user", "user", "user");
		final FastClass service = FastClass.create(employee.getClass());
		final Class<?>[] parameterTypes = {};
		assertNotNull(service.getMethod("getEmail", parameterTypes));
	}

}
