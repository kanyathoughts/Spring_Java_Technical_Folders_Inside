/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.plugin.test;

import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.StringContains.containsString;
import static org.junit.Assert.assertNotNull;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.fail;

import org.apache.commons.lang.exception.ExceptionUtils;
import org.junit.Test;
import org.junit.jupiter.api.Disabled;

/**
 * Smoke tests for the server side functions.
 */
@Disabled
public class SmokeTest extends AbstractTest {

	@Test
	public void testStoreAstFunctionThrowsErrorWhenNoArgumentsWereGiven() {
		try {
			executeSQL("SELECT storeAst()");
			fail("An exception was expected.");
		} catch(final Exception e) {
			final Throwable rootCause = ExceptionUtils.getRootCause(e);
			assertNotNull(rootCause);
			assertThat(rootCause, instanceOf(IllegalArgumentException.class));
			final String message = rootCause.getLocalizedMessage();
			assertThat(message, containsString("Function was called with the wrong number of arguments"));
		}
	}
	
	@Test
	public void testStoreAstFunctionThrowsErrorWhenTooManyArgumentsWereGiven() {
		try {
			executeSQL("SELECT storeAst(1,1)");
			fail("An exception was expected.");
		} catch (final Exception e) {
			final Throwable rootCause = ExceptionUtils.getRootCause(e);
			assertNotNull(rootCause);
			assertThat(rootCause, instanceOf(IllegalArgumentException.class));
			final String message = rootCause.getLocalizedMessage();
			assertThat(message, containsString("Function was called with the wrong number of arguments"));
		}
	}
	
	@Test
	public void testStoreAstFunctionValidatesParameterType() {
		try {
			executeSQL("SELECT storeAst('hello')");
			fail("An exception was expected.");
		} catch (final Exception e) {
			final Throwable rootCause = ExceptionUtils.getRootCause(e);
			assertNotNull(rootCause);
			assertThat(rootCause, instanceOf(IllegalArgumentException.class));
			final String message = rootCause.getLocalizedMessage();
			assertThat(message, containsString("Wrong type of argument 'module ID'. Expected Number but was String"));
		}
	}
	
	@Test
	public void printFunctionCanBeCalled() {
		executeSQL("SELECT print(id) FROM Module WHERE path='src/cobol/programs/PROG1.cbl' AND projectLink.name='Demo Project C'");
	}

	@Test
	public void printStrucutreFunctionCanBeCalled() {
		executeSQL("SELECT printStructure(id) FROM Module WHERE path='src/cobol/programs/PROG1.cbl' AND projectLink.name='Demo Project C'");
	}
}
