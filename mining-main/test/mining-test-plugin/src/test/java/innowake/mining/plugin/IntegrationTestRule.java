/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static java.lang.Boolean.parseBoolean;
import static org.junit.Assume.assumeFalse;

import org.junit.rules.TestRule;
import org.junit.runner.Description;
import org.junit.runners.model.Statement;

import innowake.lib.core.api.lang.Nullable;

/**
 * Rule checking if the system property {@code skipIntegrationTests} is set and if so, skips the tests.
 */
public class IntegrationTestRule implements TestRule {

	@Override
	public Statement apply(@Nullable final Statement base, @Nullable final Description description) {
		return new Statement() {

			@Override
			public void evaluate() throws Throwable {
				assumeFalse("Test skipped because skipIntegrationTests system property was provided.", parseBoolean(System.getProperty("skipIntegrationTests")));
				assertNotNull(base).evaluate();
			}
			
		};
	}

}
