/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.config;

import java.lang.reflect.InvocationTargetException;

import org.flywaydb.core.Flyway;
import org.flywaydb.core.internal.sqlscript.FlywaySqlScriptException;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.flyway.FlywayMigrationInitializer;
import org.springframework.boot.autoconfigure.flyway.FlywayMigrationStrategy;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * This is a workaround for WMIN-4402. If an exception is thrown during Flyway migrations,
 * the exception is first caught internally by Spring and causes the application server startup to abort.
 * Then the exception is rethrown. Log4j then tries to log the exception. This transitively invokes {@link FlywaySqlScriptException#getMessage()},
 * which attempts to access the ClassLoader to get the path of the resource (SQL Script) that contains the migration that failed.
 * Since the application server is already shut down, this call fails. This in turn causes log4j to fail internally, causing the entire
 * stack trace and error message of the original migration Exception to be lost.
 */
@Configuration
@ConditionalOnProperty(name = "spring.flyway.enabled")
public class FlywayInitializationWorkaroundConfig {

	/**
	 * "Overrides" {@link org.springframework.boot.autoconfigure.flyway.FlywayAutoConfiguration.FlywayConfiguration#flywayInitializer(Flyway, ObjectProvider)}
	 * due to that method's {@code @ConditionalOnMissingBean} annotation
	 *
	 * @param flyway required by FlywayMigrationInitializer
	 * @param migrationStrategy required by FlywayMigrationInitializer
	 * @return the FlywayMigrationInitializer instance which invokes Flyway migrations
	 */
	@Bean
	public FlywayMigrationInitializer flywayInitializer(final Flyway flyway, final ObjectProvider<FlywayMigrationStrategy> migrationStrategy) {
		return new FlywayMigrationInitializer(flyway, migrationStrategy.getIfAvailable()) {
			@Override
			public void afterPropertiesSet() throws Exception {
				try {
					super.afterPropertiesSet();
				} catch (final Exception e) {
					final Throwable actualException;
					if (e instanceof InvocationTargetException) {
						/* "sometimes" the super.afterPropertiesSet() call uses reflection to invoke the actual migration ... "-.- */
						actualException = e.getCause();
					} else {
						actualException = e;
					}
					if (actualException.getCause() instanceof FlywaySqlScriptException) {
						/* We MUST call FlywaySqlScriptException.getMessage() right here and now! Attempting to call it later will fail 
						 * as the implementation of getMessage() attempt to access the class loader and fails if the server has already been shut down.
						 * We replace the broken FlywaySqlScriptException by IllegalStateException. */
						final FlywaySqlScriptException scriptException = (FlywaySqlScriptException) actualException.getCause();
						throw new IllegalStateException("Flyway migration failed:" + scriptException.getMessage(), scriptException.getCause());
					} else {
						throw e;
					}
				}
			}
		};
	}
}
