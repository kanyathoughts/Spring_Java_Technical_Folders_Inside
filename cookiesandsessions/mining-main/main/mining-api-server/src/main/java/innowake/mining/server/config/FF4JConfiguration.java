/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.config;

import org.ff4j.FF4j;
import org.ff4j.springjdbc.store.FeatureStoreSpringJdbc;
import org.ff4j.springjdbc.store.PropertyStoreSpringJdbc;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import javax.sql.DataSource;

/**
 * Configuration class for ff4j.
 */
@Configuration
public class FF4JConfiguration {

	/**
	 * Create the ff4j instance and configure it to use the OrientDB backend.
	 * @param dataSource Database
	 *
	 * @return The configured ff4j instance.
	 */
	@Bean
	@ConditionalOnMissingBean
	public FF4j getFF4j(@Qualifier("postgres") final DataSource dataSource) {
		final FF4j ff4j = new FF4j();

		ff4j.autoCreate(true);

		ff4j.setFeatureStore(new FeatureStoreSpringJdbc(dataSource));
		ff4j.setPropertiesStore(new PropertyStoreSpringJdbc(dataSource));
		ff4j.setEnableAudit(false);

		return ff4j;
	}

}
