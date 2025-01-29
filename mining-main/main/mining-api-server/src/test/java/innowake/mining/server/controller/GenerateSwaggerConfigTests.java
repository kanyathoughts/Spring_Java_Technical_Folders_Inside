/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.controller;

import static java.nio.charset.StandardCharsets.ISO_8859_1;

import java.io.File;

import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.flyway.FlywayMigrationInitializer;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.EnabledIf;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import innowake.mining.server.MiningApiApplication;
import innowake.mining.server.MockedBaseTest;
import innowake.mining.server.config.FF4jFeatureImportRunner;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.service.PostgresIntegrationService;

/**
 * Test class to create the swagger config file needed for the swagger client generation
 * This is the official way to do it: https://github.com/springfox/springfox/issues/1959
 */
@SpringBootTest(classes=MiningApiApplication.class)
@ActiveProfiles(value = {Profiles.LEGACY_AUTH, "dev"}, inheritProfiles = false )
@TestPropertySource(properties= {"mining.cookieId=DISABLED", "postgres.enabled=false"})
@EnabledIf("#{ 'true'.equals( systemProperties[ 'innowake.swagger.generate.config' ] ) }")
class GenerateSwaggerConfigTests extends MockedBaseTest {

	private static final String TARGET_FOLDER = "target/generated-sources/swagger-config.json";

	@Autowired
	WebApplicationContext context;

	@MockBean
	FlywayMigrationInitializer flywayInitializer;

	@MockBean
	JdbcTemplate orientPlainJdbcTemplate;

	@MockBean
	@Qualifier("postgres")
	JdbcTemplate postgresJdbcTemplate;

	@MockBean
	FF4jFeatureImportRunner runner;

	@MockBean
	MiningUiController uiController;

	@MockBean(name="postgres")
	javax.sql.DataSource postgresDataSource;

	@MockBean
	PostgresIntegrationService pgIntegrationService;

	@Test
	void generateSwagger() throws Exception {
		MockMvcBuilders.webAppContextSetup(context).build()
			.perform(MockMvcRequestBuilders.get("/v3/api-docs").accept(MediaType.APPLICATION_JSON))
			.andDo((result) -> FileUtils.writeStringToFile(new File(TARGET_FOLDER), result.getResponse().getContentAsString(), ISO_8859_1));
	}
}
