/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization.graphql;

import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.graphql.test.tester.GraphQlTester.Response;
import org.springframework.graphql.test.tester.HttpGraphQlTester;
import org.springframework.test.web.servlet.client.MockMvcWebTestClient;
import org.springframework.web.context.WebApplicationContext;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.event.CustomPropertiesModifiedEvent;
import innowake.mining.server.event.TaxonomiesModifiedEvent;
import innowake.mining.server.integration.DatabaseResettingTest;
import innowake.mining.shared.access.EntityId;

/**
 * Tests for querying data from project specific schemas.
 */
class GraphQlProjectSpecificTests extends DatabaseResettingTest {

	@Nullable
	private static final Long SCHEMA_GENERAL = null;
	private static final Long SCHEMA_PROJECT_1 = Long.valueOf(1);
	
	@Autowired
	private WebApplicationContext webAppContext;

	@Autowired
	private ApplicationEventPublisher eventPublisher;
	
	private HttpGraphQlTester buildTester(@Nullable final Long schemaProjectId) {
		return HttpGraphQlTester.create(
				MockMvcWebTestClient.bindToApplicationContext(webAppContext)
					.configureClient()
					.baseUrl(GraphQlAuthorizationTests.GRAPHQL_ENDPOINT + (schemaProjectId != null ? "?projectId=" + schemaProjectId : ""))
					.build()
			);
	}
	
	@Override
	protected ResetScriptFile getScriptFile() {
		return ResetScriptFile.COMPLETE;
	}
	
	/**
	 * The graphql project specific test data scripts to be executed.
	 *
	 * @return SQLs to be executed separated by semicolon
	 */
	@Override
	protected List<String> getAdditionalPgScriptFile() {
		return List.of("test-data-graphql-project", "test-data-custom-properties");
	}
	
	private Response query(@Nullable final Long schemaProjectId, final String q) {
		if (schemaProjectId != null) {
			/* If the tests are executed and the test projects do not yet exist in the DB, then the MiningDataPointSource impls are unable to provide
			 * any data points for custom properties of the projects in CustomPropertyDataPointSource.provideDataPoints(MiningDataPointBuilder). The
			 * MiningDataPointSource.provideDataPoints() methods are called when the spring components & services are initialized but not again after
			 * a test project was created which is why we need to do it manually here. */
			eventPublisher.publishEvent(new CustomPropertiesModifiedEvent(Optional.of(EntityId.of(schemaProjectId))));

			/* If the tests are executed and the test projects do not yet exist in the DB, then the MiningDataPointSource impls are unable to provide
			 * any data points for taxonomies of the projects in TaxonomyDataPointSource.provideDataPoints(MiningDataPointBuilder). The
			 * MiningDataPointSource.provideDataPoints() methods are called when the spring components & services are initialized but not again after
			 * a test project or taxonomies were created which is why we need to do it manually here. */
			eventPublisher.publishEvent(new TaxonomiesModifiedEvent(schemaProjectId));
		}

		return buildTester(schemaProjectId).document(q).execute();
	}

	@Test
	void testQueryProject() {
		query(SCHEMA_GENERAL, "{ project(projectId: 1) { name clientNid } }").path("")
			.matchesJson("{\"project\":{\"name\":\"Demo Project A\",\"clientNid\":1}}}");
	}

	@Test
	void testQueryModuleCustomProperties() {
		query(SCHEMA_PROJECT_1, "{ modules(projectId: 1, filterObject: {content_name: {in: [\"PRGA\", \"PRGB\", \"PRGC\"]}}) { content { name customProperties { ModuleCustomProperties { customMetaInfo1 customMetaInfo2 } } } } }").path("")
			.matchesJson("{\"modules\":{\"content\":["
					+ "{\"name\":\"PRGA\",\"customProperties\":{\"ModuleCustomProperties\":{\"customMetaInfo1\":\"some custom meta 1A value\",\"customMetaInfo2\":\"some custom meta 2A value\"}}},"
					+ "{\"name\":\"PRGB\",\"customProperties\":{\"ModuleCustomProperties\":{\"customMetaInfo1\":\"some custom meta 1B value\",\"customMetaInfo2\":\"some custom meta 2B value\"}}},"
					+ "{\"name\":\"PRGC\",\"customProperties\":{\"ModuleCustomProperties\":{\"customMetaInfo1\":null,\"customMetaInfo2\":null}}}"
					+ "]}}");
	}

	@Test
	void testQueryModuleTaxonomies() {
		query(SCHEMA_PROJECT_1, "{ modules(projectId: 1, filterObject: {content_id: {in: [2000, 2001, 2002]}}) { content { name taxonomy { dataDomain businessProcess businessSubsystem } } } }").path("")
			.matchesJson("{\"modules\":{\"content\":["
					+ "{\"name\":\"PRG1\",\"taxonomy\":{\"dataDomain\":[\"Employee domain\"],\"businessProcess\":[],\"businessSubsystem\":[\"ARB100\"]}},"
					+ "{\"name\":\"QBGPSLP1MMRS710A.STEP01.MMRS7102\",\"taxonomy\":{\"dataDomain\":[\"Employee domain\"],\"businessProcess\":[],\"businessSubsystem\":[]}},"
					+ "{\"name\":\"MMRS7101\",\"taxonomy\":{\"dataDomain\":[],\"businessProcess\":[],\"businessSubsystem\":[]}}"
					+ "]}}");
	}

}
