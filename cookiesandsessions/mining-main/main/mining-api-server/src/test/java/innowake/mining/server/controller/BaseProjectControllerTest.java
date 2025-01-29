/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.controller;

import static org.springframework.http.MediaType.APPLICATION_JSON;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Collections;
import java.util.HashSet;
import java.util.function.Function;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import innowake.mining.data.access.postgres.ClientPgDao;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.ClientService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;

/**
 * Abstract base class for testing the {@link ProjectController} and {@link ProjectControllerV2}.
 */
@AutoConfigureMockMvc
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
abstract class BaseProjectControllerTest extends DatabaseRelatedTest {

	@Autowired
	protected MockMvc mvc;

	@Autowired
	private ClientService clientService;

	@Autowired
	@Qualifier("postgres")
	private JdbcTemplate jdbcTemplate;
	
	/* counter for createTestData() for unique client and project names */
	private static int testCount = 1;

	/**
	 * Generic test method that creates a test client and project and then performs the call. The call must return HTTP status code {@code 200}.
	 * <p>After the first call was successful the client is marked as to be deleted in the DB and the call is executed again. The call must
	 * return HTTP status code {@code 404}.
	 *
	 * @param url The URL of the call
	 * @param uriVarSupplier A {@link Function} that returns the URI variable for the call, id or rid of the test project
	 * @throws Exception when the mocked call was not successful
	 */
	void testClientMarkedAsDeleted(final String url, final Function<ProjectPojo, ?> uriVarSupplier) throws Exception {
		final ProjectPojo project = createTestData();

		try {
			final Object uriVar = uriVarSupplier.apply(project);

			/* Call for client which is not marked as deleted */
			mvc.perform(get("/api" + url, uriVar)
					.contentType(APPLICATION_JSON))
					.andExpect(status().isOk());

			markClientAsToBeDeleted(project.getClientNid());

			/* must always be 404 after client was marked as to be deleted */
			mvc.perform(get("/api" + url, uriVar)
					.contentType(APPLICATION_JSON))
					.andExpect(status().isNotFound());
		} finally {
			clientService.deleteDirectly(project.getClient());
		}
	}

	/**
	 * Creates a new client with a new project for testing.
	 *
	 * @return the {@link ProjectPojo}
	 */
	private ProjectPojo createTestData() {
		return projectService.create(new ProjectPojoPrototype()
				.setClient(clientService.create("ToBeDeletedClientWithProject" + testCount).identity())
				.setName("ToBeDeletedProject" + testCount++)
				.setNatures(new HashSet<>(Collections.emptyList())) );
	}

	/**
	 * Marks only the client for the given {@code clientId} as to be deleted,
	 * circumventing the regular store implementation to prevent the deletion job from being triggered.
	 *
	 * @param clientId the client id
	 */
	private void markClientAsToBeDeleted(final Long clientId) {
		ClientPgDao clientDao = new ClientPgDao(jdbcTemplate);
		clientDao.markForDeletion(EntityId.of(clientId));
	}

	@Override
	protected ResetScriptFile getScriptFile() {
		return ResetScriptFile.COMPLETE;
	}

}
