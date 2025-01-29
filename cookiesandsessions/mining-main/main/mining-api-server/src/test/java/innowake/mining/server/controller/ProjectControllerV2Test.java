/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.server.controller;

import static innowake.mining.server.controller.ProjectControllerV2.PROJECT_COLLECTIONS_FOR_CLIENT_URL;
import static innowake.mining.server.controller.ProjectControllerV2.PROJECT_COUNT_FOR_CLIENT_URL;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.junit.jupiter.api.Test;

import innowake.mining.shared.entities.ProjectPojo;

/**
 * Tests for project and client deletion.
 */
class ProjectControllerV2Test extends BaseProjectControllerTest {
	
	/**
	 * Tests {@link ProjectController#findProjectsForClient(HttpServletRequest, HttpServletResponse, Long)} for a project that is linked with a client which
	 * was marked as to be deleted.
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@Test
	void testFindProjectsForToBeDeletedClient() throws Exception {
		testClientMarkedAsDeleted(PROJECT_COLLECTIONS_FOR_CLIENT_URL, ProjectPojo::getClientNid);
	}

	/**
	 * Tests {@link ProjectController#findNumberOfProjectsByClient(HttpServletRequest, HttpServletResponse, Long)} for a project that is linked with a client
	 * which was marked as to be deleted.
	 *
	 * @throws Exception when the mocked call was not successful
	 */
	@Test
	void findNumberOfProjectsForToBeDeletedClient() throws Exception {
		testClientMarkedAsDeleted(PROJECT_COUNT_FOR_CLIENT_URL, ProjectPojo::getClientNid);
	}
}
