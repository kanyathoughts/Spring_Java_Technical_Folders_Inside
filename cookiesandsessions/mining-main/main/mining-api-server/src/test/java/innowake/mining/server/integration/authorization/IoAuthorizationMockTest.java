/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.authorization;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

import innowake.mining.server.config.Profiles;
import innowake.mining.server.controller.IoController;

/**
 * Authorization tests for end-points in {@link IoController}.
 */
@ActiveProfiles(value = Profiles.AUTH_TEST, inheritProfiles = false )
@AutoConfigureMockMvc
class IoAuthorizationMockTest extends AuthorizationTests {

	@Autowired
	private MockMvc mockMvc;
	private static final Long PROJECT_ONE = Long.valueOf(1); 
	
	/**
	 * User with admin access on the entire application.
	 * 
	 * @throws Exception when something with the call fails
	 */
	@Test
	void exportToFormatAdminRole() throws Exception {
		mockMvc.perform(buildExportToFormatRequest().with(miningUser(roles(admin()))))
			.andDo(print())
			.andExpect(status().is2xxSuccessful());
	}
	
	/**
	 * User with mining nature and viewer role on the project.
	 * 
	 * @throws Exception when something with the call fails
	 */
	@Test
	void exportToFormatMiningNatureViewerRole() throws Exception {
		mockMvc.perform(buildExportToFormatRequest().with(miningUser(miningRoles("client-1-project-1-mining", "client-1-project-1-viewer"))))
			.andDo(print())
			.andExpect(status().is2xxSuccessful());
	}
	
	/**
	 * User with no mining nature and any role on the project.
	 * 
	 * @throws Exception when something with the call fails
	 */
	@Test
	void exportToFormatNoNatureNoRole() throws Exception {
		mockMvc.perform(buildExportToFormatRequest().with(miningUser(miningRoles("client-2-project-3-mining", "client-2-project-3-viewer"))))
			.andDo(print())
			.andExpect(status().isForbidden());
	}
	
	/**
	 * User with no mining nature and viewer role on the project.
	 * 
	 * @throws Exception when something with the call fails
	 */
	@Test
	void exportToFormatDiscoveryNatureNoRole() throws Exception {
		mockMvc.perform(buildExportToFormatRequest().with(miningUser(miningRoles("client-1-project-1-discovery", "client-1-project-1-viewer"))))
			.andDo(print())
			.andExpect(status().isForbidden());
	}
	
	/**
	 * User with no nature but admin role on the client.
	 * 
	 * @throws Exception when something with the call fails
	 */
	@Test
	void exportToFormatNoNatureAdminRole() throws Exception {
		mockMvc.perform(buildExportToFormatRequest().with(miningUser(miningRoles("client-2-project-3-mining", "client-1-admin"))))
			.andDo(print())
			.andExpect(status().is2xxSuccessful());
	}
	
	/**
	 * User with mining nature but admin role on a different client.
	 * 
	 * @throws Exception when something with the call fails
	 */
	@Test
	void exportToFormatNoNatureAdminRoleOnDifferentClient() throws Exception {
		mockMvc.perform(buildExportToFormatRequest().with(miningUser(miningRoles("client-1-project-1-mining", "client-2-admin"))))
			.andDo(print())
			.andExpect(status().isForbidden());
	}
	
	private MockHttpServletRequestBuilder buildExportToFormatRequest() {
		return post("/api/v2/projects/{projectId}/job-extensions/{extensionId}", PROJECT_ONE, "datapoint-csv").queryParam("$query", "moduleDependencies")
				.queryParam("$columns", "content.targetName");
	}
}
