/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.aspect;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.web.servlet.MockMvc;

import innowake.mining.server.config.security.MiningRole;
import innowake.mining.server.controller.AnnotationController;
import innowake.mining.server.integration.authorization.AbstractUserNameTest;
import innowake.mining.server.integration.authorization.AuthorizationTests;
import innowake.mining.shared.model.Member;
import innowake.mining.shared.model.ProjectNature;
import innowake.mining.shared.model.ProjectRole;
import innowake.mining.shared.model.UserRole;

/**
 * Tests for the {@link UserNameAspect}.
 */
public class UserNameAspectTest extends AbstractUserNameTest {

	@Autowired
	private MockMvc mockMvc;
	private static final Long ONE = Long.valueOf(1);

	/**
	 * Test to verify if the user name is populated for the {@code BaseController} methods returning {@code UserIdentifyable}. 
	 *
	 * @throws Exception during the execution
	 */
	@Test
	void testEndpointReturningUserIdentifyable() throws Exception {
		AuthorizationTests.setAuthentication(Arrays.asList(
			new MiningRole("client-1-project-1-mining"),
			new MiningRole("client-1-project-1-viewer")
		));
		mockKeycloakExtensionResponse();
		mockMvc.perform(get("/api" + AnnotationController.ANNOTATION_BY_ID_URL, ONE, ONE))
				.andExpect(status().is2xxSuccessful())
				.andExpect(jsonPath("$.createdByUserName").value("super user"))
				.andExpect(jsonPath("$.updatedByUserName").value("super user"));
	}

	/**
	 * Test to verify if the user name is populated for the {@code BaseController} methods returning {@code Iterable}. 
	 *
	 * @throws Exception during the execution
	 */
	@Test
	void testEndpointReturningListOfUserIdentifyable() throws Exception {
		AuthorizationTests.setAuthentication(Arrays.asList(
			new MiningRole("client-1-project-1-mining"),
			new MiningRole("client-1-project-1-viewer")
		));
		mockKeycloakExtensionResponse();
		mockMvc.perform(get("/api" + AnnotationController.ANNOTATION_SEARCH_URL, ONE)
				.queryParam("name", "Annotation 1"))
				.andExpect(status().is2xxSuccessful())
				.andExpect(jsonPath("$[0].createdByUserName").value("super user"))
				.andExpect(jsonPath("$[0].updatedByUserName").value("super user"))
				.andExpect(jsonPath("$[1].createdByUserName").value("SYSTEM"))
				.andExpect(jsonPath("$[1].updatedByUserName").value("super user"));
	}

	@Override
	protected ResetScriptFile getScriptFile() {
		return ResetScriptFile.COMPLETE;
	}

	private void mockKeycloakExtensionResponse() {
		final List<ProjectNature> projectNatures = Collections.singletonList(ProjectNature.MINING);
		final List<ProjectRole> projectRoles = Collections.singletonList(new ProjectRole(Long.valueOf(3), UserRole.EDITOR, projectNatures));
		final Member adminMember = new Member("admin", "super", "user", "admin@email.com", projectRoles);
		final ResponseEntity<Member> mockedResponse = new ResponseEntity<>(adminMember, HttpStatus.OK);
		given(assertNotNull(keycloakRestTemplate).getForEntity(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/users/admin", Member.class))
				.willReturn(mockedResponse);

		final Member systemUserMember = new Member("system_user", "system", "user", "system@email.com", projectRoles);
		final ResponseEntity<Member> mockedResponse1 = new ResponseEntity<>(systemUserMember, HttpStatus.OK);
		given(assertNotNull(keycloakRestTemplate).getForEntity(MOCK_HOST + "/realms/" + MOCK_REALM + "/admin/users/system_user", Member.class))
				.willReturn(mockedResponse1);
	}

}
