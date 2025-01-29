/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static java.lang.Long.valueOf;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.authentication.TestingAuthenticationToken;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.request.RequestPostProcessor;
import org.springframework.web.client.RestTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.config.Profiles;
import innowake.mining.server.config.security.MiningRole;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;
import innowake.mining.tags.AuthorizationTest;

/**
 * Base class for authorization tests. This class only defines some helper methods.
 */
@ActiveProfiles(value = Profiles.AUTH_TEST, inheritProfiles = false )
@AutoConfigureMockMvc
@AuthorizationTest
public abstract class AuthorizationTests extends DatabaseRelatedTest {

	public static final String TEST_USER_ID = "miningTester-with-UUID-string-length";
	
	protected static final String ADMIN = RoleType.ADMIN.getValue();
	protected static final String MANAGER = RoleType.MANAGER.getValue();
	protected static final String EDITOR = RoleType.EDITOR.getValue();
	protected static final String VIEWER = RoleType.VIEWER.getValue();

	protected static final String MINING = NatureType.MINING.getValue();
	protected static final String DISCOVERY = NatureType.DISCOVERY.getValue();
	protected static final String DISCOVERY_LIGHT = NatureType.DISCOVERY_LIGHT.getValue();
	
	@MockBean
	@Nullable
	protected RestTemplate keycloakRestTemplate;
	
	/**
	 * Convenience method for defining the roles which should be used for the test.
	 * 
	 * @param roles the roles with which the test should make the call
	 * @return a list of roles
	 */
	protected static List<GrantedAuthority> roles(final GrantedAuthority... roles) {
		return Arrays.asList(roles);
	}
	
	/**
	 * Convenience method for defining the Mining roles which should be used for the test.
	 * 
	 * @param roles the roles with which the test should make the call
	 * @return a list of roles
	 */
	protected static List<GrantedAuthority> miningRoles(final String... roles) {
		return Arrays.stream(roles).map(s -> new MiningRole(s)).collect(Collectors.toList());
	}
	
	/**
	 * Convenience method for setting up the given roles in the call context.
	 *
	 * @param roles the roles that should be used
	 * @return a request post processor for setting the mocked security context
	 */
	protected static RequestPostProcessor miningUser(final List<GrantedAuthority> roles) {
		return SecurityMockMvcRequestPostProcessors.user("doNotCare").authorities(roles);
	}
	
	/**
	 * Set a new authentication context with the given roles.
	 *
	 * @param roles List of roles.
	 */
	public static void setAuthentication(final List<GrantedAuthority> roles) {
		SecurityContextHolder.getContext().setAuthentication(new TestingAuthenticationToken(TEST_USER_ID, "mocked", roles));
	}
	
	/**
	 * Convenience method for the admin role for usage with {@link AuthorizationTests#roles(GrantedAuthority...)}.
	 *
	 * @return the role name
	 */
	protected static MiningRole admin() {
		return new MiningRole(ADMIN);
	}
	
	/**
	 * Convenience method for the client-admin role on the client with ID 1 for usage with {@link AuthorizationTests#roles(GrantedAuthority...)}.
	 *
	 * @return the role name
	 */
	protected static MiningRole clientAdmin() {
		return clientAdmin(1);
	}
	
	/**
	 * Convenience method for the client-admin role on the client with the given ID for usage with {@link AuthorizationTests#roles(GrantedAuthority...)}.
	 * 
	 * @param clientId the ID of the client
	 * @return the role name
	 */
	protected static MiningRole clientAdmin(final long clientId) {
		return new MiningRole(String.format("client-%d-admin", valueOf(clientId)));
	}

	/**
	 * Convenience method for the MINING MANAGER role on the client/project with the ID 1 for usage with {@link AuthorizationTests#roles(GrantedAuthority...)}.
	 *
	 * @return the role name
	 */
	protected static MiningRole[] miningManager() {
		return miningManager(1, 1);
	}

	/**
	 * Convenience method for the MINING MANAGER role on the client/project with the given IDs for usage with {@link AuthorizationTests#roles(GrantedAuthority...)}.
	 * 
	 * @param clientId the ID of the client
	 * @param projectId the ID of the project
	 * @return the role name
	 */
	protected static MiningRole[] miningManager(final long clientId, final long projectId) {
		return roles(clientId, projectId, MANAGER, MINING);
	}
	
	/**
	 * Convenience method for the MINING EDITOR role on the client/project with the ID 1 for usage with {@link AuthorizationTests#roles(GrantedAuthority...)}.
	 *
	 * @return the role name
	 */
	protected static MiningRole[] miningEditor() {
		return miningEditor(1, 1);
	}

	/**
	 * Convenience method for the MINING EDITOR role on the client/project with the given IDs for usage with {@link AuthorizationTests#roles(GrantedAuthority...)}.
	 * 
	 * @param clientId the ID of the client
	 * @param projectId the ID of the project
	 * @return the role name
	 */
	protected static MiningRole[] miningEditor(final long clientId, final long projectId) {
		return roles(clientId, projectId, EDITOR, MINING);
	}
	
	/**
	 * Convenience method for the MINING VIEWER role on the client/project with the ID 1 for usage with {@link AuthorizationTests#roles(GrantedAuthority...)}.
	 *
	 * @return the role name
	 */
	protected static MiningRole[] miningViewer() {
		return miningViewer(1, 1);
	}

	/**
	 * Convenience method for the MINING VIEWER role on the client/project with the given IDs for usage with {@link AuthorizationTests#roles(GrantedAuthority...)}.
	 * 
	 * @param clientId the ID of the client
	 * @param projectId the ID of the project
	 * @return the role name
	 */
	protected static MiningRole[] miningViewer(final long clientId, final long projectId) {
		return roles(clientId, projectId, VIEWER, MINING);
	}
	
	/**
	 * Convenience method for the DISCOVERY MANAGER role on the client/project with the ID 1 for usage with {@link AuthorizationTests#roles(GrantedAuthority...)}.
	 *
	 * @return the role name
	 */
	protected static MiningRole[] discoveryManager() {
		return discoveryManager(1, 1);
	}

	/**
	 * Convenience method for the DISCOVERY MANAGER role on the client/project with the given IDs for usage with {@link AuthorizationTests#roles(GrantedAuthority...)}.
	 * 
	 * @param clientId the ID of the client
	 * @param projectId the ID of the project
	 * @return the role name
	 */
	protected static MiningRole[] discoveryManager(final long clientId, final long projectId) {
		return roles(clientId, projectId, MANAGER, DISCOVERY);
	}
	
	/**
	 * Convenience method for the DISCOVERY EDITOR role on the client/project with the ID 1 for usage with {@link AuthorizationTests#roles(GrantedAuthority...)}.
	 *
	 * @return the role name
	 */
	protected static MiningRole[] discoveryEditor() {
		return discoveryEditor(1, 1);
	}

	/**
	 * Convenience method for the DISCOVERY EDITOR role on the client/project with the given IDs for usage with {@link AuthorizationTests#roles(GrantedAuthority...)}.
	 * 
	 * @param clientId the ID of the client
	 * @param projectId the ID of the project
	 * @return the role name
	 */
	protected static MiningRole[] discoveryEditor(final long clientId, final long projectId) {
		return roles(clientId, projectId, EDITOR, DISCOVERY);
	}
	
	/**
	 * Convenience method for the DISCOVERY VIEWER role on the client/project with the ID 1 for usage with {@link AuthorizationTests#roles(GrantedAuthority...)}.
	 *
	 * @return the role name
	 */
	protected static MiningRole[] discoveryViewer() {
		return discoveryViewer(1, 1);
	}

	/**
	 * Convenience method for the DISCOVERY VIEWER role on the client/project with the given IDs for usage with {@link AuthorizationTests#roles(GrantedAuthority...)}.
	 * 
	 * @param clientId the ID of the client
	 * @param projectId the ID of the project
	 * @return the role name
	 */
	protected static MiningRole[] discoveryViewer(final long clientId, final long projectId) {
		return roles(clientId, projectId, VIEWER, DISCOVERY);
	}
	
	/**
	 * Convenience method for the DISCOVERY-LIGHT MANAGER role on the client/project with the ID 1 for usage with {@link AuthorizationTests#roles(GrantedAuthority...)}.
	 *
	 * @return the role name
	 */
	protected static MiningRole[] discoveryLightManager() {
		return discoveryLightManager(1, 1);
	}

	/**
	 * Convenience method for the DISCOVERY-LIGHT MANAGER role on the client/project with the given IDs for usage with {@link AuthorizationTests#roles(GrantedAuthority...)}.
	 * 
	 * @param clientId the ID of the client
	 * @param projectId the ID of the project
	 * @return the role name
	 */
	protected static MiningRole[] discoveryLightManager(final long clientId, final long projectId) {
		return roles(clientId, projectId, MANAGER, DISCOVERY_LIGHT);
	}
	
	/**
	 * Convenience method for the DISCOVERY-LIGHT EDITOR role on the client/project with the ID 1 for usage with {@link AuthorizationTests#roles(GrantedAuthority...)}.
	 *
	 * @return the role name
	 */
	protected static MiningRole[] discoveryLightEditor() {
		return discoveryLightEditor(1, 1);
	}

	/**
	 * Convenience method for the DISCOVERY-LIGHT EDITOR role on the client/project with the given IDs for usage with {@link AuthorizationTests#roles(GrantedAuthority...)}.
	 * 
	 * @param clientId the ID of the client
	 * @param projectId the ID of the project
	 * @return the role name
	 */
	protected static MiningRole[] discoveryLightEditor(final long clientId, final long projectId) {
		return roles(clientId, projectId, EDITOR, DISCOVERY_LIGHT);
	}
	
	/**
	 * Convenience method for the DISCOVERY-LIGHT VIEWER role on the client/project with the ID 1 for usage with {@link AuthorizationTests#roles(GrantedAuthority...)}.
	 *
	 * @return the role name
	 */
	protected static MiningRole[] discoveryLightViewer() {
		return discoveryLightViewer(1, 1);
	}

	/**
	 * Convenience method for the DISCOVERY-LIGHT VIEWER role on the client/project with the given IDs for usage with {@link AuthorizationTests#roles(GrantedAuthority...)}.
	 * 
	 * @param clientId the ID of the client
	 * @param projectId the ID of the project
	 * @return the role name
	 */
	protected static MiningRole[] discoveryLightViewer(final long clientId, final long projectId) {
		return roles(clientId, projectId, VIEWER, DISCOVERY_LIGHT);
	}
	
	/**
	 * Returns the pair of project nature and user roles.
	 *
	 * @param clientId the ID of the client
	 * @param projectId the ID of the project
	 * @param userRole the role name of the user role
	 * @param projectNature the role name of the nature role
	 * @return an array containing the project nature and user roles
	 */
	protected static MiningRole[] roles(final long clientId, final long projectId, final String userRole, final String projectNature) {
		final Long cId = valueOf(clientId);
		final Long pId = valueOf(projectId);
		return new MiningRole[] {
			new MiningRole(String.format("client-%d-project-%d-%s", cId, pId, userRole)),
			new MiningRole(String.format("client-%d-project-%d-%s", cId, pId, projectNature))
		};
	}
}
