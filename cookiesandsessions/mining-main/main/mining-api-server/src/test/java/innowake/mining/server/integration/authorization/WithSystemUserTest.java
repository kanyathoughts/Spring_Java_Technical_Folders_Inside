/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.authorization;

import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.server.aspect.SystemUserTestComponent;
import innowake.mining.server.aspect.WithSystemUser;
import innowake.mining.shared.service.UserRoleService;

/**
 * Tests proper behavior of the {@link WithSystemUser} annotation.
 */
public class WithSystemUserTest extends AuthorizationTests {

	@Autowired
	private UserRoleService userRoleService;
	
	@Autowired
	private SystemUserTestComponent testComponent;
	
	@Test
	@WithMockUser
	public void testSystemUserIsAdmin() {
		/* first, check that the default mock user is not an admin */
		assertFalse(userRoleService.isAdmin());
		/* second, check that user is also not admin when executing another, not annotated method */
		testComponent.checkUserIsNotAdminInsideNotAnnotatedMethod();
		/* third, check that user is admin inside of a method annotated with @WithSystemUser */
		testComponent.checkUserIsAdminInsideAnnotatedMethod();
		/* finally, check that user is still not admin after leaving the annotated method */
		assertFalse(userRoleService.isAdmin());
	}
	
	
}
