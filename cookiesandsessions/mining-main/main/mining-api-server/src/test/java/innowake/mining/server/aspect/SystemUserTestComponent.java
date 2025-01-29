/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.aspect;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import innowake.mining.server.integration.authorization.WithSystemUserTest;
import innowake.mining.shared.service.UserRoleService;

/**
 * Test component used by {@link WithSystemUserTest}.
 */
@Component
public class SystemUserTestComponent {

	@Autowired
	private UserRoleService userRoleService;
	
	public void checkUserIsNotAdminInsideNotAnnotatedMethod() {
		assertFalse(userRoleService.isAdmin());
	}
	
	@WithSystemUser
	public void checkUserIsAdminInsideAnnotatedMethod() {
		assertTrue(userRoleService.isAdmin());
	}
}
