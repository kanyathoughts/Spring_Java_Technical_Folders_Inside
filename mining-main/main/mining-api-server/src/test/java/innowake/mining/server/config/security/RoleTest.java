/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.config.security;

import static innowake.mining.shared.security.RoleType.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

/**
 * Tests the hierarchy of {@link Role.Type}.
 */
class RoleTest {

	@Test
	final void adminTest() {
		assertTrue(ADMIN.contains(ADMIN));
		assertTrue(ADMIN.contains(CLIENT_ADMIN));
		assertTrue(ADMIN.contains(MANAGER));
		assertTrue(ADMIN.contains(EDITOR));
		assertTrue(ADMIN.contains(VIEWER));
	}

	@Test
	final void clientAdminTest() {
		assertFalse(CLIENT_ADMIN.contains(ADMIN));
		assertTrue(CLIENT_ADMIN.contains(CLIENT_ADMIN));
		assertTrue(CLIENT_ADMIN.contains(MANAGER));
		assertTrue(CLIENT_ADMIN.contains(EDITOR));
		assertTrue(CLIENT_ADMIN.contains(VIEWER));
	}
	
	@Test
	final void managerTest() {
		assertFalse(MANAGER.contains(ADMIN));
		assertFalse(MANAGER.contains(CLIENT_ADMIN));
		assertTrue(MANAGER.contains(MANAGER));
		assertTrue(MANAGER.contains(EDITOR));
		assertTrue(MANAGER.contains(VIEWER));
	}

	@Test
	final void editorTest() {
		assertFalse(EDITOR.contains(ADMIN));
		assertFalse(EDITOR.contains(CLIENT_ADMIN));
		assertFalse(EDITOR.contains(MANAGER));
		assertTrue(EDITOR.contains(EDITOR));
		assertTrue(EDITOR.contains(VIEWER));
	}

	@Test
	final void viewerTest() {
		assertFalse(VIEWER.contains(ADMIN));
		assertFalse(VIEWER.contains(CLIENT_ADMIN));
		assertFalse(VIEWER.contains(MANAGER));
		assertFalse(VIEWER.contains(EDITOR));
		assertTrue(VIEWER.contains(VIEWER));
	}
}
