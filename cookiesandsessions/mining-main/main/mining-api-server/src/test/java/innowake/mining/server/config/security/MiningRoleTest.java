/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.config.security;

import static innowake.mining.shared.security.RoleType.*;
import static org.junit.Assert.*;

import org.junit.Test;

/**
 * Tests for {@link MiningRole}.
 */
public class MiningRoleTest {

	private static final Long ONE = Long.valueOf(1);
	private static final Long TWO = Long.valueOf(2);
	
	/**
	 * Check admin role.
	 */
	@Test
	public void admin() {
		final MiningRole role = new MiningRole("admin");
		assertFalse("Client ID must not be available", role.clientId().isPresent());
		assertFalse("Project ID must not be available", role.projectId().isPresent());
		assertEquals(ADMIN.getValue(), role.value());
	}

	/**
	 * Check client admin role.
	 */
	@Test
	public void clientAdmin() {
		final MiningRole role = new MiningRole("client-1-admin");
		assertTrue("Client ID must be available", role.clientId().isPresent());
		assertEquals(ONE, role.clientId().get());
		assertFalse("Project ID must not be available", role.projectId().isPresent());
		assertEquals(CLIENT_ADMIN.getValue(), role.value());
	}
	
	/**
	 * Check mining role.
	 */
	@Test
	public void mining() {
		final MiningRole role = new MiningRole("client-1-project-2-mining");
		assertTrue("Client ID must be available", role.clientId().isPresent());
		assertEquals(ONE, role.clientId().get());
		assertTrue("Project ID must be available", role.projectId().isPresent());
		assertEquals(TWO, role.projectId().get());
		assertEquals("mining", role.value());
	}
	
	/**
	 * Check discovery role.
	 */
	@Test
	public void discovery() {
		final MiningRole role = new MiningRole("client-2-project-1-discovery");
		assertTrue("Client ID must be available", role.clientId().isPresent());
		assertEquals(TWO, role.clientId().get());
		assertTrue("Project ID must be available", role.projectId().isPresent());
		assertEquals(ONE, role.projectId().get());
		assertEquals("discovery", role.value());
	}
	
	/**
	 * Check discovery-light role.
	 */
	@Test
	public void discoverLight() {
		final MiningRole role = new MiningRole("client-1-project-1-discovery-light");
		assertTrue("Client ID must be available", role.clientId().isPresent());
		assertEquals(ONE, role.clientId().get());
		assertTrue("Project ID must be available", role.projectId().isPresent());
		assertEquals(ONE, role.projectId().get());
		assertEquals("discovery-light", role.value());
	}

	/**
	 * Check manager role.
	 */
	@Test
	public void manager() {
		final MiningRole role = new MiningRole("client-1-project-1-manager");
		assertTrue("Client ID must be available", role.clientId().isPresent());
		assertEquals(ONE, role.clientId().get());
		assertTrue("Project ID must be available", role.projectId().isPresent());
		assertEquals(ONE, role.projectId().get());
		assertEquals("manager", role.value());
	}
	
	/**
	 * Check editor role.
	 */
	@Test
	public void editor() {
		final MiningRole role = new MiningRole("client-1-project-1-editor");
		assertTrue("Client ID must be available", role.clientId().isPresent());
		assertEquals(ONE, role.clientId().get());
		assertTrue("Project ID must be available", role.projectId().isPresent());
		assertEquals(ONE, role.projectId().get());
		assertEquals("editor", role.value());
	}
	
	/**
	 * Check viewer role.
	 */
	@Test
	public void viewer() {
		final MiningRole role = new MiningRole("client-1-project-1-viewer");
		assertTrue("Client ID must be available", role.clientId().isPresent());
		assertEquals(ONE, role.clientId().get());
		assertTrue("Project ID must be available", role.projectId().isPresent());
		assertEquals(ONE, role.projectId().get());
		assertEquals("viewer", role.value());
	}

	/**
	 * Checks for role value with a lot of segments.
	 * <p>
	 * This might be needed in the future, when new project natures are added.
	 */
	@Test
	public void valueWithALotOfSegments() {
		final MiningRole role = new MiningRole("client-1-project-1-discovery-light-plus-and-some");
		assertTrue("Client ID must be available", role.clientId().isPresent());
		assertEquals(ONE, role.clientId().get());
		assertTrue("Project ID must be available", role.projectId().isPresent());
		assertEquals(ONE, role.projectId().get());
		assertEquals("discovery-light-plus-and-some", role.value());
	}

	/**
	 * Check non numerical IDs lead to an error.
	 */
	@SuppressWarnings("unused")
	@Test(expected = IllegalArgumentException.class)
	public void nonNumericalIds() {
		new MiningRole("client-x-project-y-discovery-light");
	}

	/**
	 * Check non numerical client ID lead to an error.
	 */
	@SuppressWarnings("unused")
	@Test(expected = IllegalArgumentException.class)
	public void nonNumericalClientId() {
		new MiningRole("client-x-project-1-discovery-light");
	}

	/**
	 * Check non numerical project ID lead to an error.
	 */
	@SuppressWarnings("unused")
	@Test(expected = IllegalArgumentException.class)
	public void nonNumericalProjectId() {
		new MiningRole("client-1-project-x-discovery-light");
	}
	
	/**
	 * Check empty segments lead to an error.
	 */
	@SuppressWarnings("unused")
	@Test(expected = IllegalArgumentException.class)
	public void emptySegments() {
		new MiningRole("client-1-project----discovery---light");
	}

}
