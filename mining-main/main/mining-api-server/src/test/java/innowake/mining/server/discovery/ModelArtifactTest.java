/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.Test;

import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.model.discovery.ResolveTarget;


/**
 * Tests equality and hashCode of ModelArtifacts.
 */
public class ModelArtifactTest {
	
	/**
	 * Test a simple equality.
	 */
	@Test
	public void testSimpleEqualityWithNull() {
		final ModelArtifact first = new ModelArtifact().setName("First").setType(ResolveTarget.UNKNOWN).setRepresentation(Representation.VIRTUAL).setPath(null);
		final ModelArtifact second = new ModelArtifact().setName("First").setType(ResolveTarget.UNKNOWN).setRepresentation(Representation.VIRTUAL).setPath(null);
		assertEquals(first, second);
	}
	
	/**
	 * Test the hash code implementation.
	 */
	@Test
	public void testSimpleHashCode() {
		final ModelArtifact first = new ModelArtifact().setName("First").setType(ResolveTarget.UNKNOWN).setRepresentation(Representation.VIRTUAL).setPath(null);
		final ModelArtifact second = new ModelArtifact().setName("First").setType(ResolveTarget.UNKNOWN).setRepresentation(Representation.VIRTUAL).setPath(null);
		assertEquals(first.hashCode(), second.hashCode());
	}
	
	/**
	 * Test all equal but differently cased names.
	 */
	@Test
	public void testDifferentCaseArtifactnames() {
		final ModelArtifact first = new ModelArtifact().setName("First").setType(ResolveTarget.UNKNOWN).setRepresentation(Representation.VIRTUAL).setPath(null);
		final ModelArtifact second = new ModelArtifact().setName("fiRst").setType(ResolveTarget.UNKNOWN).setRepresentation(Representation.VIRTUAL).setPath(null);
		assertEquals(first, second);
		assertEquals(first.hashCode(), second.hashCode());
	}
	
	/**
	 * Test artifacts with same names but different paths.
	 */
	@Test
	public void testPhysicalAritfacts() {
		final ModelArtifact first = new ModelArtifact().setName("First").setType(ResolveTarget.BASIC_OBJECT).setRepresentation(Representation.PHYSICAL).setPath("/this.bas");
		final ModelArtifact second = new ModelArtifact().setName("First").setType(ResolveTarget.BASIC_OBJECT).setRepresentation(Representation.PHYSICAL).setPath("/THIS.bas");
		
		assertNotEquals(first, second);
		
		second.setPath("/this.bas");
		
		assertEquals(first, second);
		assertEquals(first.hashCode(), second.hashCode());
	}
	
	/**
	 * Test different artifact names and different paths.
	 */
	@Test
	public void testPhysicalAritfacts2() {
		final ModelArtifact first = new ModelArtifact().setName("First").setType(ResolveTarget.BASIC_OBJECT).setRepresentation(Representation.PHYSICAL).setPath("/this.bas");
		final ModelArtifact second = new ModelArtifact().setName("First").setType(ResolveTarget.BASIC_OBJECT).setRepresentation(Representation.PHYSICAL).setPath("/THIS.bas");
		
		assertNotEquals(first, second);
	}
	
	@Test
	public void testVirtualArtifacts() {
		final ModelArtifact first = new ModelArtifact().setName("First").setType(ResolveTarget.UNKNOWN);
		assertNotNull(first.getName());
		assertNotNull(first.getPath());
		assertNotNull(first.getType());
		assertNotNull(first.getRepresentation());
		assertEquals(Representation.VIRTUAL, first.getRepresentation());
	}
}
