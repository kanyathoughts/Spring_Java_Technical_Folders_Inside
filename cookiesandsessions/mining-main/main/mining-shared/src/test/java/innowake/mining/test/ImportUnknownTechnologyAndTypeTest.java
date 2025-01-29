/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.Test;

import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests for mapping Unknown Languages and Types from Discovery file import.
 * The unknown languages and types are mapped to {@link Technology#UNKNOWN} and {@link Type#UNKNOWN} respectively.
 */
public class ImportUnknownTechnologyAndTypeTest {
	
	/** UNKNOWN_VALUE is a literal representing unknown technology or type from Discovery File.  */
	private static final String UNKNOWN_VALUE = "UnknownValue";
	
	/**
	 * Tests the mapping of unknown value using {@link Technology#fromName(String)} returns {@link Technology#UNKNOWN}.
	 */
	@Test
	public void testUnknownTechnology() {
		assertEquals(Technology.UNKNOWN, Technology.fromName(UNKNOWN_VALUE));
	}
	
	/**
	 * Tests the mapping of unknown value using {@link Type#fromName(String)} returns {@link Type#UNKNOWN}.
	 */
	@Test
	public void testUnknownType() {
		assertEquals(Type.UNKNOWN, Type.fromName(UNKNOWN_VALUE));
	}
	
	/**
	 * Tests the {@link Storage} for {@link Technology#UNKNOWN} returns {@link Storage#UNDEFINED}.
	 */
	@Test
	public void testStorageForUnknownTechnology() {
		assertEquals(Storage.UNDEFINED, Storage.from(Technology.UNKNOWN, Type.ADAPTER));
	}
	
	/**
	 * Tests the {@link Storage} for {@link Type#UNKNOWN} returns {@link Storage#UNDEFINED}.
	 */
	@Test
	public void testStorageForUnknownType() {
		assertEquals(Storage.UNDEFINED, Storage.from(Technology.ASSEMBLER, Type.UNKNOWN));
	}
	
	/**
	 * Tests the ModuleReferenceType for {@link Type#UNKNOWN} returns ModuleReferenceType#REFERENCES.
	 */
	@Test
	public void testModuleReferenceTypeForUnknownType() {
		assertEquals(RelationshipType.REFERENCES, RelationshipType.from(Technology.ASSEMBLER, Type.UNKNOWN));
	}
}
