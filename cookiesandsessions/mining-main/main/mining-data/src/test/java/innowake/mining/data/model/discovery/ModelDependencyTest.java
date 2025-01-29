/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.model.discovery;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.Test;

import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.shared.model.DatabaseAccessType;

/**
 * Test class for testing {@link ModelDependency ModelDependencies}
 */
class ModelDependencyTest {

	/**
	 * Tests that the modified state of {@link ModelDependency} is {@code true} when a new attribute collection is added.
	 */
	@Test
	void testAddNewAttributeCollection() {
		final List<DatabaseAccessType> dbAccessTypes1 = new ArrayList<>();
		dbAccessTypes1.add(DatabaseAccessType.READ);
		dbAccessTypes1.add(DatabaseAccessType.DELETE);

		final ModelDependency dependency = new ModelDependency();
		dependency.setModified(false);
		assertFalse(dependency.isModified());

		dependency.addAttribute(ModelAttributeKey.DB_ACCESS_TYPE, dbAccessTypes1);
		assertTrue(dependency.isModified());
	}

	/**
	 * Tests that the modified state of {@link ModelDependency} is {@code true} when an attribute collection of different size is set.
	 */
	@Test
	void testAddAttributeCollectionWithDifferentSize() {
		final List<DatabaseAccessType> dbAccessTypes1 = new ArrayList<>();
		dbAccessTypes1.add(DatabaseAccessType.READ);
		dbAccessTypes1.add(DatabaseAccessType.DELETE);

		final ModelDependency dependency = new ModelDependency();
		dependency.addAttribute(ModelAttributeKey.DB_ACCESS_TYPE, dbAccessTypes1);
		dependency.setModified(false);
		assertFalse(dependency.isModified());

		final List<String> dbAccessTypes2 = Arrays.asList("READ");
		dependency.addAttribute(ModelAttributeKey.DB_ACCESS_TYPE, dbAccessTypes2);
		assertTrue(dependency.isModified());
		dependency.setModified(false);
		assertFalse(dependency.isModified());

		final List<String> dbAccessTypes3 = Arrays.asList("READ", "WRITE");
		dependency.addAttribute(ModelAttributeKey.DB_ACCESS_TYPE, dbAccessTypes3);
		assertTrue(dependency.isModified());
	}

	/**
	 * Tests that the modified state of {@link ModelDependency} remains {@code false} when the same attribute collection but with 
	 * a different order is set. Also test that the modified state changes to {@code true} when one element in the new collection
	 * differs.
	 */
	@Test
	void testAddAttributeCollectionSimple() {
		final List<DatabaseAccessType> dbAccessTypes1 = new ArrayList<>();
		dbAccessTypes1.add(DatabaseAccessType.READ);
		dbAccessTypes1.add(DatabaseAccessType.DELETE);
		dbAccessTypes1.add(DatabaseAccessType.STORE);

		final ModelDependency dependency = new ModelDependency();
		dependency.addAttribute(ModelAttributeKey.DB_ACCESS_TYPE, dbAccessTypes1);
		assertTrue(dependency.isModified());
		dependency.setModified(false);
		assertFalse(dependency.isModified());
		
		final Set<String> dbAccessTypes2 = new HashSet<>();
		dbAccessTypes2.add("STORE");
		dbAccessTypes2.add("READ");
		dbAccessTypes2.add("DELETE");
		dependency.addAttribute(ModelAttributeKey.DB_ACCESS_TYPE, dbAccessTypes2);
		assertFalse(dependency.isModified());

		final Set<String> dbAccessTypes3 = new HashSet<>();
		dbAccessTypes3.add("READ");
		dbAccessTypes3.add("WRITE");
		dbAccessTypes3.add("STORE");
		dependency.addAttribute(ModelAttributeKey.DB_ACCESS_TYPE, dbAccessTypes3);
		assertTrue(dependency.isModified());
	}

	/**
	 * Tests that the modified state of {@link ModelDependency} is {@code true} when repeating elements in the old or new collection
	 * are used.
	 */
	@Test
	void testAddAttributeCollectionRepeatingElements() {
		final List<DatabaseAccessType> dbAccessTypes11 = new ArrayList<>();
		dbAccessTypes11.add(DatabaseAccessType.READ);
		dbAccessTypes11.add(DatabaseAccessType.DELETE);

		final ModelDependency dependency1 = new ModelDependency();
		dependency1.addAttribute(ModelAttributeKey.DB_ACCESS_TYPE, dbAccessTypes11);
		assertTrue(dependency1.isModified());
		dependency1.setModified(false);
		assertFalse(dependency1.isModified());
		
		final Set<String> dbAccessTypes12 = new HashSet<>();
		dbAccessTypes12.add("READ");
		dbAccessTypes12.add("READ");
		dependency1.addAttribute(ModelAttributeKey.DB_ACCESS_TYPE, dbAccessTypes12);
		assertTrue(dependency1.isModified());

		
		final List<DatabaseAccessType> dbAccessTypes21 = new ArrayList<>();
		dbAccessTypes21.add(DatabaseAccessType.READ);
		dbAccessTypes21.add(DatabaseAccessType.READ);

		final ModelDependency dependency21 = new ModelDependency();
		dependency21.addAttribute(ModelAttributeKey.DB_ACCESS_TYPE, dbAccessTypes21);
		assertTrue(dependency21.isModified());
		dependency21.setModified(false);
		assertFalse(dependency21.isModified());
		
		final Set<String> dbAccessTypes22 = new HashSet<>();
		dbAccessTypes22.add("DELETE");
		dbAccessTypes22.add("READ");
		dependency21.addAttribute(ModelAttributeKey.DB_ACCESS_TYPE, dbAccessTypes22);
		assertTrue(dependency21.isModified());
	}
}
