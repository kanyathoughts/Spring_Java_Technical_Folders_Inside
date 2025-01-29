/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.taxonomy;

import static org.hamcrest.collection.IsMapWithSize.anEmptyMap;
import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNot.not;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Test;

import innowake.mining.data.core.taxonomy.api.DefaultDependecyModule;
import innowake.mining.data.core.taxonomy.api.DependencyEdge;
import innowake.mining.data.core.taxonomy.api.DependencyModule;
import innowake.mining.shared.model.ReferenceAttributes;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests ensuring the behavior of the default implementation of the {@link DependencyModule} and {@link DependencyEdge}.
 */
public class DependencyModuleTests {

	private static final int FIRST = 0;
	private static final int EXPECTED_INCOMING_EDGE_COUNT = 1;

	@Test
	public void dependencyModuleTestFacilityWorks() {
		final DefaultDependecyModule rootModule = new DefaultDependecyModule(Technology.COBOL, Type.PROGRAM, "MyCOBOL");
		assertEquals(Technology.COBOL, rootModule.getTechnology());
		assertEquals(Type.PROGRAM, rootModule.getType());

		final String jobName = "MYJOB";
		final DependencyModule incomingJcl = new DefaultDependecyModule(Technology.JCL, Type.JOB, jobName);
		assertEquals(Technology.JCL, incomingJcl.getTechnology());
		assertEquals(Type.JOB, incomingJcl.getType());
		assertEquals(jobName, incomingJcl.getName());

		rootModule.addIncoming(incomingJcl, RelationshipType.CALLS);
		
		final List<DependencyEdge> incomingsFromRoot = rootModule.getIncomings();
		assertFalse("Incoming edges list of root Module must not be empty", incomingsFromRoot.isEmpty());
		final List<DependencyEdge> incomingCallsFromRoot = rootModule.getIncomings(RelationshipType.CALLS);
		assertFalse("Incoming 'calls' edges list of root Module must not be empty", incomingCallsFromRoot.isEmpty());
		final List<DependencyEdge> incomingIncludesFromRoot = rootModule.getIncomings(RelationshipType.INCLUDES);
		assertTrue("Incoming 'includes' edges list of root Module must be empty", incomingIncludesFromRoot.isEmpty());
		assertTrue("Outgoing edges list of root Module must be empty", rootModule.getOutgoings().isEmpty());
		
		assertEquals("Incoming edges list of root Module must have 1 entry", EXPECTED_INCOMING_EDGE_COUNT, incomingsFromRoot.size());
		final DependencyEdge incomingEdge = incomingsFromRoot.get(FIRST);
		assertEquals(RelationshipType.CALLS, incomingEdge.getRelationship());
		assertEquals(incomingJcl, incomingEdge.getIncomingModule());
		assertThat(incomingEdge.getProperties(), is(anEmptyMap()));
		
		final DependencyModule outgoingTable = new DefaultDependecyModule(Technology.RESOURCE, Type.TABLE, "MYTABLE");
		final Map<String, Object> properties = new HashMap<>();
		properties.put(ReferenceAttributes.DB_ACCESS_TYPES.getReferenceAttributeExcelName(), "STORE,OTHER");
		properties.put(ReferenceAttributes.DB_ACCESS_OPERATIONS.getReferenceAttributeExcelName(), "DECLARE_TABLE");
		rootModule.addOutgoing(outgoingTable, RelationshipType.ACCESSES, properties);
		
		final List<DependencyEdge> outgoingReadWritesFromRoot = rootModule.getOutgoings(RelationshipType.ACCESSES);
		assertEquals("Outgoing readswrites edges list of root module must have 1 entry", 1, outgoingReadWritesFromRoot.size());
		final DependencyEdge readsWritesEdge = outgoingReadWritesFromRoot.get(FIRST);
		assertThat(readsWritesEdge.getProperties(), not(anEmptyMap()));
		assertEquals(properties, readsWritesEdge.getProperties());
	}
}
