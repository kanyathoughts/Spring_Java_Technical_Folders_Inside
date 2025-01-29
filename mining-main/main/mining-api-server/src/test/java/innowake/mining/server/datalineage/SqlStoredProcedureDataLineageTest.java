/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.datalineage;

import static innowake.mining.shared.entities.dataflow.DataFlowNodeRelationshipType.RELATED_FIELD;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang3.tuple.Triple;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

import innowake.mining.server.datalineage.context.DataLineageContext;
import innowake.mining.server.datalineage.operations.sqlstoredprocedure.SqlStoredProcedureTracer;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.dataflow.DataFlowNodePojo;
import innowake.mining.shared.entities.dataflow.DataFlowNodeRelationshipType;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.datalineage.DataFlowId;

/**
 * Tests for {@link SqlStoredProcedureTracer}
 */
class SqlStoredProcedureDataLineageTest extends BaseDataLineageTest {

	static final String CALLER_FILE = "storedprocedure/CALLPROC.cbl";
	static final String SQL_SCRIPT_FILE = "storedprocedure/CreateProcedure2.sql";
	static final String IMPL_FILE = "storedprocedure/PRGPROC2.cbl";

	@Autowired
	private ObjectMapper objectMapper;

	@BeforeEach
	void beforeEach() throws IOException {
		/* we want to assert that the same result is produced regardless of where the trace is started, so we must reset before each test */
		resetTestData();
		sourceService.resetCaches();
	}

	/**
	 * Tests that stored procedure is linked correctly with caller module and implementation module when trace is started in stored procedure module.
	 */
	@Test
	void testStoredProcedureLinkageFromSP() throws IOException {
		final var testModules = createTestModules();
		final ModulePojo storedProcedureModule = testModules.getMiddle();

		/* execute traceAll() on SP module */
		executeTrace(storedProcedureModule);

		/* link SP module with caller */
		resolveProxyContainer(storedProcedureModule, ProxyContainerPojo.Type.ENTRY_POINT);
		/* link SP module with implementation */
		resolveProxyContainer(storedProcedureModule, ProxyContainerPojo.Type.CALL);

		/* calling traceAll() on the SP module which sits in the middle, establishes all connection across all modules */
		assertCallerModuleResult(testModules);
		assertStoredProcedureModuleResult(testModules);
		assertImplementationModuleResult(testModules);
	}

	/**
	 * Tests that stored procedure is linked correctly with implementation module when trace is started in implementation module.
	 */
	@Test
	void testStoredProcedureLinkageFromImplementation() throws IOException {
		final var testModules = createTestModules();
		final ModulePojo implementationModule = testModules.getRight();

		/* execute traceAll() on implementation module */
		executeTrace(implementationModule);

		/* link implementation module to stored procedure */
		resolveProxyContainer(implementationModule, ProxyContainerPojo.Type.ENTRY_POINT);

		/* calling traceAll() on the implementation module only connects the implementation module with the SP, but not the SP with the caller module */
		assertStoredProcedureModuleResult(testModules);
		assertImplementationModuleResult(testModules);
	}

	/**
	 * Tests that stored procedure is linked correctly with caller module when trace is started in caller module.
	 */
	@Test
	void testStoredProcedureLinkageFromCaller() throws IOException {
		final var testModules = createTestModules();
		final ModulePojo callerModule = testModules.getLeft();

		/* execute traceAll() on caller module */
		executeTrace(callerModule);

		/* link caller module to stored procedure */
		resolveProxyContainer(callerModule, ProxyContainerPojo.Type.CALL);

		/* calling traceAll() on the caller module only connects the caller module with the SP, but not the SP with the implementation */
		assertCallerModuleResult(testModules);
		assertStoredProcedureModuleResult(testModules);
	}

	private Triple<ModulePojo, ModulePojo, ModulePojo> createTestModules() throws IOException {
		/* the Cobol module containing the EXEC SQL CALL to the stored procedure */
		final ModulePojo callerModule = moduleService.getModule(createModule(CALLER_FILE, "CALLPROC", Technology.COBOL, Type.PROGRAM));
		/* the SQL Stored Procedure
		 * - not 100% accurate because discovery creates the stored procedure as a sub-module of the SQL script - should work the same way though */
		final ModulePojo storedProcedureModule = moduleService.getModule(createModule(SQL_SCRIPT_FILE, "SP_SUBSCRIBE", Technology.SQL, Type.STORED_PROCEDURE));
		/* the Cobol module that implements the stored procedure */
		final ModulePojo implementationModule = moduleService.getModule(createModule(IMPL_FILE, "PRGPROC2", Technology.COBOL, Type.PROGRAM));

		makeCallReference(callerModule.identity(), storedProcedureModule.identity(), new ModuleLocation(343, 251));
		makeCallReference(storedProcedureModule.identity(), implementationModule.identity());

		return Triple.of(callerModule, storedProcedureModule, implementationModule);
	}

	private void executeTrace(final ModulePojo module) {
		dataLineageCoreService.traceModule(new DataLineageContext(PROJECT_ID), module.identity());
	}

	private void resolveProxyContainer(final ModulePojo module, final ProxyContainerPojo.Type type) {
		final DataLineageContext context = new DataLineageContext(PROJECT_ID);
		for (final ProxyContainerPojo proxyContainer : dataLineageCoreService.getProxyContainersOfType(context, module.identity(), type)) {
			dataLineageCoreService.resolveProxyContainer(context, EntityId.of(module.getId()), proxyContainer.getDataFlowId());
		}

	}

	private void assertCallerModuleResult(final Triple<ModulePojo, ModulePojo, ModulePojo> testModules) throws JsonProcessingException {
		final ModulePojo callerModule = testModules.getLeft();
		final ModulePojo storedProcedureModule = testModules.getMiddle();

		/* Assertions for Caller Module:
		 * - a CALL container has been created and it has one field for each host variable that was passed to the EXEC SQL CALL (literals / constants are ignored)
		 * - the CALL container has the correct CALL_PARAMETER_INDEX property indicating the position of each field in the parameter list
		 * - each field of the CALL container is linked to the correct field of the ENTRY_POINT of the Stored Procedure
		 */

		final ProxyContainerPojo spEntryPoint = dataFlowService.findAnyProxyContainer(q -> q.ofModule(storedProcedureModule.identity()).withType(ProxyContainerPojo.Type.ENTRY_POINT))
				.stream().findAny().orElseGet(() -> Assertions.fail("ProxyContainerPojo of type ENTRY_POINT on Stored ProcedureModule should have been created"));

		final ProxyContainerPojo callerCall = dataFlowService.findAnyProxyContainer(q -> q.ofModule(callerModule.identity()).withType(ProxyContainerPojo.Type.CALL))
				.stream().findAny().orElseGet(() -> Assertions.fail("ProxyContainerPojo of type CALL on Caller Module should have been created"));

		/* check that the proxy fields on CALL were created */
		assertEquals(
				Arrays.asList("ALPHA-LONG", "ALPHA-SHORT", "NUMBER-FIELD", "OTHER-FIELD"),
				dataFlowService.find(q -> q.byIds(callerCall.getFieldNodesUids())).stream().map(DataFlowNodePojo::getName).sorted().collect(Collectors.toList())
		);

		/* parse and validate CALL_PARAMETER_INDEX */
		final List<Integer> parameterIndexes = objectMapper.readValue((String) callerCall.getProperties().get(ProxyContainerPojo.Property.CALL_PARAMETER_INDEX.name()),
				/* https://bugs.eclipse.org/bugs/show_bug.cgi?id=572534: ClassCastException when diamond operator is used for TypeReference */
				new TypeReference<List<Integer>>() {});

		assertEquals(
				Arrays.asList(0, 1, 3, 5),
				parameterIndexes
		);

		final Map<DataFlowId, DataFlowId> spEntryPointMap = new HashMap<>();
		for (final var spField : spEntryPoint.getFieldNodes()) {
			final var relationships = dataFlowService.find(q -> q.withRelationshipFrom(spField.getId(), RELATED_FIELD));
			if ( ! relationships.isEmpty()) {
				assertEquals(1, relationships.size(), "Field " + spField.getId() + " should be linked to exactly one field of proxy container: " + callerCall.getDataFlowId());
				/* key: field from spEntryPoint, value: field from relationship */
				spEntryPointMap.put(spField.getDataFlowId(), relationships.get(0).getDataFlowId());
			}
		}

		final Map<DataFlowId, DataFlowId> callerMap = new HashMap<>();
		for (final var callerField : callerCall.getFieldNodes()) {
			final var relationships = dataFlowService.find(q -> q.withRelationshipFrom(callerField.getId(), RELATED_FIELD));
			assertEquals(1, relationships.size(), "Field " + callerField.getId() + " should be linked to exactly one field of proxy container: " + spEntryPoint.getDataFlowId());
			/* key: field from relationship, value: field from callerCall */
			callerMap.put(relationships.get(0).getDataFlowId(), callerField.getDataFlowId());
		}

		/* check that CALL fields are linked to correct ENTRY_POINT fields of SP (according to parameter index) */
		assertEquals(spEntryPointMap, callerMap);
	}

	private void assertStoredProcedureModuleResult(final Triple<ModulePojo, ModulePojo, ModulePojo> testModules) {
		final ModulePojo storedProcedureModule = testModules.getMiddle();
		
		/* Assertions for Stored Procedure Module:
		 * - an ENTRY_POINT container was created and it has a field for each parameter of the stored procedure in the correct order
		 * - a CALL container was created (because the procedure is using the EXTERNAL keyword) and it has the same fields in the same order as the ENTRY_POINT
		 * - each field of the ENTRY_POINT is linked to the corresponding field on the CALL
		 */

		final ProxyContainerPojo spEntryPoint = dataFlowService.findProxyContainers(q -> q.ofModule(storedProcedureModule.identity()).withType(ProxyContainerPojo.Type.ENTRY_POINT))
				.stream().findAny().orElseGet(() -> Assertions.fail("ProxyContainerPojo of type ENTRY_POINT on Stored ProcedureModule should have been created"));

		/* check that the proxy fields on ENTRY_POINT were created */
		assertEquals(
				Set.of("RETURN_VALUE", "PUBLICATION", "ARTICLE", "ISSUE", "COUNT", "OTHER"),
				dataFlowService.find(q -> q.byIds(spEntryPoint.getFieldNodesUids())).stream().map(DataFlowNodePojo::getName).collect(Collectors.toSet())
		);

		/* since the Stored Procedure is using EXTERNAL - it should CALL the implementation module, so a CALL container must be created */
		final ProxyContainerPojo spCall = dataFlowService.findProxyContainers(q -> q.ofModule(storedProcedureModule.identity()).withType(ProxyContainerPojo.Type.CALL))
				.stream().findAny().orElseGet(() -> Assertions.fail("ProxyContainerPojo of type CALL on Stored Procedure Module should have been created"));

		/* check that the proxy fields on CALL were created */
		assertEquals(
				Set.of("RETURN_VALUE", "PUBLICATION", "ARTICLE", "ISSUE", "COUNT", "OTHER"),
				dataFlowService.find(q -> q.byIds(spCall.getFieldNodesUids())).stream().map(DataFlowNodePojo::getName).collect(Collectors.toSet())
		);

		/* each field on the ENTRY_POINT is linked to CALL according to whether it is an IN, OUT or INOUT parameter */
		/* ENTRY_POINT is OUT parameter and therefore is only "written" by the fake statement */
		final var field0 = spEntryPoint.getFieldNodesUids().get(0);
		final var field0Write = dataFlowService.find(q -> q.withRelationshipFrom(field0, DataFlowNodeRelationshipType.WRITE_ACCESS)).get(0);
		final var field0ReadName = dataFlowService.find(q -> q.withRelationshipFrom(field0Write.getId(), DataFlowNodeRelationshipType.READ_ACCESS)).get(0).getName();
		assertEquals("RETURN_VALUE", field0ReadName);
		assertTrue(dataFlowService.find(q -> q.withRelationshipFrom(field0, DataFlowNodeRelationshipType.READ_ACCESS)).isEmpty(), "RETURN_VALUE should have no read access");
		/* all other parameters are IN parameters and therefore "read" by the fake statement */
		final var field1 = spEntryPoint.getFieldNodesUids().get(1);
		final var field1Read = dataFlowService.find(q -> q.withRelationshipFrom(field1, DataFlowNodeRelationshipType.READ_ACCESS)).get(0);
		final var field1WriteName = dataFlowService.find(q -> q.withRelationshipFrom(field1Read.getId(), DataFlowNodeRelationshipType.WRITE_ACCESS)).get(0).getName();
		assertEquals("PUBLICATION", field1WriteName);
		assertTrue(dataFlowService.find(q -> q.withRelationshipFrom(field1, DataFlowNodeRelationshipType.WRITE_ACCESS)).isEmpty(), "PUBLICATION should have no write access");
		final var field2 = spEntryPoint.getFieldNodesUids().get(2);
		final var field2Read = dataFlowService.find(q -> q.withRelationshipFrom(field2, DataFlowNodeRelationshipType.READ_ACCESS)).get(0);
		final var field2WriteName = dataFlowService.find(q -> q.withRelationshipFrom(field2Read.getId(), DataFlowNodeRelationshipType.WRITE_ACCESS)).get(0).getName();
		assertEquals("ARTICLE", field2WriteName);
		assertTrue(dataFlowService.find(q -> q.withRelationshipFrom(field2, DataFlowNodeRelationshipType.WRITE_ACCESS)).isEmpty(), "ARTICLE should have no write access");
		final var field3 = spEntryPoint.getFieldNodesUids().get(3);
		final var field3Read = dataFlowService.find(q -> q.withRelationshipFrom(field3, DataFlowNodeRelationshipType.READ_ACCESS)).get(0);
		final var field3WriteName = dataFlowService.find(q -> q.withRelationshipFrom(field3Read.getId(), DataFlowNodeRelationshipType.WRITE_ACCESS)).get(0).getName();
		assertEquals("ISSUE", field3WriteName);
		assertTrue(dataFlowService.find(q -> q.withRelationshipFrom(field3, DataFlowNodeRelationshipType.WRITE_ACCESS)).isEmpty(), "ISSUE should have no write access");
		final var field4 = spEntryPoint.getFieldNodesUids().get(4);
		final var field4Read = dataFlowService.find(q -> q.withRelationshipFrom(field4, DataFlowNodeRelationshipType.READ_ACCESS)).get(0);
		final var field4WriteName = dataFlowService.find(q -> q.withRelationshipFrom(field4Read.getId(), DataFlowNodeRelationshipType.WRITE_ACCESS)).get(0).getName();
		assertEquals("COUNT", field4WriteName);
		assertTrue(dataFlowService.find(q -> q.withRelationshipFrom(field4, DataFlowNodeRelationshipType.WRITE_ACCESS)).isEmpty(), "COUNT should have no write access");
		assertTrue(dataFlowService.find(q -> q.withRelationshipFrom(field3, DataFlowNodeRelationshipType.WRITE_ACCESS)).isEmpty(), "ISSUE should have no write access");
		final var field5 = spEntryPoint.getFieldNodesUids().get(5);
		final var field5Read = dataFlowService.find(q -> q.withRelationshipFrom(field5, DataFlowNodeRelationshipType.READ_ACCESS)).get(0);
		final var field5WriteName = dataFlowService.find(q -> q.withRelationshipFrom(field5Read.getId(), DataFlowNodeRelationshipType.WRITE_ACCESS)).get(0).getName();
		assertEquals("OTHER", field5WriteName);
		assertTrue(dataFlowService.find(q -> q.withRelationshipFrom(field5, DataFlowNodeRelationshipType.WRITE_ACCESS)).isEmpty(), "OTHER should have no write access");
	}

	private void assertImplementationModuleResult(final Triple<ModulePojo, ModulePojo, ModulePojo> testModules) {
		final ModulePojo storedProcedureModule = testModules.getMiddle();
		final ModulePojo implementationModule = testModules.getRight();

		/* Assertions for Implementation Module:
		 * - an ENTRY_POINT container has been created and it has a field for each parameter in the PROCEDURE DIVISION USING clause of the program,
		 *   in the correct order
		 * - each field of the ENTRY_POINT is linked to the correct field of the CALL of the Stored Procedure module
		 */

		final ProxyContainerPojo implEntryPoint = dataFlowService.findProxyContainers(q -> q.ofModule(implementationModule.identity()).withType(ProxyContainerPojo.Type.ENTRY_POINT))
				.stream().findAny().orElseGet(() -> Assertions.fail("ProxyContainerPojo of type ENTRY_POINT on Implementation Module should have been created"));

		final ProxyContainerPojo spCall = dataFlowService.findProxyContainers(q -> q.ofModule(storedProcedureModule.identity()).withType(ProxyContainerPojo.Type.CALL))
				.stream().findAny().orElseGet(() -> Assertions.fail("ProxyContainerPojo of type CALL on Stored Procedure Module should have been created"));

		/* check that the proxy fields on ENTRY_POINT were created */
		assertEquals(
				Set.of("RESULT", "PUBLICATION", "ARTICLE", "ISSUE", "COUNTP", "OTHERP"),
				dataFlowService.find(q -> q.byIds(implEntryPoint.getFieldNodesUids())).stream().map(DataFlowNodePojo::getName).collect(Collectors.toSet())
		);

		final Map<DataFlowId, Set<DataFlowId>> spCallMap = new HashMap<>();
		for (final var spField : spCall.getFieldNodes()) {
			spCallMap.put(spField.getDataFlowId(), dataFlowService.find(q -> q.withRelationshipFrom(spField.getId(), RELATED_FIELD)).stream()
																	.map(DataFlowNodePojo::getDataFlowId)
																	.collect(Collectors.toSet()));
		}

		/* check that ENTRY_POINT is linked with CALL from Stored Procedure */
		for (final var implEntryField : implEntryPoint.getFieldNodes()) {
			final var fields = dataFlowService.find(q -> q.withRelationshipFrom(implEntryField.getId(), RELATED_FIELD));
			assertFalse(fields.isEmpty());
			boolean matchFound = false;
			for (final var field : fields) {
				final var set = spCallMap.get(field.getDataFlowId());
				if (set != null && set.contains(implEntryField.getDataFlowId())) {
					matchFound = true;
					break;
				}
			}
			assertTrue(matchFound);
		}
	}
}
