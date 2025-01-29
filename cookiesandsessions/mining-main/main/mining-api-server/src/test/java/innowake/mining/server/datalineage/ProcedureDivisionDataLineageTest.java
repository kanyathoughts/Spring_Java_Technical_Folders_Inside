package innowake.mining.server.datalineage;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import innowake.mining.server.datalineage.context.DataLineageContext;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.dataflow.DataFlowNodePojo;
import innowake.mining.shared.entities.dataflow.DataFlowNodeRelationshipType;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

class ProcedureDivisionDataLineageTest extends BaseDataLineageTest{
	
	static final String CALLER_FILE = "proceduredivision/CALLER.cbl";
	static final String CALLE_FILE = "proceduredivision/CALLEE.cbl";
	

	@BeforeEach
	void beforeEach() throws IOException {
		/* we want to assert that the same result is produced regardless of where the trace is started, so we must reset before each test */
		resetTestData();
		sourceService.resetCaches();
	}
	
	private void executeTrace(final ModulePojo module) {
		dataLineageCoreService.traceModule(new DataLineageContext(PROJECT_ID), module.identity());
	}

	private void resolveProxyContainer(final ModulePojo module, final ProxyContainerPojo.Type type) {
		final DataLineageContext context = new DataLineageContext(PROJECT_ID);
		for (final ProxyContainerPojo proxyContainer : dataLineageCoreService.getProxyContainersOfType(context, module.identity(), type)) {
			dataLineageCoreService.resolveProxyContainer(context, module.identity(), proxyContainer.getDataFlowId());
		}

	}

	private void executeDiscoverProxyContainers(final ModulePojo module, final ProxyContainerPojo.Type type) {
		dataLineageCoreService.getProxyContainersOfType(new DataLineageContext(PROJECT_ID), module.identity(), type);
	}
	
	
	private Pair<ModulePojo, ModulePojo> createTestModules() throws IOException {
		/* the Cobol module containing the EXEC SQL CALL to the stored procedure */
		final ModulePojo callerModule = moduleService.getModule(createModule(CALLER_FILE, "CALLER", Technology.COBOL, Type.PROGRAM));
		/* the Cobol module that implements the stored procedure */
		final ModulePojo calleeModule = moduleService.getModule(createModule(CALLE_FILE, "CALLEE", Technology.COBOL, Type.PROGRAM));

		makeCallReference(callerModule.identity(), calleeModule.identity(), new ModuleLocation(189, 26));

		return Pair.of(callerModule, calleeModule);
	}
	
	private void assertCallerModuleResult(final Pair<ModulePojo, ModulePojo> testModules, final boolean assertCaller, final boolean assertCallee) {
		final ModulePojo callerModule = testModules.getLeft();
		final ModulePojo calleeModule = testModules.getRight();

		ProxyContainerPojo callerContainer = null;
		if (assertCaller) {
			callerContainer = dataFlowService.findAnyProxyContainer(q -> q.ofModule(callerModule.identity()).withType(ProxyContainerPojo.Type.CALL))
					.orElseGet(() -> Assertions.fail("ProxyContainer of type CALL on Caller Module should have been created"));

			/* check that the proxy fields on CALL were created */
			assertEquals(
					List.of("DUMMY"),
					callerContainer.getFieldNodes().stream().map(DataFlowNodePojo::getName).collect(Collectors.toList())
			);
		}

		ProxyContainerPojo calleeContainer = null;
		if (assertCallee) {
			calleeContainer = dataFlowService.findAnyProxyContainer(q -> q.ofModule(calleeModule.identity()).withType(ProxyContainerPojo.Type.ENTRY_POINT))
					.orElseGet(() -> Assertions.fail("ProxyContainer of type ENTRY_POINT on callee module should have been created"));

			/* check that the proxy fields on ENTRY_POINT were created */
			assertEquals(
					List.of("G1"),
					calleeContainer.getFieldNodes().stream().map(DataFlowNodePojo::getName).collect(Collectors.toList())
			);
		}

		if (assertCaller && assertCallee) {
			/* check that caller field is linked to callee field */
			assertNotNull(callerContainer);
			assertNotNull(calleeContainer);
			final ProxyContainerPojo cc = callerContainer;
			final var calleeField = calleeContainer.getFieldNodesUids().get(0);
			final var relatedNodes = dataFlowService.find(q -> q.withRelationshipFrom(cc.getFieldNodesUids().get(0), DataFlowNodeRelationshipType.RELATED_FIELD));
			assertTrue(relatedNodes.stream().map(DataFlowNodePojo::getId).anyMatch(m -> m.equals(calleeField)),
					"Callee proxy field should have caller proxy field as related field");
		}
	}
	
	/**
	 * Tests that stored procedure is linked correctly with caller module when trace is started on a specific field (ALPHA-LONG)
	 * in the caller module.
	 */
	@Test
	void testTraceAll() throws IOException {
		final var testModules = createTestModules();
		final ModulePojo callerModule = testModules.getLeft();

		/* execute traceAll() in caller module */
		executeTrace(callerModule);

		/* connect CALL with ENTRY_POINT */
		resolveProxyContainer(callerModule, ProxyContainerPojo.Type.CALL);

		/* calling traceAll() inside the caller module should connect it with the ENTRY_POINT of the callee */
		assertCallerModuleResult(testModules, true, true);
	}
	
	@Test
	void testTraceField() throws IOException {
		final var testModules = createTestModules();
		final ModulePojo callerModule = testModules.getLeft();

		/* execute traceField() on DUMMY in caller module */
		executeTrace(callerModule);

		/* calling traceField() inside the caller module only creates the CALL container but does not connect it with the caller */
		assertCallerModuleResult(testModules,true, false);
	}
	
	@Test
	void testDiscoverCall() throws IOException {
		final var testModules = createTestModules();
		final ModulePojo callerModule = testModules.getLeft();

		/* execute discoverProxyContainersOfType() on caller module */
		executeDiscoverProxyContainers(callerModule, ProxyContainerPojo.Type.ENTRY_POINT);

		/* calling discoverProxyContainersOfType() on the caller module only creates the CALL proxy container and fields but does not connect them */
		assertCallerModuleResult(testModules, true, false);
	}

	@Test
	void testDiscoverEntryPoint() throws IOException {
		final var testModules = createTestModules();
		final ModulePojo calleeModule = testModules.getRight();

		/* execute discoverProxyContainersOfType() on callee module */
		executeDiscoverProxyContainers(calleeModule, ProxyContainerPojo.Type.ENTRY_POINT);

		/* calling discoverProxyContainersOfType() on the caller module only creates the CALL proxy container and fields but does not connect them */
		assertCallerModuleResult(testModules, false, true);
	}
}

