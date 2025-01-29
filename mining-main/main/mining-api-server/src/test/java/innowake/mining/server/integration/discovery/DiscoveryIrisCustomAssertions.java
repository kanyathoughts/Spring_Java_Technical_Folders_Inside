/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.discovery;

import com.google.common.collect.Maps;
import innowake.lib.job.api.NullProgressMonitor;
import innowake.mining.extensions.export.callchain.CallChainService;
import innowake.mining.extensions.export.callchain.Parameters;
import innowake.mining.extensions.export.callchain.model.CallChain;
import innowake.mining.extensions.export.callchain.model.CallChainGraph;
import innowake.mining.server.discovery.DiscoveryExcelUtil;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest.AssertionValues;
import innowake.mining.server.service.UtilityAggregationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FieldInfoService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ErrorMarkerPojo;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.io.WorkbookDefinition;
import innowake.mining.shared.model.ModuleFieldName;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.aggregations.AggregationOperator;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;
import innowake.mining.tags.DiscoveryTest;
import org.apache.commons.lang.StringUtils;
import org.junit.Assert;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
/**
 * Supporting class for {@link DiscoveryIrisTest} by including custom assertions.
 *
 * A custom assertion can be included by adding a new method that returns {@link Entry} with parameterized types: {@link String} as key and {@link BiConsumer}
 * as value. This {@link BiConsumer} takes the parameterized types: {@link Long} and AssertionValues
 */
@Component
@DiscoveryTest
class DiscoveryIrisCustomAssertions {

	@Autowired
	private ModuleService moduleService;

	@Autowired
	private FieldInfoService dataSchemaService;
	
	@Autowired
	private UtilityAggregationService utilityAggregationService;

	@Autowired
	private CallChainService callChainService;

	public static final String ERROR_MSG = "Multiple possible candidates found";

	Entry<String, BiConsumer<EntityId, AssertionValues>> testWMIN2279() {
		return Maps.immutableEntry("WMIN2279", (projectId, assertionValues) ->
		DiscoveryExcelUtil.compareIgnoreOrder(assertionValues.expectedMetricsContent, assertionValues.actualMetricsContent, Set.of("Uid", "Target Uid", "Cause",
				"Parent Uid", "Reached From Uids"), Collections.emptyMap())
		);
	}

	Entry<String, BiConsumer<EntityId, AssertionValues>> testWMIN5639() {
		return Maps.immutableEntry("WMIN5639", (projectId, assertionValues) -> {
			DiscoveryExcelUtil.compareIgnoreOrder(assertionValues.expectedMetricsContent, assertionValues.actualMetricsContent, Set.of("Uid", "Target Uid", "Cause",
					"Parent Uid", "Reached From Uids"), Collections.emptyMap());
			final List<Long> moduleUidList = getModuleUIDs(DiscoveryExcelUtil.getValuesFromCsv(assertionValues.actualMetricsContent));
			moduleService.findErrorMarkers(q -> q.ofProject(projectId)).forEach(error -> validateErrorUIDs(error, moduleUidList));
		});
	}

	Entry<String, BiConsumer<EntityId, AssertionValues>> testWMIN8248C() {
		return Maps.immutableEntry("WMIN8248C", (projectId, assertionValues) -> {
			DiscoveryExcelUtil.compareIgnoreOrder(assertionValues.expectedMetricsContent, assertionValues.actualMetricsContent, Set.of("Uid", "Target Uid", "Cause",
					"Parent Uid", "Reached From Uids"), Collections.emptyMap());
		});
	}

	Entry<String, BiConsumer<EntityId, AssertionValues>> testWMIN5453() {
		return Maps.immutableEntry("WMIN5453", (projectId, assertionValues) -> {
			DiscoveryExcelUtil.compareIgnoreOrder(assertionValues.expectedMetricsContent, assertionValues.actualMetricsContent, Set.of("Uid", "Target Uid", "Cause",
					"Parent Uid", "Reached From Uids"), Collections.emptyMap());
			final List<ModulePojo> modules = moduleService.findModules(builder -> builder.ofProject(projectId));
			/* Get module details based on individual job steps to verify which step in the job calls which Natural program */
			final Optional<ModulePojo> stepAModule = modules.stream().filter(module -> "TESTJOB.STEP01.EXEC_PGM".equals(module.getName())).findAny();
			final Optional<ModulePojo> stepBModule = modules.stream().filter(module -> "TESTJOB.STEP02.EXEC_PGM".equals(module.getName())).findAny();

			final List<ModuleRelationshipPojo> stepACallReferenceList = moduleService.findRelationship(q -> q.ofProject(projectId)
					.ofSource(stepAModule.get().identity()));
			final List<ModuleRelationshipPojo> stepBCallReferenceList = moduleService.findRelationship(q -> q.ofProject(projectId)
					.ofSource(stepBModule.get().identity()));

			assertTrue("MAIN6".equalsIgnoreCase(moduleService.getModuleLightweight(EntityId.of(stepACallReferenceList.get(0).getDstModule())).getName()));
			assertTrue("MAIN6".equalsIgnoreCase(moduleService.getModuleLightweight(EntityId.of(stepBCallReferenceList.get(0).getDstModule())).getName()));

			/* Assert the path of modules based on the outgoing call reference created for each natural program to verify if the job steps refers the correct
			 * natural program.
			 */
			assertEquals("src/natural/WMIN5453/A/programs/MAIN6.nsp",
					modules.stream().filter(module -> stepACallReferenceList.get(0).getDstModule().equals(module.getUid())).findFirst().get().getPath().get());
			assertEquals("src/natural/WMIN5453/B/programs/MAIN6.nsp",
					modules.stream().filter(module -> stepBCallReferenceList.get(0).getDstModule().equals(module.getUid())).findFirst().get().getPath().get());
		});
	}
	
	Entry<String, BiConsumer<EntityId, AssertionValues>> testWMIN12520() {
		return Maps.immutableEntry("WMIN12520", (projectId, assertionValues) -> {
			DiscoveryExcelUtil.compareIgnoreOrder(assertionValues.expectedMetricsContent, assertionValues.actualMetricsContent, Set.of("Uid", "Target Uid", "Cause",
					"Parent Uid", "Reached From Uids"), Collections.emptyMap());
			final List<ModulePojo> modules = moduleService.findModules(builder -> builder.ofProject(projectId));
			assertFalse("Modules should be present", modules.isEmpty());
			assertFalse("Requires review should be false by default.", modules.iterator().next().isRequiresReview());
		});
	}
	
	@SuppressWarnings({ "unchecked", "cast", "rawtypes" })
	Entry<String, BiConsumer<EntityId, AssertionValues>> testWMIN8614A() {
		return Maps.immutableEntry("WMIN8614A", (projectId, assertionValues) -> {
			DiscoveryExcelUtil.compareIgnoreOrder(assertionValues.expectedMetricsContent, assertionValues.actualMetricsContent, Set.of("Uid", "Target Uid", "Cause",
					"Parent Uid", "Reached From Uids"), Collections.emptyMap());
			final List<ModulePojo> moduleQRSTCASE = moduleService.findModules(q -> q.ofProject(projectId).withName("QRSTCASE"));
			assertEquals(1, moduleQRSTCASE.size());
			final List<ModulePojo> moduleABC = moduleService.findModules(q -> q.ofProject(projectId).withName("ABC"));
			assertEquals(1, moduleABC.size());
			final var moduleQRSTCASEFieldInfoList = dataSchemaService.find(q -> q.ofProject(projectId).ofModule(moduleQRSTCASE.get(0).identity()));
			final var moduleABCFieldInfoList = dataSchemaService.find(q -> q.ofProject(projectId).ofModule(moduleABC.get(0).identity()));
			assertEquals(1, moduleQRSTCASEFieldInfoList.size());
			assertEquals(3, moduleABCFieldInfoList.size());
			final Map<String, String> moduleQRSTCASEFieldProperties = (Map<String, String>) (Map) moduleQRSTCASEFieldInfoList.get(0).getProperties()
															.orElseThrow(() -> new IllegalStateException("FieldInfo properties must not be null"));
			assertEquals(5, moduleQRSTCASEFieldProperties.size());
			assertTrue("CASE".equalsIgnoreCase(moduleQRSTCASEFieldProperties.get("NAME")));
			assertTrue("6".equalsIgnoreCase(moduleQRSTCASEFieldProperties.get("BYTES")));
			assertTrue("1".equalsIgnoreCase(moduleQRSTCASEFieldProperties.get("START")));
			assertTrue("P".equalsIgnoreCase(moduleQRSTCASEFieldProperties.get("TYPE")));
			assertTrue("true".equalsIgnoreCase(moduleQRSTCASEFieldProperties.get("SEQ")));
			final Map<String, String> moduleABCFieldProperties = (Map<String, String>) (Map) moduleABCFieldInfoList.get(0).getProperties()
															.orElseThrow(() -> new IllegalStateException("FieldInfo properties must not be null"));
			assertEquals(5, moduleABCFieldProperties.size());
			assertTrue("AFIELDA".equalsIgnoreCase(moduleABCFieldProperties.get("NAME")));
			assertTrue("10".equalsIgnoreCase(moduleABCFieldProperties.get("BYTES")));
			assertTrue("1".equalsIgnoreCase(moduleABCFieldProperties.get("START")));
			assertTrue("C".equalsIgnoreCase(moduleABCFieldProperties.get("TYPE")));
			assertTrue("true".equalsIgnoreCase(moduleABCFieldProperties.get("SEQ")));
			final Map<String, String> moduleFieldProperties = (Map<String, String>) (Map) moduleABCFieldInfoList.get(1).getProperties()
															.orElseThrow(() -> new IllegalStateException("FieldInfo properties must not be null"));
			assertEquals(4, moduleFieldProperties.size());
			assertTrue("AFIELDB".equalsIgnoreCase(moduleFieldProperties.get("NAME")));
			assertTrue("10".equalsIgnoreCase(moduleFieldProperties.get("BYTES")));
			assertTrue("11".equalsIgnoreCase(moduleFieldProperties.get("START")));
			assertTrue("C".equalsIgnoreCase(moduleFieldProperties.get("TYPE")));
			assertNull(moduleFieldProperties.get("SEQ"));
		});
	}
	
	Entry<String, BiConsumer<EntityId, AssertionValues>> testWMIN8614B() {
		return Maps.immutableEntry("WMIN8614B", (projectId, assertionValues) -> {
			DiscoveryExcelUtil.compareIgnoreOrder(assertionValues.expectedMetricsContent, assertionValues.actualMetricsContent, Set.of("Uid", "Target Uid", "Cause",
					"Parent Uid", "Reached From Uids"), Collections.emptyMap());
			final var moduleQRSTCASE = moduleService.findModules(q -> q.ofProject(projectId).withName("QRSTCASE"));
			assertEquals(1, moduleQRSTCASE.size());
			final var moduleQRSTCASEFieldInfoList = dataSchemaService.find(q -> q.ofProject(projectId).ofModule(moduleQRSTCASE.get(0).identity()));
			assertEquals(0, moduleQRSTCASEFieldInfoList.size());
		});
	}

	Entry<String, BiConsumer<EntityId, AssertionValues>> testWMIN8867() {
		return Maps.immutableEntry("WMIN8867", (projectId, assertionValues) -> {
			DiscoveryExcelUtil.compareIgnoreOrder(assertionValues.expectedMetricsContent, assertionValues.actualMetricsContent, Set.of("Uid", "Target Uid", "Cause",
					"Parent Uid", "Reached From Uids"), Collections.emptyMap());

			final List<ModulePojo> stepCallingProc1 = moduleService.findModules(q -> q.ofProject(projectId).withName("PROD.STEP010.EXEC"));
			Assert.assertEquals(1, stepCallingProc1.size());
			final List<ModulePojo> stepCallingProc2 = moduleService.findModules(q -> q.ofProject(projectId).withName("JOB2.STEP010.EXEC"));
			Assert.assertEquals(1, stepCallingProc2.size());
			final List<ModulePojo> procSteps = moduleService.findModules(q -> q.ofProject(projectId).withName("APROC.PSTEP.EXEC_PGM"));
			Assert.assertEquals(1, procSteps.size());
			final var references = moduleService.findRelationship(q -> q.ofSource(procSteps.get(0).identity()));

			/* assert that the references created from proc step contains the stepsCallingProc */
			Assert.assertEquals(2, references.size());
			final var references1 = references.get(0);
			final var conditionalModules1 = references1.getValidIfReachedFrom();
			assertTrue(conditionalModules1.containsAll(List.of(stepCallingProc1.get(0).getUid(), stepCallingProc2.get(0).getUid())));

			final var references2 = references.get(1);
			final var conditionalModules2 =  references2.getValidIfReachedFrom();
			assertTrue(conditionalModules2.containsAll(List.of(stepCallingProc1.get(0).getUid(), stepCallingProc2.get(0).getUid())));

			/* Assert that other dependency steps invoked from job does not have conditional dependency */
			final var jobStep = moduleService.findModules(q -> q.ofProject(projectId).withName("JOB2.NOTPSTEP.EXEC_PGM"));
			Assert.assertEquals(1, jobStep.size());
			final var jobReferences = moduleService.findRelationship(q -> q.ofSource(jobStep.get(0).identity()));
			Assert.assertEquals(2, jobReferences.size());
			final var jobReference1 = jobReferences.get(0);
			assertEquals(0, jobReference1.getValidIfReachedFrom().size());
		});
	}

	Entry<String, BiConsumer<EntityId, AssertionValues>> testWMIN8867B() {
		return Maps.immutableEntry("WMIN8867B", (projectId, assertionValues) -> {
			DiscoveryExcelUtil.compareIgnoreOrder(assertionValues.expectedMetricsContent, assertionValues.actualMetricsContent, Set.of("Uid", "Target Uid", "Cause",
					"Parent Uid", "Reached From Uids"), Collections.emptyMap());

			final List<ModulePojo> stepCallingProc1 = moduleService.findModules(q -> q.ofProject(projectId).withName("PROD.STEP010.EXEC"));
			Assert.assertEquals(1, stepCallingProc1.size());
			final List<ModulePojo> stepCallingProc2 = moduleService.findModules(q -> q.ofProject(projectId).withName("JOB2.STEP010.EXEC"));
			Assert.assertEquals(1, stepCallingProc2.size());
			final List<ModulePojo> procSteps = moduleService.findModules(q -> q.ofProject(projectId).withName("APROC.PSTEP.EXEC_PGM"));
			Assert.assertEquals(1, procSteps.size());
			final var references = moduleService.findRelationship(q -> q.ofSource(procSteps.get(0).identity()));

			/* assert that the references created from proc step contains the stepsCallingProc rid */
			final var stepsCallingProc = new ArrayList<>(List.of(stepCallingProc1.get(0).getUid(), stepCallingProc2.get(0).getUid()));
			Assert.assertEquals(2, references.size());
			final var references1 = references.get(0);
			final var conditionalModules1 = references1.getValidIfReachedFrom();
			assertEquals(1, conditionalModules1.size());
			assertTrue(stepsCallingProc.contains(conditionalModules1.get(0)));
			stepsCallingProc.remove(conditionalModules1.get(0));

			final var references2 = references.get(1);
			final var conditionalModules2 =  references2.getValidIfReachedFrom();
			assertEquals(1, conditionalModules2.size());
			assertTrue(stepsCallingProc.contains(conditionalModules2.get(0)));
			stepsCallingProc.remove(conditionalModules2.get(0));
			assertTrue(stepsCallingProc.isEmpty());
		});
	}

	Entry<String, BiConsumer<EntityId, AssertionValues>> testWMIN8847() {
		return Maps.immutableEntry("WMIN8847", (projectId, assertionValues) -> {
			DiscoveryExcelUtil.compareIgnoreOrder(assertionValues.expectedMetricsContent, assertionValues.actualMetricsContent, Set.of("Uid", "Target Uid", "Cause",
					"Parent Uid", "Reached From Uids"), Collections.emptyMap());
			final List<ModulePojo> stepCallingProc1 = moduleService.findModules(q -> q.ofProject(projectId).withName("JOB1.STEP01.EXEC"));
			Assert.assertEquals(1, stepCallingProc1.size());
			final List<ModulePojo> stepCallingProc2 = moduleService.findModules(q -> q.ofProject(projectId).withName("JOB2.STEP01.EXEC"));
			Assert.assertEquals(1, stepCallingProc2.size());
			final List<ModulePojo> procSteps = moduleService.findModules(q -> q.ofProject(projectId).withName("PROC1.PSTEP1.EXEC_PGM"));
			Assert.assertEquals(1, procSteps.size());
			final var references = moduleService.findRelationship(q -> q.ofSource(procSteps.get(0).identity()));

			/* assert that the references created from proc step contains the stepsCallingProc rid */
			final var stepsCallingProc = new ArrayList<>(List.of(stepCallingProc1.get(0).getUid(), stepCallingProc2.get(0).getUid()));
			Assert.assertEquals(3, references.size());
			final var ddReferences = references.stream().filter(reference -> ! moduleService.getModuleLightweight(EntityId.of(reference.getDstModule())).getName().equals("IEFBR14"))
					.collect(Collectors.toList());
			assertEquals(2, ddReferences.size());
			final var references1 = ddReferences.get(0);
			final var conditionalModules1 = references1.getValidIfReachedFrom();
			assertEquals(1, conditionalModules1.size());
			assertTrue(stepsCallingProc.contains(conditionalModules1.get(0)));
			stepsCallingProc.remove(conditionalModules1.get(0));

			final var references2 = ddReferences.get(1);
			final var conditionalModules2 =  references2.getValidIfReachedFrom();
			assertEquals(1, conditionalModules2.size());
			assertTrue(stepsCallingProc.contains(conditionalModules2.get(0)));
			stepsCallingProc.remove(conditionalModules2.get(0));
			assertTrue(stepsCallingProc.isEmpty());
		});
	}
	
	Entry<String, BiConsumer<EntityId, AssertionValues>> testWMIN9998A() {
		return Maps.immutableEntry("WMIN9998A", (projectId, assertionValues) -> {
			DiscoveryExcelUtil.compareIgnoreOrder(assertionValues.expectedMetricsContent, assertionValues.actualMetricsContent, Set.of("Uid", "Target Uid", "Cause",
					"Parent Uid", "Reached From Uids"), Collections.emptyMap());
			jobToProcConditionalDependencyTest(projectId);
		});
	}
	
	Entry<String, BiConsumer<EntityId, AssertionValues>> testWMIN11000A() {
		return Maps.immutableEntry("WMIN11000A", (projectId, assertionValues) -> {
			DiscoveryExcelUtil.compareIgnoreOrder(assertionValues.expectedMetricsContent, assertionValues.actualMetricsContent,
					Set.of("Uid", "Target Uid", "Cause", "Parent Uid", "Reached From Uids"), Collections.emptyMap());
			final var modules = moduleService.findModuleIds(q -> q.ofProject(projectId).withName("BASIC_FUNCTION").withTechnology(Technology.UNKNOWN).withType(Type.UNKNOWN));
			assertEquals(1, modules.size(), "BASIC_FUNCTION module should be only one.");
		});
	}
	
	Entry<String, BiConsumer<EntityId, AssertionValues>> testWMIN13346() {
		return Maps.immutableEntry("WMIN13346", (projectId, assertionValues) -> {
			DiscoveryExcelUtil.compareIgnoreOrder(assertionValues.expectedMetricsContent, assertionValues.actualMetricsContent,
					Set.of("Uid", "Target Uid", "Cause", "Parent Uid", "Reached From Uids"), Collections.emptyMap());
			final Optional<ModulePojo> utiityModule = moduleService
					.findAnyModule(q -> q.ofProject(projectId).withName("DFSRRC00").withTechnology(Technology.UNKNOWN).withType(Type.UTILITY));
			assertTrue("DFSRRC00 module should be only one.", utiityModule.isPresent());
			assertEquals(Origin.ENVIRONMENT, utiityModule.get().getOrigin(), "Origin should be ENVIRONMENT.");
		});
	}
	
	Entry<String, BiConsumer<EntityId, AssertionValues>> testWMIN11000B() {
		return Maps.immutableEntry("WMIN11000B", (projectId, assertionValues) -> {
			DiscoveryExcelUtil.compareIgnoreOrder(assertionValues.expectedMetricsContent, assertionValues.actualMetricsContent,
					Set.of("Uid", "Target Uid", "Cause", "Parent Uid", "Reached From Uids"), Collections.emptyMap());
			final var modules = moduleService.findModuleIds(q -> q.ofProject(projectId).withName("IEBGENER").withTechnology(Technology.UNKNOWN).withType(Type.UTILITY));
			assertEquals(1, modules.size(), "IEBGENER module should be only one.");
		});
	}


	Entry<String, BiConsumer<EntityId, AssertionValues>> testWMIN9998B() {
		return Maps.immutableEntry("WMIN9998B", (projectId, assertionValues) -> {
			DiscoveryExcelUtil.compareIgnoreOrder(assertionValues.expectedMetricsContent, assertionValues.actualMetricsContent, Set.of("Uid", "Target Uid", "Cause",
					"Parent Uid", "Reached From Uids"), Collections.emptyMap());
			jobToProcConditionalDependencyTest(projectId);
			
			/* Tests checks if there is a Proc within a Proc still captures conditional dependency, it also checks 
			 * if the target module is a JCL_PROC then conditional dependency is not added to it */
			final List<ModulePojo> stepCallingProctoProc1 = moduleService.findModules(q -> q.ofProject(projectId).withName("PROCA.PSTEP2.EXEC"));
			Assert.assertEquals(1, stepCallingProctoProc1.size());
			final List<ModulePojo> stepCallingProctoProc2 = moduleService.findModules(q -> q.ofProject(projectId).withName("PROCA.PSTEP3.EXEC"));
			Assert.assertEquals(1, stepCallingProctoProc2.size());
			
			final var references = moduleService.findRelationship(q -> q.ofProject(projectId).ofSource(stepCallingProctoProc1.get(0).identity()));
			Assert.assertEquals(1, references.size());
			final var conditionalDependency = references.get(0).getValidIfReachedFrom();
			Assert.assertNotNull(conditionalDependency);
			/* Conditional dependency will not be assigned to the JCL_PROC */
			Assert.assertEquals(0, conditionalDependency.size());
			
			final List<ModulePojo> proctoProcSteps = moduleService.findModules(q -> q.ofProject(projectId).withName("PROCB.PSTEPB.EXEC_PGM"));
			Assert.assertEquals(1, proctoProcSteps.size());
			final var referencesFromProcToProc = moduleService.findRelationship(q -> q.ofProject(projectId).ofSource(proctoProcSteps.get(0).identity()));
			Assert.assertEquals(3, referencesFromProcToProc.size());

			referencesFromProcToProc.forEach(reference -> {
				final var dstModule = moduleService.getModuleLightweight(EntityId.of(reference.getDstModule()));
				
				if (dstModule.getName().equals("LF.FILE")) {
					assertTrue("LF.FILE module should contain a conditional dependency",
							reference.getValidIfReachedFrom().containsAll(List.of(stepCallingProctoProc1.get(0).getUid())));
				}

				if (dstModule.getName().equals("MF.FILE")) {
					assertTrue("MF.FILE module should contain a conditional dependency",
							reference.getValidIfReachedFrom().containsAll(List.of(stepCallingProctoProc2.get(0).getUid())));
				}

				if (dstModule.getName().equals("PRGB")) {
					assertTrue("PRGB module should contain two conditional dependency that are merged",
							reference.getValidIfReachedFrom().containsAll(List.of(stepCallingProctoProc1.get(0).getUid(),
									stepCallingProctoProc2.get(0).getUid())));
				}
			});
		});
	}
	
	Entry<String, BiConsumer<EntityId, AssertionValues>> testWMIN11274() {
		return Maps.immutableEntry("WMIN11274", (projectId, assertionValues) -> {
			DiscoveryExcelUtil.compareIgnoreOrder(assertionValues.expectedMetricsContent, assertionValues.actualMetricsContent,
					Set.of("Uid", "Target Uid", "Cause", "Parent Uid", "Reached From Uids"), Collections.emptyMap());
			final AggregationRequest<ModuleFieldName> aggregationRequest = new AggregationRequest<ModuleFieldName>();
			final Map<ModuleFieldName, AggregationOperator> reqFields = new HashMap<>();
			reqFields.put(ModuleFieldName.INBOUND, AggregationOperator.SUM);
			reqFields.put(ModuleFieldName.OUTBOUND, AggregationOperator.SUM);
			aggregationRequest.setFields(reqFields);
			aggregationRequest.setGroupBy(Set.of(ModuleFieldName.INTERFACE));
			final List<AggregationResult<ModuleFieldName>> aggregatedUtilityValues = utilityAggregationService.getAggregatedUtilityValues(projectId,
					aggregationRequest);
			assertEquals(1, aggregatedUtilityValues.size());
			final AggregationResult<ModuleFieldName> utilityValues = aggregatedUtilityValues.get(0);
			final Map<ModuleFieldName, Object> fields = utilityValues.getFields();
			final Map<ModuleFieldName, Object> group = utilityValues.getGroup();
			assertEquals("Utility for VSAM", group.get(ModuleFieldName.INTERFACE).toString());
			final String inbound = fields.get(ModuleFieldName.INBOUND).toString();
			assertEquals("0", inbound);
			final String outbound = fields.get(ModuleFieldName.OUTBOUND).toString();
			assertEquals("6", outbound);
		});
	}
	
	private void jobToProcConditionalDependencyTest(EntityId projectId) {
		final List<ModulePojo> stepCallingProc1 = moduleService.findModules(q -> q.ofProject(projectId).withName("JOBA.STEP1.EXEC"));
		Assert.assertEquals(1, stepCallingProc1.size());
		final List<ModulePojo> stepCallingProc2 = moduleService.findModules(q -> q.ofProject(projectId).withName("JOBA.STEP2.EXEC"));
		Assert.assertEquals(1, stepCallingProc2.size());
		final List<ModulePojo> stepCallingProc3 = moduleService.findModules(q -> q.ofProject(projectId).withName("JOBA.STEP3.EXEC"));
		Assert.assertEquals(1, stepCallingProc3.size());

		final List<ModulePojo> procSteps = moduleService.findModules(q -> q.ofProject(projectId).withName("PROCA.PSTEP1.EXEC_PGM"));
		Assert.assertEquals(1, procSteps.size());
		final var references = moduleService.findRelationship(q -> q.ofProject(projectId).ofSource(procSteps.get(0).identity()));
		Assert.assertEquals(3, references.size());

		references.forEach(reference -> {
			final var dstModule = moduleService.getModuleLightweight(EntityId.of(reference.getDstModule())); 
			
			if (dstModule.getName().equals("YF.FILE")) {
				assertTrue("YF.FILE module should contain two conditional dependency that are merged",
						reference.getValidIfReachedFrom().containsAll(List.of(stepCallingProc1.get(0).getUid(), stepCallingProc3.get(0).getUid())));
			}

			if (dstModule.getName().equals("ZF.FILE")) {
				assertTrue("ZF.FILE module should contain a conditional dependency",
						reference.getValidIfReachedFrom().containsAll(List.of(stepCallingProc2.get(0).getUid())));
			}

			if (dstModule.getName().equals("IEFBR14")) {
				assertTrue("IEFBR14 module should contain three conditional dependency that are merged",
						reference.getValidIfReachedFrom().containsAll(List.of(stepCallingProc1.get(0).getUid(),
								stepCallingProc2.get(0).getUid(), stepCallingProc3.get(0).getUid())));
			}
		});
	}
	
	/**
	 * Returns the custom assertions for {@link DiscoveryIrisTest}.
	 *
	 * @return Map of test cases where the test case name as the key and asserting BiConsumer as the value. This BiConsumer accepts the project id and the
	 * {@link AssertionValues}
	 *
	 * @throws IllegalAccessException
	 * @throws IllegalArgumentException
	 * @throws InvocationTargetException
	 */
	Map<String, BiConsumer<EntityId, AssertionValues>> getCustomAssertions() {
		/* This method invokes all the methods of return type: Entry<String, BiConsumer<Long, AssertionValues>> */
		final Map<String, BiConsumer<EntityId, AssertionValues>> testCases = new HashMap<>();
		for (final Method method : this.getClass().getDeclaredMethods()) {
			if (method.getGenericReturnType() != null && method.getGenericReturnType() instanceof ParameterizedType) {
				final ParameterizedType returnType = (ParameterizedType) method.getGenericReturnType();
				/* Checks if the return type is Entry and has two parameterized types. Also checks if the first type argument is String */
				if (returnType.getRawType() == Entry.class && returnType.getActualTypeArguments().length == 2
						&& returnType.getActualTypeArguments()[0] == String.class && returnType.getActualTypeArguments()[1] instanceof ParameterizedType) {
					final ParameterizedType returnType2 = (ParameterizedType) returnType.getActualTypeArguments()[1];
					/* Checks if the return type is BiConsumer and has two parameterized types. Also checks if the first type argument is Long and second type
					 * argument is AssertionValues */
					if (returnType2.getRawType() == BiConsumer.class && returnType2.getActualTypeArguments().length == 2
							&& returnType2.getActualTypeArguments()[0] == EntityId.class && returnType2.getActualTypeArguments()[1] == AssertionValues.class) {
						try {
							/* Invokes the filtered method and wraps into a Map with test case name as Key and the asserting function as the value */
							@SuppressWarnings("unchecked")
							final Entry<String, BiConsumer<EntityId, AssertionValues>> testCase = (Entry<String, BiConsumer<EntityId, AssertionValues>>) method
									.invoke(this);
							testCases.put(testCase.getKey(), testCase.getValue());
						} catch (final IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
							throw new IllegalStateException(e);
						}
					}
				}
			}
		}
		return testCases;
	}

	Entry<String, BiConsumer<EntityId, AssertionValues>> testWMIN14118A() {
		return Maps.immutableEntry("WMIN14118A", (projectId, assertionValues) -> {
			testWMIN14118(projectId);
		});
	}

	Entry<String, BiConsumer<EntityId, AssertionValues>> testWMIN14118B() {
		return Maps.immutableEntry("WMIN14118B", (projectId, assertionValues) -> {
			testWMIN14118(projectId);
		});
	}

	/* shared assertions for WMIN14118A and WMIN14118B */
	private void testWMIN14118(final EntityId projectId) {
		final Optional<ModulePojo> job1 = moduleService.findAnyModule(q -> q.ofProject(projectId).withName("JOB1"));
		final Optional<ModulePojo> job2 = moduleService.findAnyModule(q -> q.ofProject(projectId).withName("JOB2"));

		assertTrue("Expected to find Module for Job1", job1.isPresent());
		assertTrue("Expected to find Module for Job2", job2.isPresent());

		/* expect to find only FILE1 in the outbound call chain for JOB1 */
		final List<CallChainGraph> callChainGraphs = callChainService.createCallChainGraphs(new NullProgressMonitor(), new Parameters.Builder()
				.setProjectId(projectId)
				.setCallTypes(Set.of(RelationshipType.CALLS, RelationshipType.ACCESSES))
				.setStartModuleIds(List.of(job1.get().identity()))
				.setEndModuleTypes(Set.of(Type.FILE))
				.setDirections(List.of(CallChain.CallChainDirection.OUT))
				.setParallel(1)
				.build()).orElseThrow(() -> new IllegalStateException("failed to create call chain graphs"));

		assertEquals(1, callChainGraphs.size(), "expected one call chain graph for 'JOB1 outgoing'");
		final List<ModuleLightweightPojo> endModules = callChainGraphs.get(0).getEndModules(f -> true);
		final Set<String> endModuleNames = endModules.stream().map(ModuleLightweightPojo::getName).collect(Collectors.toSet());
		assertEquals(Set.of("FILE1"), endModuleNames, "expected only one end module 'FILE1'");

		/* expect to find only FILE2 in the outbound call chain for JOB2 */
		final List<CallChainGraph> callChainGraphs2 = callChainService.createCallChainGraphs(new NullProgressMonitor(), new Parameters.Builder()
				.setProjectId(projectId)
				.setCallTypes(Set.of(RelationshipType.CALLS, RelationshipType.ACCESSES))
				.setStartModuleIds(List.of(job2.get().identity()))
				.setEndModuleTypes(Set.of(Type.FILE))
				.setDirections(List.of(CallChain.CallChainDirection.OUT))
				.setParallel(1)
				.build()).orElseThrow(() -> new IllegalStateException("failed to create call chain graphs"));

		assertEquals(1, callChainGraphs2.size(), "expected one call chain graph for 'JOB2 outgoing'");
		final List<ModuleLightweightPojo> endModules2 = callChainGraphs2.get(0).getEndModules(f -> true);
		final Set<String> endModuleNames2 = endModules2.stream().map(ModuleLightweightPojo::getName).collect(Collectors.toSet());
		assertEquals(Set.of("FILE2"), endModuleNames2, "expected only one end module 'FILE2'");
	}

	private void validateErrorUIDs(final ErrorMarkerPojo error, final List<Long> moduleUidList) {
		if (StringUtils.contains(error.getCause(), ERROR_MSG)) {
			final String errorUID = assertNotNull(error.getCause()).replaceAll(".*\\[(.*?)\\].*", "$1");
			assertTrue(
					moduleUidList.containsAll(Arrays.asList(errorUID.split(",")).stream().map(uid -> Long.valueOf(uid.trim())).collect(Collectors.toList())));
		}
	}

	private List<Long> getModuleUIDs(final Map<String, List<String[]>> workbookMap) {
		final List<String[]> moduleRows = assertNotNull(workbookMap.get(WorkbookDefinition.SHEET_MODULES), "Module sheet rows must not be null");
		if (moduleRows.isEmpty()) {
			return Collections.emptyList();
		}

		return moduleRows.stream()
					.skip(1) /* Skip Header "UID" */
					.map(moduleRow -> StringUtils.isBlank(moduleRow[0]) ? Long.valueOf(0) : Long.valueOf(moduleRow[0]))
					.collect(Collectors.toList());
	}
}
