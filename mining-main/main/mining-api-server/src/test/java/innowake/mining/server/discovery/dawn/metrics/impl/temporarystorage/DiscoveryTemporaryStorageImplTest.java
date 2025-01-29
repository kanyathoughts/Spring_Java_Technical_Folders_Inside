/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.impl.temporarystorage;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.shared.entities.testing.ModulePojoDummy.newModuleDefinition;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.discovery.metrics.DiscoveryCache;
import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.api.model.ContributorResult;
import innowake.mining.server.discovery.dawn.metrics.api.model.DeferredActionDefinition;
import innowake.mining.server.discovery.dawn.metrics.api.temporarystorage.DiscoveryTemporaryStorage;
import innowake.mining.server.discovery.dawn.metrics.test.DiscoveryTestContext;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.ClientService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ClientPojo;
import innowake.mining.shared.entities.ModuleDeadCodePojoPrototype;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.StatementPojoPrototype;
import innowake.mining.shared.entities.testing.SourcePojoDummy;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.StatementType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;


/**
 *  Tests for testing methods of {@link DiscoveryTemporaryStorageImpl}.
 */
class DiscoveryTemporaryStorageImplTest extends DatabaseRelatedTest {

	@Autowired
	private DiscoveryTemporaryStorage discoveryTemporaryStorage;
	
	@Autowired
	private DiscoveryCache discoveryCache;
	
	@Nullable
	DiscoveryContext context;
	
	@Autowired
	private ClientService clientService;
	
	EntityId projectId = null;
	
	@BeforeAll
	void initialize() {
		final ClientPojo client = clientService.get(EntityId.of(Long.valueOf(1)), false);
		projectId = projectService.create(new ProjectPojoPrototype()
				.setName("Test Project")
				.setNatures(Collections.emptySet())
				.setClient(client.identity())
			).identity();

		final SourcePojo testSource = SourcePojoDummy.build(o -> o
				.setProject(projectId)
				.setName("TEST1")
				.setPath("/src/cobol/programs/TEST1.cbl")
				.setTechnology(Technology.COBOL)
				.setType(Type.PROGRAM)
				.setContent(new BinaryString("THERE SHOULD BE COBOL IN HERE")));
		context = new DiscoveryTestContext(Collections.singletonList(testSource), projectId);
	}
	
	@Test
	void testStoreUnanchoredResult() {
		final StatementPojoPrototype statement = newStatementDefinition(StatementType.EXECUTE, "EXECUTE PGM", Collections.emptyMap());
		final ErrorMarker error = new ErrorMarker(Severity.ERROR, ErrorKey.PARSE_ERROR, "Error occured", null);
		final ModuleDeadCodePojoPrototype deadCode = newModelDeadCode("deadCode", 10, 12);
		final String name = "subModule1";
		final ModulePojoPrototype moduleDefinition = newModuleDefinition(name, ModuleType.ASSEMBLER_PROGRAM, null,
				null, Storage.FILE_SECTION, Representation.VIRTUAL,
				Collections.emptyList(), Identification.IDENTIFIED, Origin.CUSTOM);
		final ContributorResult contributorResult1 = new ContributorResult(ContributorResult.Type.ANCHOR_TO,
				new ModuleFilter().setNames(name),
				Collections.emptySet(), moduleDefinition, Collections.singletonList(error),
				Collections.singletonList(deadCode), Collections.emptyList(), Collections.singletonList(statement),
				Collections.emptyList());
		discoveryTemporaryStorage.storeUnanchoredContributorResult(assertNotNull(context).getJobId(), contributorResult1);
		final Set<ContributorResult> results = discoveryCache.getMultiValue(assertNotNull(context).getJobId(),
				DiscoveryTemporaryStorageImpl.UNACHOR_KEY).stream()
				.map(ContributorResult.class::cast).collect(Collectors.toSet());
		assertEquals(1, results.size());
		assertEqualsContributorResult(contributorResult1, results.iterator().next());
	}
	
	@Test
	void testGetAndRemoveUnanchoredResult() {
		final ModuleDeadCodePojoPrototype deadCode = newModelDeadCode("deadCode", 10, 12);
		final ModulePojoPrototype moduleDefinition = newModuleDefinition("subModule1", ModuleType.ASSEMBLER_PROGRAM, null,
				null, Storage.FILE_SECTION, Representation.VIRTUAL,
				Collections.emptyList(), Identification.IDENTIFIED, Origin.CUSTOM);
		final ContributorResult contributorResult1 = new ContributorResult(ContributorResult.Type.ANCHOR_TO,
				new ModuleFilter().setNames("subModule1"),
				Collections.emptySet(), moduleDefinition, Collections.emptyList(),
				Collections.singletonList(deadCode), Collections.emptyList(), Collections.emptyList(), Collections.emptyList());
		discoveryTemporaryStorage.storeUnanchoredContributorResult(assertNotNull(context).getJobId(), contributorResult1);
		final Collection<ContributorResult> contributorResults = 
				discoveryTemporaryStorage.getAndRemoveUnanchoredContributorResults(assertNotNull(context).getJobId());
		assertEquals(1, contributorResults.size());
		/* check whether correct contributor result is returned */
		assertEqualsContributorResult(contributorResult1, contributorResults.iterator().next());
		final Set<ContributorResult> results = discoveryCache.getMultiValue(assertNotNull(context).getJobId(),
				DiscoveryTemporaryStorageImpl.UNACHOR_KEY).stream()
				.map(ContributorResult.class::cast).collect(Collectors.toSet());
		assertEquals(0, results.size());
	}
	
	@Test
	void testStoreDeferredActions() {
		final DeferredActionDefinition actionDefinition1 = new DeferredActionDefinition("CContributor", "process" );
		final DeferredActionDefinition actionDefinition2 = new DeferredActionDefinition("JavaContributor", "resolve" );
		discoveryTemporaryStorage.storeDeferredAction(assertNotNull(context).getJobId(), EntityId.of(0l), actionDefinition1);
		discoveryTemporaryStorage.storeDeferredAction(assertNotNull(context).getJobId(), EntityId.of(0l), actionDefinition2);
		discoveryTemporaryStorage.storeDeferredAction(assertNotNull(context).getJobId(), EntityId.of(1l), actionDefinition2);
		final List<EntityId> allModuleIds = discoveryCache.getMultiValue(assertNotNull(context).getJobId(),
				DiscoveryTemporaryStorageImpl.DEFERRED_ACTIONS_MODULEIDS).stream().map(EntityId.class::cast).collect(Collectors.toList());
		assertEquals(2, allModuleIds.size());
		assertEquals(EntityId.of(0l), allModuleIds.get(0));
		assertEquals(EntityId.of(1l), allModuleIds.get(1));
		final List<DeferredActionDefinition> allActionsWithId0 = discoveryCache.getMultiValue(assertNotNull(context).getJobId(),
				DiscoveryTemporaryStorageImpl.DEFERRED_ACTIONS_FOR_MODULE + EntityId.of(0l)).stream()
				.map(DeferredActionDefinition.class::cast).collect(Collectors.toList());
		final List<DeferredActionDefinition> allActionsWithId1 = discoveryCache.getMultiValue(assertNotNull(context).getJobId(),
				DiscoveryTemporaryStorageImpl.DEFERRED_ACTIONS_FOR_MODULE  + EntityId.of(1l)).stream()
				.map(DeferredActionDefinition.class::cast).collect(Collectors.toList());
		assertEquals(2, allActionsWithId0.size());
		assertEquals(1, allActionsWithId1.size());
		allActionsWithId0.sort(Comparator.comparing(DeferredActionDefinition::getContributorClassName));
		allActionsWithId1.sort(Comparator.comparing(DeferredActionDefinition::getContributorClassName));
		assertEqualsDeferredAction(actionDefinition1, allActionsWithId0.get(0));
		assertEqualsDeferredAction(actionDefinition2, allActionsWithId0.get(1));
		assertEqualsDeferredAction(actionDefinition2, allActionsWithId1.get(0));
		discoveryCache.clearDiscoveryJobCache(assertNotNull(context).getJobId());
	}
	
	@Test
	void testGetModulesDeferredActions() {
		final DeferredActionDefinition actionDefinition1 = new DeferredActionDefinition("CContributor", "process" );
		final DeferredActionDefinition actionDefinition2 = new DeferredActionDefinition("JavaContributor", "resolve" );
		discoveryTemporaryStorage.storeDeferredAction(assertNotNull(context).getJobId(), EntityId.of(0l), actionDefinition1);
		discoveryTemporaryStorage.storeDeferredAction(assertNotNull(context).getJobId(), EntityId.of(0l), actionDefinition2);
		discoveryTemporaryStorage.storeDeferredAction(assertNotNull(context).getJobId(), EntityId.of(1l), actionDefinition2);
		final List<EntityId> expectedModuleIds = discoveryCache.getMultiValue(assertNotNull(context).getJobId(),
				DiscoveryTemporaryStorageImpl.DEFERRED_ACTIONS_MODULEIDS).stream().map(EntityId.class::cast).collect(Collectors.toList());
		final List<EntityId> actualModuleIds = discoveryTemporaryStorage.getAndRemoveModulesWithDeferredActions(assertNotNull(context).getJobId()).stream()
				.map(EntityId.class::cast).collect(Collectors.toList());
		assertEquals(expectedModuleIds.size(), actualModuleIds.size());
		assertTrue(actualModuleIds.containsAll(expectedModuleIds));
		discoveryCache.clearDiscoveryJobCache(assertNotNull(context).getJobId());
	}
	
	@Test
	void testGetDeferredActions() {
		final DeferredActionDefinition actionDefinition1 = new DeferredActionDefinition("CContributor", "process" );
		final DeferredActionDefinition actionDefinition2 = new DeferredActionDefinition("JavaContributor", "resolve" );
		discoveryTemporaryStorage.storeDeferredAction(assertNotNull(context).getJobId(), EntityId.of(5l), actionDefinition1);
		discoveryTemporaryStorage.storeDeferredAction(assertNotNull(context).getJobId(), EntityId.of(5l), actionDefinition2);
		final List<DeferredActionDefinition> actualActions = discoveryTemporaryStorage.getAndRemoveDeferredActions(assertNotNull(context).getJobId(), EntityId.of(5l))
				.stream().map(DeferredActionDefinition.class::cast).collect(Collectors.toList());
		assertEquals(2, actualActions.size());
		actualActions.sort(Comparator.comparing(DeferredActionDefinition::getName));
		assertEqualsDeferredAction(actionDefinition1, actualActions.get(0));
		assertEqualsDeferredAction(actionDefinition2, actualActions.get(1));
		/* check that deferred actions for module is deleted */
		final List<DeferredActionDefinition> actionsInCache = discoveryCache.getMultiValue(assertNotNull(context).getJobId(),
				DiscoveryTemporaryStorageImpl.DEFERRED_ACTIONS_FOR_MODULE + Long.valueOf(5)).stream()
				.map(DeferredActionDefinition.class::cast).collect(Collectors.toList());
		final Collection<DeferredActionDefinition> modulesWithDeferredActions = discoveryTemporaryStorage
				.getAndRemoveDeferredActions(assertNotNull(context).getJobId(), EntityId.of(5l));
		assertEquals(0, actionsInCache.size());
		assertEquals(0, modulesWithDeferredActions.size());
		/* As ModuleIds are deleted only when calling DiscoveryTemporaryStorage#getAndRemoveModulesWithDeferredActions, We need to  check that there are no
		 deferredActions for that module is present in discoveryCache*/
		final Set<Object> moduleIds = discoveryCache.getMultiValue(assertNotNull(context).getJobId(),
				DiscoveryTemporaryStorageImpl.DEFERRED_ACTIONS_FOR_MODULE + Long.valueOf(5));
		assertEquals(0, moduleIds.size());
		discoveryCache.clearDiscoveryJobCache(assertNotNull(context).getJobId());
	}
	
	@Test
	void testGetDeferredActionsWithMultipleModules() {
		final DeferredActionDefinition actionDefinition1 = new DeferredActionDefinition("CContributor", "process" );
		final DeferredActionDefinition actionDefinition2 = new DeferredActionDefinition("JavaContributor", "resolve" );
		discoveryTemporaryStorage.storeDeferredAction(assertNotNull(context).getJobId(), EntityId.of(1l), actionDefinition1);
		discoveryTemporaryStorage.storeDeferredAction(assertNotNull(context).getJobId(), EntityId.of(1l), actionDefinition2);
		discoveryTemporaryStorage.storeDeferredAction(assertNotNull(context).getJobId(), EntityId.of(2l), actionDefinition2);
		discoveryTemporaryStorage.storeDeferredAction(assertNotNull(context).getJobId(), EntityId.of(3l), actionDefinition1);
		final List<DeferredActionDefinition> actualActions = discoveryTemporaryStorage.getAndRemoveDeferredActions(assertNotNull(context).getJobId(), EntityId.of(1l))
				.stream().map(DeferredActionDefinition.class::cast).collect(Collectors.toList());
		assertEquals(2, actualActions.size());
		actualActions.sort(Comparator.comparing(DeferredActionDefinition::getName));
		assertEqualsDeferredAction(actionDefinition1, actualActions.get(0));
		assertEqualsDeferredAction(actionDefinition2, actualActions.get(1));
		/* check that deferred actions for module is deleted */
		final List<DeferredActionDefinition> actionsInCache = discoveryCache.getMultiValue(assertNotNull(context).getJobId(),
				DiscoveryTemporaryStorageImpl.DEFERRED_ACTIONS_FOR_MODULE + Long.valueOf(1)).stream()
				.map(DeferredActionDefinition.class::cast).collect(Collectors.toList());
		final Collection<DeferredActionDefinition> modulesWithDeferredActions = discoveryTemporaryStorage
				.getAndRemoveDeferredActions(assertNotNull(context).getJobId(), EntityId.of(1l));
		assertEquals(0, actionsInCache.size());
		assertEquals(0, modulesWithDeferredActions.size());
		final List<DeferredActionDefinition> actionsForId2 = 
				discoveryTemporaryStorage.getAndRemoveDeferredActions(assertNotNull(context).getJobId(), EntityId.of(2l))
					.stream().map(DeferredActionDefinition.class::cast).collect(Collectors.toList());
		assertEquals(1, actionsForId2.size());
		assertEqualsDeferredAction(actionDefinition2, actionsForId2.get(0));
		discoveryCache.clearDiscoveryJobCache(assertNotNull(context).getJobId());
	}

	private void assertEqualsDeferredAction(DeferredActionDefinition expected, DeferredActionDefinition actual) {
		assertEquals(expected.getName(), actual.getName());
		assertEquals(expected.getContributorClassName(), actual.getContributorClassName());
	}

	private void assertEqualsContributorResult(final ContributorResult expected, final ContributorResult actual) {
		assertEquals(expected.getStatements().size(), actual.getStatements().size());
		assertEquals(expected.getErrors().size(), actual.getErrors().size());
		assertEquals(expected.getDeadCodes().size(), actual.getDeadCodes().size());
		assertEquals(expected.getModuleDefinition().name.orElse(StringUtils.EMPTY),
				actual.getModuleDefinition().name.orElse(StringUtils.EMPTY));
	}

	private StatementPojoPrototype newStatementDefinition(final StatementType statementType, final String text, final Map<String, Object> properties) {
		return new StatementPojoPrototype()
				.setType(statementType)
				.setText(text)
				.setProperties(properties);
	}

	private ModuleDeadCodePojoPrototype newModelDeadCode(final String name, final int offset, final int length) {
		return new ModuleDeadCodePojoPrototype()
				.setDeadCode(name)
				.setStartingLine(offset)
				.setNumberOfLines(length);
	}
}
