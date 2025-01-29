/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.impl.core;

import com.google.common.collect.Sets;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.discovery.metrics.DiscoveryCache;
import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.data.model.discovery.attribute.ModelAttributeValue;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DeferredAction;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilder.ModuleBuilder;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryBuilderFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributor;
import innowake.mining.server.discovery.dawn.metrics.api.contributor.DiscoveryContributorFromSource;
import innowake.mining.server.discovery.dawn.metrics.api.core.DiscoveryContext;
import innowake.mining.server.discovery.dawn.metrics.api.model.ContributorResult;
import innowake.mining.server.discovery.dawn.metrics.api.model.DeferredActionDefinition;
import innowake.mining.server.discovery.dawn.metrics.api.persistence.ImportResult;
import innowake.mining.server.discovery.dawn.metrics.api.temporarystorage.DiscoveryTemporaryStorage;
import innowake.mining.server.discovery.dawn.metrics.impl.persistence.DiscoveryPersistenceImpl;
import innowake.mining.server.discovery.dawn.metrics.test.DiscoveryTestContext;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.ClientService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ModuleService.RelationshipField;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.searchorder.SearchOrder;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.discovery.config.searchorder.Source;
import innowake.mining.shared.discovery.config.searchorder.Target;
import innowake.mining.shared.discovery.config.utility.UtilityList;
import innowake.mining.shared.entities.ClientPojo;
import innowake.mining.shared.entities.DependencyDefinitionPojo;
import innowake.mining.shared.entities.DependencyDefinitionPojoPrototype;
import innowake.mining.shared.entities.ErrorMarkerPojo;
import innowake.mining.shared.entities.ModuleDeadCodePojo;
import innowake.mining.shared.entities.ModuleDeadCodePojoPrototype;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModuleLightweightPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourceMetricsPojo;
import innowake.mining.shared.entities.SourceMetricsPojoPrototype;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.entities.StatementPojo;
import innowake.mining.shared.entities.StatementPojoPrototype;
import innowake.mining.shared.entities.testing.SourcePojoDummy;
import innowake.mining.shared.model.AdditionalInfo;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.StatementType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.mining.shared.model.discovery.Severity;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.boot.test.mock.mockito.SpyBean;

import java.io.Serializable;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;

/**
 * Tests for testing methods of {@link DiscoveryCoreImpl}.
 */
class DiscoveryCoreTest extends DatabaseRelatedTest {

	@Autowired
	private ClientService clientService;
	
	@Autowired
	private ModuleService moduleService;
	
	@Autowired
	private AutowireCapableBeanFactory autowireCapableBeanFactory;
	
	@Autowired
	private SourceService sourceService;

	private DiscoveryCoreImpl discoveryCore;
	
	private DiscoveryCoreImpl discoveryCoreForDeferredAction;
	
	EntityId projectId = EntityId.VOID;

	DiscoveryContext context;
	
	@Autowired
	@SpyBean
	DiscoveryPersistenceImpl discoveryPersistence;
	
	@Autowired
	private DiscoveryTemporaryStorage temporaryStorage;
	
	@Autowired
	private DiscoveryCache discoveryCache;
	
	EntityId dependencyIdForUpperCase = EntityId.VOID;
	EntityId dependencyIdForLowerCase = EntityId.VOID;
	EntityId dependencyId3 = EntityId.VOID;
	StatementPojoPrototype statement;
	ErrorMarker error;
	ModuleDeadCodePojoPrototype deadCode;
	ModulePojoPrototype moduleDefinition;
	ContributorResult contributorResultForRoot;
	ContributorResult contributorResultForSub;
	
	@BeforeAll
	void initialize() {
		final ClientPojo client = clientService.get(EntityId.of(Long.valueOf(1)), true);
		projectId = projectService.create(new ProjectPojoPrototype()
				.setName("Test Project")
				.setNatures(Collections.emptySet())
				.setClient(client.identity())).identity();
		final SourcePojo testSource = SourcePojoDummy.build(o -> o
				.setName("TEST1")
				.setPath("/src/cobol/programs/TEST1.cbl")
				.setTechnology(Technology.COBOL)
				.setType(Type.PROGRAM)
				.setContent(new BinaryString("THERE SHOULD BE COBOL IN HERE")));
		context = new DiscoveryTestContext(Collections.singletonList(testSource), projectId);
		discoveryCore = new DiscoveryCoreImpl(Collections.singletonList(new NoSourceContributor()), Collections.singletonList(new SourceContributor()));
		autowireCapableBeanFactory.autowireBean(discoveryCore);
		
		dependencyIdForUpperCase = createTestModule("FOO", "/cobol/programs/foo7.pgm", ModuleType.COBOL_PROGRAM);
		dependencyIdForLowerCase = createTestModule("foo", "/cobol/programs/foo8.pgm", ModuleType.COBOL_PROGRAM);
		dependencyId3 = createTestModule("dependencyModule", "/cobol/programs/cobol4.pgm", ModuleType.COBOL_PROGRAM);
		statement = new StatementPojoPrototype()
				.setType(StatementType.EXECUTE)
				.setText("EXECUTE PGM");
		error = new ErrorMarker(Severity.ERROR, ErrorKey.PARSE_ERROR, "Error occured", null);
		deadCode = new ModuleDeadCodePojoPrototype()
				.setNumberOfLines(12)
				.setStartingLine(10)
				.setDeadCode("deadCode");
		final DependencyDefinitionPojoPrototype dependencyDefinitionForSub = new DependencyDefinitionPojoPrototype()
				.setModuleFilters(List.of(new ModuleFilter()))
				.setRelationshipType(RelationshipType.NONE)
				.setResolutionFlags(Collections.emptySet())
				.setBindingType(Binding.UNKNOWN);
		moduleDefinition = newModuleDefinition("subModule1", ModuleType.ASSEMBLER_PROGRAM, null,
				null, Storage.FILE_SECTION, ModulePojo.Representation.VIRTUAL,
				Collections.emptyList(), Identification.IDENTIFIED, Origin.CUSTOM);
		contributorResultForRoot = new ContributorResult(ContributorResult.Type.ROOT_MODULE,
				new ModuleFilter().setNames("subModule1"),
				Collections.emptySet(), moduleDefinition, Collections.singletonList(error),
				Collections.singletonList(deadCode), Collections.emptyList(), Collections.singletonList(statement),
				Collections.emptyList());
		contributorResultForSub = new ContributorResult(ContributorResult.Type.SUB_MODULE,
				new ModuleFilter().setNames("subModule1"),
				Collections.emptySet(), moduleDefinition, Collections.singletonList(error),
				Collections.singletonList(deadCode), Collections.emptyList(), Collections.singletonList(statement),
				Collections.singletonList(dependencyDefinitionForSub));
		discoveryCoreForDeferredAction = new DiscoveryCoreImpl(Collections.singletonList(new NoSourceContributor()), Collections.singletonList(new SourceContributor()));
		autowireCapableBeanFactory.autowireBean(discoveryCoreForDeferredAction);
	}
	
	/**
	 * Test to execute contributor on no source object
	 */
	@Test
	void testExecuteContributor() {
		final List<DiscoveryContributor> discoveryContributorList = ContributorProviderUtility.provideContributors();
		assertEquals(1, discoveryContributorList.size());		
		final List<ContributorResult> contributorResultList = discoveryCore
				.executeContributor(discoveryContributorList.get(0), new DiscoveryTestContext(Collections.emptyList()));
		assertEquals(1, contributorResultList.size());
		ContributorResult contributorResult = contributorResultList.get(0);
		assertEquals(ContributorResult.Type.EXTERNAL_MODULE, contributorResult.getType());
		assertEquals(ModuleType.COBOL_PROGRAM.getTechnology(), contributorResult.getModuleDefinition().technology.get());
		assertEquals(ModuleType.COBOL_PROGRAM.getType(), contributorResult.getModuleDefinition().type.get());
		assertEquals("TEST-MODULE", contributorResult.getModuleDefinition().name.get());
	}
	
	/**
	 * Test to execute contributor on no source object for external module with origin.
	 */
	@Test
	void testExecuteContributorForExternalModuleWithOrigin() {
		final List<DiscoveryContributor> discoveryContributorList = ContributorProviderUtility.provideContributorsForExternalModuleWithOrigin();
		assertEquals(1, discoveryContributorList.size());		
		final List<ContributorResult> contributorResultList = discoveryCore
				.executeContributor(discoveryContributorList.get(0), new DiscoveryTestContext(Collections.emptyList()));
		assertEquals(1, contributorResultList.size());
		final ContributorResult contributorResult = contributorResultList.get(0);
		assertEquals(ContributorResult.Type.EXTERNAL_MODULE, contributorResult.getType());
		assertEquals(ModuleType.UNKNOWN_UTILITY.getTechnology(), contributorResult.getModuleDefinition().technology.get());
		assertEquals(ModuleType.UNKNOWN_UTILITY.getType(), contributorResult.getModuleDefinition().type.get());
		assertEquals("DFSRRC00", contributorResult.getModuleDefinition().name.get());
		assertEquals(Origin.ENVIRONMENT, contributorResult.getModuleDefinition().origin.get());
		discoveryCore.importContributorResults(context, contributorResultList);
		final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(projectId).withName("DFSRRC00"));
		assertEquals(1, modules.size());
		assertEquals(Origin.ENVIRONMENT, modules.get(0).getOrigin());
	}

	/**
	 * Test to execute contributor on source object
	 */
	@Test
	void testExecuteContributorOnSourceObject() {		
		final SourcePojo sourceObjectCobol = SourcePojoDummy.build(o -> o.setTechnology(Technology.COBOL).setType(Type.PROGRAM));
		final DiscoveryContext discoveryContextCobol = new DiscoveryTestContext(Collections.singletonList(sourceObjectCobol));
		final SourcePojo sourceObjectNatural = SourcePojoDummy.build(o -> o.setTechnology(Technology.NATURAL).setType(Type.PROGRAM));
		final DiscoveryContext discoveryContextNatural = new DiscoveryTestContext(Collections.singletonList(sourceObjectNatural));
		final SourcePojo sourceObjectJava = SourcePojoDummy.build(o -> o.setTechnology(Technology.JAVA).setType(Type.PROGRAM));
		final DiscoveryContext discoveryContextJava = new DiscoveryTestContext(Collections.singletonList(sourceObjectJava));	
		final List<DiscoveryContributorFromSource> discoveryContributorFromSourceList = ContributorProviderUtility.provideSourceContributors();
		assertEquals(4, discoveryContributorFromSourceList.size());
		discoveryContributorFromSourceList.forEach(contributor -> {			
			if (contributor.getClass().getSimpleName().equals("CobolTestContributor")) {
				testContributorResult(contributor, sourceObjectCobol, discoveryContextCobol, ModuleType.COBOL_PROGRAM, "COBOL-MODULE");
				return;
			} else if (contributor.getClass().getSimpleName().equals("CustomExtensionContributor")){				
				testContributorResult(contributor, sourceObjectCobol, discoveryContextCobol, ModuleType.COBOL_PROGRAM, "CUSTOM-COBOL-MODULE");
				testContributorResult(contributor, sourceObjectJava, discoveryContextJava, ModuleType.JAVA_TYPE, "CUSTOM-JAVA-MODULE");
				return;
			} else if (contributor.getClass().getSimpleName().equals("NaturalTestContributor")){
				testContributorResult(contributor, sourceObjectNatural, discoveryContextNatural, ModuleType.NATURAL_PROGRAM, "NATURAL-MODULE");
				return;
			} else if (contributor.getClass().getSimpleName().equals("JavaTestContributor")){
				testContributorResult(contributor, sourceObjectJava, discoveryContextJava, ModuleType.JAVA_TYPE, "JAVA-MODULE");
				return;
			}			
		});			
	}
	
	@Test
	void testImportContributorResultWithMultipleRootModule() {
		final var results =  Arrays.asList(contributorResultForRoot, contributorResultForRoot);
		final IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () ->
			discoveryCore.importContributorResults(context, results)
		);
		assertTrue(exception.getMessage().contains("More than one root module is not allowed"));
	}
	
	@Test
	void testImportContributorResultWithDbError() {
		final ModulePojoPrototype moduleDefinition = newModuleDefinition("foo", null, null, null, null, null,
				Collections.emptyList(), null, null);
		final ContributorResult contributorResult1 = new ContributorResult(ContributorResult.Type.ROOT_MODULE,
				new ModuleFilter().setNames("invalidModule"),
				Collections.emptySet(), moduleDefinition, Collections.singletonList(error),
				Collections.singletonList(deadCode), Collections.emptyList(), Collections.singletonList(statement),
				Collections.emptyList());
		final var results = List.of(contributorResult1);
		final IllegalStateException exception = assertThrows(IllegalStateException.class, () -> 
			discoveryCore.importContributorResults(context, results)
		);
		assertTrue(exception.getMessage().contains("Error occured while persisting Module"));
	}
	
	@Test
	void testImportContributorResultWithoutRoot() {
		discoveryCore.importContributorResults(context, Arrays.asList(contributorResultForSub));
		final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(projectId).withName("subModule1"));
		assertEquals(1, modules.size());
		final List<ErrorMarkerPojo> errors = moduleService.findErrorMarkers(q -> q.ofProject(projectId).ofModule(modules.get(0).identity()));
		assertEquals(1, errors.size());
		final List<ModuleDeadCodePojo> deadCodes = moduleService.findDeadCode(q -> q.ofModule(modules.get(0).identity()));
		assertEquals(1, deadCodes.size());
		final List<StatementPojo> statements = moduleService.findStatements(q -> q.ofModule(modules.get(0).identity()));
		assertEquals(1, statements.size());
	}
	
	@Test
	void testImportContributorResultWithRoot() {
		final var moduleDefinition1 = newModuleDefinition("rootModule1", ModuleType.ASSEMBLER_PROGRAM, null,
				"src/rootModule1", Storage.FILE, ModulePojo.Representation.PHYSICAL,
				Collections.emptyList(), Identification.IDENTIFIED, Origin.CUSTOM);
		final var moduleDefinition2 = newModuleDefinition("subModule2", ModuleType.ASSEMBLER_PROGRAM, null,
				null, Storage.FILE_SECTION, ModulePojo.Representation.VIRTUAL,
				Collections.emptyList(), Identification.IDENTIFIED, Origin.CUSTOM);
		final ContributorResult contributorResult1 = new ContributorResult(ContributorResult.Type.ROOT_MODULE,
				new ModuleFilter().setNames("rootModule1"),
				Collections.emptySet(), moduleDefinition1, Collections.singletonList(error),
				Collections.singletonList(deadCode), Collections.emptyList(), Collections.singletonList(statement),
				Collections.emptyList());
		
		final ContributorResult contributorResult2 = new ContributorResult(ContributorResult.Type.SUB_MODULE,
				new ModuleFilter().setNames("subModule2"),
				Collections.emptySet(), moduleDefinition2, Collections.singletonList(error),
				Collections.singletonList(deadCode), Collections.emptyList(), Collections.singletonList(statement),
				Collections.emptyList());
		
		final ContributorResult contributorResult3 = new ContributorResult(ContributorResult.Type.SUB_MODULE,
				new ModuleFilter().setNames("subModule3"),
				Collections.emptySet(), moduleDefinition2, Collections.singletonList(error),
				Collections.singletonList(deadCode), Collections.emptyList(), Collections.singletonList(statement),
				Collections.emptyList());
		
		discoveryCore.importContributorResults(context,
				Arrays.asList(contributorResult1, contributorResult2, contributorResult3));
		final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(projectId).withNames(Arrays.asList("subModule2", "subModule3")));
		assertEquals(2, modules.size());
		final var rootModule = moduleService.findModules(b -> b.ofProject(projectId).withName("rootModule1"));
		final var edge1 = moduleService.findRelationship(q -> q.ofSource(rootModule.get(0).identity())
															 .ofDestination(modules.get(0).identity())
															 .withType(RelationshipType.CONTAINS));
		assertSame(1, edge1.size());
		
		final var edge2 = moduleService.findRelationship(q -> q.ofSource(rootModule.get(0).identity())
															 .ofDestination(modules.get(1).identity())
															 .withType(RelationshipType.CONTAINS));
		assertSame(1, edge2.size());
	}
	
	@Test
	void testImportContributorResultForUpdate() {
		final EntityId id = createTestModule("rootModuleUpdate1", "/cobol/ROOTM.pgm", ModuleType.COBOL_PROGRAM);
		final var moduleDefinition1 = newModuleDefinition("rootModuleUpdated2", ModuleType.ASSEMBLER_PROGRAM, null,
				null, Storage.FILE, Representation.PHYSICAL,
				Collections.emptyList(), Identification.IDENTIFIED, Origin.CUSTOM);
		final ContributorResult contributorResult1 = new ContributorResult(ContributorResult.Type.ROOT_MODULE,
				new ModuleFilter().setNames("rootModuleUpdate1"),
				Collections.emptySet(), moduleDefinition1, Collections.singletonList(error),
				Collections.singletonList(deadCode), Collections.emptyList(), Collections.singletonList(statement),
				Collections.emptyList());
				
		discoveryCore.importContributorResults(context, Arrays.asList(contributorResult1));
		final ModulePojo module = moduleService.getModule(id);
		assertEquals(ModuleType.ASSEMBLER_PROGRAM.getTechnology(), module.getTechnology());
		assertEquals("rootModuleUpdated2", module.getName());
	}
	
	@Test
	void testImportContributorResultForPartialFailure() {
		final ContributorResult contributorResult1 = new ContributorResult(ContributorResult.Type.EXTERNAL_MODULE,
				new ModuleFilter().setNames("foo"),
				Collections.singleton(ResolutionFlag.RESOLVE_CASE_INSENSITIVE), moduleDefinition, Collections.singletonList(error),
				Collections.singletonList(deadCode), Collections.emptyList(), Collections.singletonList(statement),
				Collections.emptyList());
		final ImportResult<EntityId> dbErrorImportResult = ImportResult.forDbError("Database error occured", new Throwable());
		final ImportResult<UUID> dbErrorImportResult2 = ImportResult.forDbError("Database error occured", new Throwable());
		final ImportResult<EntityId> successImportResult = ImportResult.forSuccessfulCreation(EntityId.of(0l));
		when(discoveryPersistence.persistDeadCode(any(), any(), anyList()))
				.thenReturn(Arrays.asList(dbErrorImportResult, successImportResult, dbErrorImportResult));
		when(discoveryPersistence.persistErrors(any(), any(), anyList())).thenReturn(Arrays.asList(successImportResult, dbErrorImportResult));
		when(discoveryPersistence.persistStatements(any(), any(), any(), anyList())).thenReturn(Arrays.asList(dbErrorImportResult, successImportResult));
		when(discoveryPersistence.persistDependencyDefinitions(any(), any(), anyList()))
				.thenReturn(Arrays.asList(dbErrorImportResult2, dbErrorImportResult2));
		when(discoveryPersistence.findModules(context, contributorResult1.getModuleFilter())).thenReturn(Arrays.asList(EntityId.of(1L), EntityId.of(2L)));
		final var results = List.of(contributorResult1, contributorResultForSub);
		final IllegalStateException exception = assertThrows(IllegalStateException.class, () -> 
			discoveryCore.importContributorResults(context, results)
		);
		assertTrue(exception.getMessage()
				.contains("1 module(s) 1 statement(s) 1 error(s) 2 deadCode(s) 2 dependencyDefinition(s) is/are unable to be stored due to an error"));
	}
	
	@Test
	void testImportContributorResultForAnchorType() {
		final String name = "anchorModule";
		final var moduleDefinition = newModuleDefinition(name, ModuleType.ASSEMBLER_PROGRAM, null,
				null, Storage.FILE_SECTION, Representation.VIRTUAL,
				Collections.emptyList(), Identification.IDENTIFIED, Origin.CUSTOM);
		final ContributorResult contributorResult1 = new ContributorResult(ContributorResult.Type.ANCHOR_TO,
				new ModuleFilter().setNames(name).setContainedIn(new ModuleFilter().setModuleIds(EntityId.of(0l))),
				Collections.emptySet(), moduleDefinition, Collections.emptyList(),
				Collections.singletonList(deadCode), Collections.emptyList(), Collections.emptyList(), Collections.emptyList());
		discoveryCore.importContributorResults(context, Arrays.asList(contributorResult1));
		/* check that anchored module is not stored in DB */
		final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(projectId).withName(name));
		assertEquals(0, modules.size());
		final Supplier<Set<ContributorResult>> getContributorResults = () -> temporaryStorage
				.getAndRemoveUnanchoredContributorResults(assertNotNull(context.getJobId()))
				.stream().map(ContributorResult.class::cast).collect(Collectors.toSet());
		/* test that anchored result is stored in discoveryCache */
		final Set<ContributorResult> contributorResults1 = getContributorResults.get();
		assertEquals(1, contributorResults1.size());
		final ContributorResult actualResult = contributorResults1.iterator().next();
		assertEquals(1, actualResult.getDeadCodes().size());
		assertEquals(name, actualResult.getModuleDefinition().name.get());
		assertEquals(ModuleType.ASSEMBLER_PROGRAM.getTechnology(), actualResult.getModuleDefinition().technology.get());
		assertEquals(ModuleType.ASSEMBLER_PROGRAM.getType(), actualResult.getModuleDefinition().type.get());
		assertEquals(name, actualResult.getModuleFilter().getNames().iterator().next());
		assertEquals(EntityId.of(0l), actualResult.getModuleFilter().getContainedIn().get().getModuleIds().iterator().next());
		final Set<ContributorResult> contributorResults2 = getContributorResults.get();
		assertEquals(0, contributorResults2.size());
		discoveryCache.clearDiscoveryJobCache(context.getJobId());
	}
	
	@Test
	void testImportContributorResultWithDeferredAction() {
		discoveryCache.clearDiscoveryJobCache(context.getJobId());
		final var moduleDefinition1 = newModuleDefinition("deferredModule1", ModuleType.ASSEMBLER_PROGRAM, null,
				null, Storage.FILE_SECTION, Representation.VIRTUAL,
				Collections.emptyList(), Identification.IDENTIFIED, Origin.CUSTOM);
		final var moduleDefinition2 = newModuleDefinition("deferredModule2", ModuleType.ASSEMBLER_PROGRAM, null,
				null, Storage.FILE_SECTION, Representation.VIRTUAL,
				Collections.emptyList(), Identification.IDENTIFIED, Origin.CUSTOM);
		final DeferredActionDefinition actionDefinition1 = new DeferredActionDefinition("CContributor", "process" );
		final DeferredActionDefinition actionDefinition2 = new DeferredActionDefinition("JavaContributor", "process2" );
		final ContributorResult contributorResult1 = new ContributorResult(ContributorResult.Type.SUB_MODULE,
				new ModuleFilter().setNames("deferredModule1"),
				Collections.emptySet(), moduleDefinition1, Collections.emptyList(),
				Collections.emptyList(), Arrays.asList(actionDefinition1, actionDefinition2), Collections.emptyList(),
				Collections.emptyList());
		final ContributorResult contributorResult2 = new ContributorResult(ContributorResult.Type.SUB_MODULE,
				new ModuleFilter().setNames("deferredModule2"),
				Collections.emptySet(), moduleDefinition2, Collections.emptyList(),
				Collections.emptyList(), Arrays.asList(actionDefinition1), Collections.emptyList(),
				Collections.emptyList());
		discoveryCore.importContributorResults(context, Arrays.asList(contributorResult1, contributorResult2));
		final List<EntityId> modules = temporaryStorage.getAndRemoveModulesWithDeferredActions(context.getJobId()).stream()
				.sorted((o1, o2) -> o1.getNid().compareTo(o2.getNid())).collect(Collectors.toList());
		assertEquals(2, modules.size());
		final List<DeferredActionDefinition> actionsForModule1 = temporaryStorage.getAndRemoveDeferredActions(context.getJobId(),
				modules.get(0))
				.stream().map(DeferredActionDefinition.class::cast).sorted(Comparator.comparing(DeferredActionDefinition::getContributorClassName))
				.collect(Collectors.toList());
		assertEquals(2, actionsForModule1.size());
		assertEquals(actionDefinition1.getContributorClassName(), actionsForModule1.get(0).getContributorClassName());
		assertEquals(actionDefinition1.getName(), actionsForModule1.get(0).getName());
		assertEquals(actionDefinition2.getContributorClassName(), actionsForModule1.get(1).getContributorClassName());
		assertEquals(actionDefinition2.getName(), actionsForModule1.get(1).getName());
		final List<DeferredActionDefinition> actionsForModule2 = temporaryStorage.getAndRemoveDeferredActions(context.getJobId(),
				modules.get(1))
				.stream().map(DeferredActionDefinition.class::cast).collect(Collectors.toList());
		assertEquals(1, actionsForModule2.size());
		assertEquals(actionDefinition1.getContributorClassName(), actionsForModule2.get(0).getContributorClassName());
		assertEquals(actionDefinition1.getName(), actionsForModule2.get(0).getName());
		discoveryCache.clearDiscoveryJobCache(context.getJobId());
	}
	
	@Test
	void testImportContributorResultWithDeferredActionForAnchoredType() {
		final var moduleDefinition = newModuleDefinition("deferredModule", ModuleType.ASSEMBLER_PROGRAM, null,
				null, Storage.FILE_SECTION, Representation.VIRTUAL,
				Collections.emptyList(), Identification.IDENTIFIED, Origin.CUSTOM);
		final DeferredActionDefinition actionDefinition = new DeferredActionDefinition("CContributor", "process" );
		final ContributorResult contributorResult1 = new ContributorResult(ContributorResult.Type.ANCHOR_TO,
				new ModuleFilter().setNames("deferredModule"),
				Collections.emptySet(), moduleDefinition, Collections.emptyList(),
				Collections.emptyList(), Collections.singletonList(actionDefinition), Collections.emptyList(),
				Collections.emptyList());
		discoveryCore.importContributorResults(context, Arrays.asList(contributorResult1));
		/* check that no deferred action is stored for anchored type */
		final Collection<EntityId> modules = temporaryStorage.getAndRemoveModulesWithDeferredActions(context.getJobId());
		assertEquals(0, modules.size());
		discoveryCache.clearDiscoveryJobCache(context.getJobId());
	}
	
	@Test
	void testImportContributorResultFordependencyDefinition() {
		final String rootModuleName = "rootModuleForDependencyDefination";
		final String subModuleName = "subModuleForDependencyDefination";
		final var moduleDefinition1 = newModuleDefinition(rootModuleName, ModuleType.ASSEMBLER_PROGRAM, null, "src/rootModule.asm", Storage.FILE,
				Representation.PHYSICAL, Collections.emptyList(), Identification.IDENTIFIED, Origin.CUSTOM);
		final var moduleDefinition2 = newModuleDefinition(subModuleName, ModuleType.ASSEMBLER_PROGRAM, null, null, Storage.FILE_SECTION,
				Representation.VIRTUAL, Collections.emptyList(), Identification.IDENTIFIED, Origin.CUSTOM);
		final Map<String, Object> attributes = new HashMap<>();
		attributes.put(ModelAttributeKey.DB_ACCESS_TYPE.name(), Arrays.asList(DatabaseAccessType.READ, DatabaseAccessType.DELETE));
		attributes.put(ModelAttributeKey.FILE_ACCESS_TYPE.name(), ModelAttributeValue.FileAccess.READ);
		final ModuleFilter containedModule = new ModuleFilter()
				.setNames("Foo 1", "Foo 2")
				.setTypes(ModuleType.COBOL_PROGRAM, ModuleType.COBOL_COPYBOOK);
		final ModuleFilter moduleFilter = new ModuleFilter()
				.setNames("Test 1")
				.setTypes(ModuleType.UNKNOWN)
				.setPaths("/src/cobol/programs/Test1.cbl")
				.setModuleIds(dependencyId3)
				.setContainedIn(containedModule);
		final var dependencyDefinitionRootModule = newDependencyDefinition(moduleFilter,
				Sets.newHashSet(ResolutionFlag.RESOLVE_CASE_INSENSITIVE, ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY),
				new ModuleLocation(10, 100), RelationshipType.INCLUDES, Binding.EARLY, attributes);
		ContributorResult contributorResult1 = new ContributorResult(ContributorResult.Type.ROOT_MODULE, new ModuleFilter().setNames(rootModuleName),
				Collections.emptySet(), moduleDefinition1, Collections.emptyList(), Collections.emptyList(), Collections.emptyList(), Collections.emptyList(),
				Collections.singletonList(dependencyDefinitionRootModule));
		final var dependencyDefinitionSubModule1 = newDependencyDefinition(new ModuleFilter(), Collections.emptySet(), null,
				RelationshipType.NONE, Binding.UNKNOWN, null);
		final var dependencyDefinitionSubModule2 = newDependencyDefinition(moduleFilter, Collections.emptySet(), null,
				RelationshipType.ACCESSES, Binding.LATE, null);
		ContributorResult contributorResult2 = new ContributorResult(ContributorResult.Type.SUB_MODULE, new ModuleFilter().setNames(subModuleName),
				Collections.emptySet(), moduleDefinition2, Collections.emptyList(), Collections.emptyList(), Collections.emptyList(), Collections.emptyList(),
				Arrays.asList(dependencyDefinitionSubModule1, dependencyDefinitionSubModule2));
		discoveryCore.importContributorResults(context, Arrays.asList(contributorResult1, contributorResult2));
		final Map<String, EntityId> modulesIdAndName = moduleService.findModules(b -> b.ofProject(projectId).withNames(Arrays.asList(rootModuleName, subModuleName))).stream()
				.collect(Collectors.toMap(ModulePojo::getName, ModulePojo::identity));
		assertEquals(2, modulesIdAndName.size());
		final Function<EntityId, Integer> actualdependencyDefinationsListSize = (moduleId) -> Integer
				.valueOf(discoveryPersistence.fetchUnresolvedDependencyDefinitions(moduleId).size());
		assertEquals(1, actualdependencyDefinationsListSize.apply(modulesIdAndName.get(rootModuleName)));
		assertEquals(2, actualdependencyDefinationsListSize.apply(modulesIdAndName.get(subModuleName)));
	}
	
	@Test
	void testResolveDependencyWithResolveAny() {
		final ModuleFilter moduleFilter = new ModuleFilter().setModuleIds(EntityId.of(-1l));
		final DependencyDefinitionPojo definition = new DependencyDefinitionPojo(
				UUID.randomUUID(),
				EntityId.of(1l), null, null,
				Collections.emptyMap(),
				Binding.LATE,
				null,
				List.of(moduleFilter),
				RelationshipType.ACCESSES,
				Sets.newHashSet(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY),
				false,
				null);
		assertThrows(IllegalArgumentException.class, () -> discoveryCore.resolveDependency(context, EntityId.of(2L), definition));
	}
	
	@Test 
	void testResolveDependencyWithExclusiveFlags() {
		final ModuleFilter moduleFilter = new ModuleFilter().setModuleIds(EntityId.of(-1l));
		final DependencyDefinitionPojo definition = new DependencyDefinitionPojo(
				UUID.randomUUID(),
				EntityId.of(1l), null, null,
				Collections.emptyMap(),
				Binding.LATE,
				null,
				List.of(moduleFilter),
				RelationshipType.ACCESSES,
				Sets.newHashSet(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL, ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ERROR),
				false,
				null);
		assertThrows(IllegalArgumentException.class, () -> discoveryCore.resolveDependency(context, EntityId.of(2L), definition));
	}

	@Test
	void testResolveDependencyMultipleTargetsError() {
		final EntityId id1 = createTestModule("Cobol1", "/cobol/programs/cobol1.pgm", ModuleType.COBOL_PROGRAM);
		/* this moduleFilter will give two Modules that are created in #initialize */
		final ModuleFilter moduleFilter = new ModuleFilter().setNames("foo");
		final var def = newDependencyDefinition(moduleFilter, Sets.newHashSet(ResolutionFlag.RESOLVE_CASE_INSENSITIVE),
				null, RelationshipType.ACCESSES, Binding.LATE, null);
		final DependencyDefinitionPojo definition = createDependencyDefinition(id1, def);

		discoveryCore.resolveDependency(context, id1, definition);
		final List<ErrorMarkerPojo> errors = moduleService.findErrorMarkers(q -> q.ofProject(projectId).ofModule(id1));
		assertEquals(1, errors.size());
		assertEqualsError(errors.get(0), ErrorKey.UNDISCOVERED_DEPENDENCY, Severity.ERROR, "Multiple possible candidates found:");
	}

	@Test
	void testResolveDependencyForDbError() {
		final EntityId id1 = createTestModule("MICROPROGRAM", "/cobol/programs/microprogram.pgm", ModuleType.COBOL_PROGRAM);
		final ModuleFilter moduleFilter = new ModuleFilter().setNames("FOO");
		final var def = newDependencyDefinition(moduleFilter, Sets.newHashSet(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL),
				new ModuleLocation(1, 1), RelationshipType.ACCESSES, Binding.LATE, null);
		doReturn(ImportResult.forDbError("Dependency error", new SQLException("Db error"))).
				when(discoveryPersistence).createDependency(any(), any(), any(), any(), any(), any(), any(), anyMap(), any());
		final DependencyDefinitionPojo definition = createDependencyDefinition(id1, def);
		discoveryCore.resolveDependency(context, id1, definition);
		final List<ErrorMarkerPojo> errors = moduleService.findErrorMarkers(q -> q.ofProject(projectId).ofModule(id1));

		final var errorInfos = errors.stream().map(e -> List.of(e.getKey(), e.getSeverity(), e.getCause()))
				.collect(Collectors.toList());

		assertThat(errorInfos, containsInAnyOrder(List.of(
				ErrorKey.DEPENDENCY_RESOLUTION_ERROR,
				Severity.ERROR,
				"Failed to create dependency from moduleId : " + id1 +  " to target moduleId: " + dependencyIdForUpperCase +
						". Encountered DB_ERROR : Dependency " + "error , java.sql.SQLException: Db error"
		)));
	}
	
	@Test 
	void testResolveDependencyMultipleTargetsErrorNoFlags() {
		final EntityId id1 = createTestModule("Cobol20", "/cobol/programs/cobol20.pgm", ModuleType.COBOL_PROGRAM);
		createTestModule("dependency", "/cobol/programs/dependency1.pgm", ModuleType.COBOL_PROGRAM);
		createTestModule("dependency", "/cobol/programs/dependency2.pgm", ModuleType.COBOL_PROGRAM);
		/* this moduleFilter will give two Modules that are created in #initialize */
		final ModuleFilter moduleFilter = new ModuleFilter().setNames("dependency");
		final var def = newDependencyDefinition(moduleFilter, Collections.emptySet(),
				null, RelationshipType.ACCESSES, Binding.LATE, null);
		final DependencyDefinitionPojo definition = createDependencyDefinition(id1, def);
		discoveryCore.resolveDependency(context, id1, definition);
		final List<ErrorMarkerPojo> errors = moduleService.findErrorMarkers(q -> q.ofProject(projectId).ofModule(id1));
		assertEquals(1, errors.size());
		assertEqualsError(errors.get(0), ErrorKey.UNDISCOVERED_DEPENDENCY, Severity.ERROR, "Multiple possible candidates found:");
	}
	
	@Test 
	void testResolveDependencyMultipleMatchResolveError() {
		final EntityId id1 = createTestModule("Cobol19", "/cobol/programs/cobol19.pgm", ModuleType.COBOL_PROGRAM);
		createTestModule("dependency", "/cobol/programs/dependency3.pgm", ModuleType.COBOL_PROGRAM);
		createTestModule("dependency", "/cobol/programs/dependency4.pgm", ModuleType.COBOL_PROGRAM);
		/* this moduleFilter will give two Modules that are created in #initialize */
		final ModuleFilter moduleFilter = new ModuleFilter().setNames("dependency");
		final var def = newDependencyDefinition(moduleFilter, Sets.newHashSet(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ERROR),
				null, RelationshipType.ACCESSES, Binding.LATE, null);
		final DependencyDefinitionPojo definition = createDependencyDefinition(id1, def);
		discoveryCore.resolveDependency(context, id1, definition);
		final List<ErrorMarkerPojo> errors = moduleService.findErrorMarkers(q -> q.ofProject(projectId).ofModule(id1));
		assertEquals(1, errors.size());
		assertEqualsError(errors.get(0), ErrorKey.UNDISCOVERED_DEPENDENCY, Severity.ERROR, "Multiple possible candidates found:");
	}

	@Test
	void testResolveDependencyWithMultipleModuleFilter() {
		final EntityId id1 = createTestModule("Cobol221", "/cobol/programs/cobol221.pgm", ModuleType.COBOL_PROGRAM);
		createTestModule("dep", "/cobol/programs/dependency10.pgm", ModuleType.COBOL_PROGRAM);
		createTestModule("dep", "/cobol/programs/dependency11.pgm", ModuleType.COBOL_PROGRAM);
		final ModuleFilter moduleFilter = new ModuleFilter().setNames("dep");
		final ModuleFilter moduleFilter2 = new ModuleFilter().setPaths("/cobol/programs/dependency11.pgm");
		final var def = newDependencyDefinition(List.of(moduleFilter, moduleFilter2),
				Sets.newHashSet(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ERROR), null, RelationshipType.ACCESSES, Binding.LATE, null);
		final DependencyDefinitionPojo definition = createDependencyDefinition(id1, def);
		discoveryCore.resolveDependency(context, id1, definition);
		final List<ErrorMarkerPojo> errors = moduleService.findErrorMarkers(q -> q.ofProject(projectId).ofModule(id1));
		assertEquals(0, errors.size());
		final List<ModuleRelationshipPojo> references = moduleService.findRelationship(q -> q.ofSource(id1).withType(RelationshipType.ACCESSES));
		assertEquals(1, references.size());
	}
	
	@Test 
	void testResolveDependencyEmptyTargetPatternError() {
		final EntityId id1 = createTestModule("Cobol2", "/cobol/programs/cobol2.pgm", ModuleType.COBOL_PROGRAM);
		/* Creating one more module to force resolve dependency to use search orders */
		createTestModule("Cobol2", "/cobol/programs/cobol020.pgm", ModuleType.COBOL_PROGRAM);
		final ModuleFilter moduleFilter = new ModuleFilter().setNames("Cobol2").setPathPatterns("**/*.pgm");
		final DependencyDefinitionPojo definition = new DependencyDefinitionPojo(UUID.randomUUID(),
				id1, null, null,
				Collections.emptyMap(),
				Binding.LATE,
				null,
				List.of(moduleFilter),
				RelationshipType.ACCESSES,
				Collections.emptySet(),
				false,
				null);

		final DiscoveryContext spyContext = Mockito.spy(context);
		final SearchOrders searchOrders = Mockito.mock(SearchOrders.class);
		when(assertNotNull(spyContext).getSearchOrders()).thenReturn(searchOrders);
		discoveryCore.resolveDependency(spyContext, id1, definition);
		final List<ModuleRelationshipPojo> references = moduleService.findRelationship(q -> q.ofProject(projectId)
																							.ofModuleInDirection(id1, RelationshipDirection.BOTH)
																							.withType(RelationshipType.ACCESSES)
																							.distinct(RelationshipField.SOURCE, RelationshipField.DESTINATION));
		assertTrue(references.isEmpty());
		final List<ErrorMarkerPojo> errors = moduleService.findErrorMarkers(q -> q.ofProject(projectId).ofModule(id1));
		assertEquals(1, errors.size());
		assertEqualsError(errors.get(0), ErrorKey.DEPENDENCY_RESOLUTION_ERROR, Severity.ERROR, "No SearchOrder matches for contextPath");
	}
	
	@Test 
	void testResolveDependencyTargetPatternFastPath() {
		discoveryCache.clearDiscoveryJobCache(context.getJobId());
		final EntityId id1 = createTestModule("TABLE1", null, ModuleType.SQL_TABLE);
		final ModulePojo rid1 = moduleService.getModule(id1);
		final ModulePojo rid2 = moduleService.getModule(dependencyIdForUpperCase);
		final ModuleLocation moduleLocation = new ModuleLocation(0, 0);
		/* Create the edge, so the context path won't be null */
		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setSrcModule(rid2.identity())
				.setSrcLocation(moduleLocation)
				.setDstModule(rid1.identity())
				.setRelationship(RelationshipType.CONTAINS));
		final ModuleFilter moduleFilter = new ModuleFilter().setNames("TABLE1");
		final var def = newDependencyDefinition(moduleFilter, Collections.emptySet(),
				null, RelationshipType.ACCESSES, Binding.LATE, null);
		final DependencyDefinitionPojo definition = createDependencyDefinition(id1, def);
		discoveryCore.resolveDependency(context, dependencyId3, definition);

		/* Assert that the dependencyDefinition is marked as true by checking the count of unresolved dependencies */
		final List<DependencyDefinitionPojo> unresolvedDependencies = discoveryPersistence.fetchUnresolvedDependencyDefinitions(id1);
		assertTrue(unresolvedDependencies.isEmpty());
		final List<ModuleRelationshipPojo> references = moduleService.findRelationship(q -> q.ofSource(dependencyId3).withType(RelationshipType.ACCESSES));
		assertEquals(1, references.size());
		assertEqualsReference(references.get(0), dependencyId3, id1, null, RelationshipType.ACCESSES, Collections.emptyMap());
	}

	@Test
	void testHandleUnresolvedDependencyForDbError() {
		final EntityId id1 = createTestModule("RootModuleForMissing", "/cobol/programs/rootMissing.pgm", ModuleType.COBOL_PROGRAM);
		final ModuleFilter moduleFilter = new ModuleFilter().setNames("FOO");
		final DependencyDefinitionPojo definition = new DependencyDefinitionPojo(UUID.randomUUID(),
				id1, null, null,
				Collections.emptyMap(),
				Binding.LATE,
				new ModuleLocation(1, 1),
				List.of(moduleFilter),
				RelationshipType.ACCESSES,
				Sets.newHashSet(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY),
				false,
				null);
		doReturn(ImportResult.forDbError("Dependency error", new SQLException("Db error"))).
				when(discoveryPersistence).createDependency(any(), any(), any(), any(), any(), any(), any(), anyMap(), any());
		assertThrows(IllegalStateException.class, () -> discoveryCore.handleUnresolvedDependencies(context, id1, definition));

		final List<ErrorMarkerPojo> errors = moduleService.findErrorMarkers(q -> q.ofProject(projectId).ofModule(id1));
		assertEquals(1, errors.size());
		final String expectedMessage = "Failed to create MISSING dependency from moduleId : " + id1;
		assertEqualsError(errors.get(0), ErrorKey.DEPENDENCY_RESOLUTION_ERROR, Severity.ERROR, expectedMessage);
	}

	@Test
	void testhandleUnresolvedDependenciesNoTargetsFound() {
		final EntityId id1 = createTestModule("Cobol7", "/cobol/programs/cobol7.pgm", ModuleType.COBOL_PROGRAM);
		final ModuleFilter moduleFilter = new ModuleFilter().setModuleIds(EntityId.of(-1l));
		final DependencyDefinitionPojo definition = new DependencyDefinitionPojo(UUID.randomUUID(),
				id1, null, null,
				Collections.emptyMap(),
				Binding.LATE,
				null,
				List.of(moduleFilter),
				RelationshipType.ACCESSES,
				Collections.emptySet(),
				false,
				null);
		discoveryCore.handleUnresolvedDependencies(context, id1, definition);
		
		final List<ErrorMarkerPojo> errors = moduleService.findErrorMarkers(q -> q.ofProject(projectId).ofModule(id1));
		assertEquals(1, errors.size());
		assertEqualsError(errors.get(0), ErrorKey.UNDISCOVERED_DEPENDENCY, Severity.WARNING, "No target modules found");
		
		/* check that no dependencies have been created */
		final List<ModuleRelationshipPojo> references = moduleService.findRelationship(q -> q.ofProject(projectId)
																		.ofModuleInDirection(id1, RelationshipDirection.BOTH)
																		.withType(RelationshipType.ACCESSES)
																		.distinct(RelationshipField.SOURCE, RelationshipField.DESTINATION));
		assertEquals(0, references.size());
	}
	
	@Test
	void testResolveDependencyForNoErrors() {
		final EntityId id1 = createTestModule("Cobol18", "/cobol/programs/cobol18.pgm", ModuleType.COBOL_PROGRAM);
		final ModuleFilter moduleFilter = new ModuleFilter().setNames("dep18");
		final var def = newDependencyDefinition(moduleFilter, Sets.newHashSet(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL),
				null, RelationshipType.ACCESSES, Binding.LATE, null);
		final DependencyDefinitionPojo definition = createDependencyDefinition(id1, def);
		discoveryCore.resolveDependency(context, id1, definition);
		discoveryCore.handleUnresolvedDependencies(context, id1, definition);
		/* check that no errors have been created */
		final List<ErrorMarkerPojo> errors = moduleService.findErrorMarkers(q -> q.ofProject(projectId).ofModule(id1));
		assertEquals(0, errors.size());
		/* check that no dependencies have been created */
		final List<ModuleRelationshipPojo> references = moduleService.findRelationship(q -> q.ofProject(projectId)
																		.ofModuleInDirection(id1, RelationshipDirection.BOTH)
																		.withType(RelationshipType.ACCESSES)
																		.distinct(RelationshipField.SOURCE, RelationshipField.DESTINATION));
		assertEquals(0, references.size());
		final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(projectId).withName("dep18"));
		assertEquals(0, modules.size());
	}

	private DependencyDefinitionPojo createDependencyDefinition(final EntityId module, final DependencyDefinitionPojoPrototype definition) {
		final List<ImportResult<UUID>> importResults = discoveryPersistence.persistDependencyDefinitions(context, module, Collections.singletonList(definition));
		assertEquals(1, importResults.size());
		assertTrue(importResults.get(0).isSuccess());

		final UUID id = importResults.get(0).getKey().orElseThrow(() -> new IllegalStateException("persistDependencyDefinitions failed"));
		final List<DependencyDefinitionPojo> dependencyDefinitions = moduleService.findDependencyDefinitions(q -> q.byId(id));
		assertEquals(1, dependencyDefinitions.size(), "DependencyDefinition must exists for id: " + id);

		return dependencyDefinitions.get(0);
	}

	@Test 
	void testResolveDependencyWithOneModule() {
		discoveryCache.clearDiscoveryJobCache(context.getJobId());
		final EntityId id1 = createTestModule("Cobol3", "/cobol/programs/cobol3.pgm", ModuleType.COBOL_PROGRAM);
		final ModuleFilter moduleFilter = new ModuleFilter().setModuleIds(dependencyId3);
		final Map<String, Object> attributes = new HashMap<>();
		attributes.put(ModelAttributeKey.SEND_RECEIVE_ACCESS_TYPE.name(), ModelAttributeValue.SendReceiveAccess.SEND);
		final ModuleLocation location = new ModuleLocation(5, 10);
		final var definition = newDependencyDefinition(moduleFilter, Sets.newHashSet(ResolutionFlag.RESOLVE_CASE_INSENSITIVE),
				location, RelationshipType.ACCESSES, Binding.LATE, attributes);
		/* persist to the dependency definition to database, so that we can check that it is resolved */
		final DependencyDefinitionPojo def = createDependencyDefinition(id1, definition);
		discoveryCore.resolveDependency(context, id1, def);
		final List<ModuleRelationshipPojo> references = moduleService.findRelationship(q -> q.ofSource(id1).withType(RelationshipType.ACCESSES));
		assertEquals(1, references.size());
		assertEqualsReference(references.get(0), id1, dependencyId3, location, RelationshipType.ACCESSES, attributes);
		/* check that the dependency definition has been resolved */
		final List<DependencyDefinitionPojo> definitions = discoveryPersistence.fetchUnresolvedDependencyDefinitions(id1);
		assertEquals(0, definitions.size());
	}

	@Test
	void testResolveDependencyWithConditionalDependency() {
		discoveryCache.clearDiscoveryJobCache(context.getJobId());
		final var id1 = createTestModule("Cobol33", "/cobol/programs/cobol33.pgm", ModuleType.COBOL_PROGRAM);
		final ModuleFilter moduleFilter = new ModuleFilter().setModuleIds(dependencyId3);
		final Map<String, Object> attributes = Collections.singletonMap(ModelAttributeKey.SEND_RECEIVE_ACCESS_TYPE.name(),
				ModelAttributeValue.SendReceiveAccess.SEND);
		final ModuleLocation location = new ModuleLocation(5, 10);
		
		final DependencyDefinitionPojoPrototype definition = new DependencyDefinitionPojoPrototype()
				.setModuleFilters(List.of(moduleFilter))
				.setResolutionFlags(Sets.newHashSet(ResolutionFlag.RESOLVE_CASE_INSENSITIVE))
				.setRelationshipType(RelationshipType.ACCESSES)
				.setBindingType(Binding.LATE)
				.setAttributes(attributes)
				.setLocation(location)
				.setReachedFromModules(List.of(new ModuleFilter().setModuleIds(dependencyId3),new ModuleFilter().setModuleIds(dependencyIdForUpperCase)));

		/* persist to the dependency definition to database, so that we can check that it is resolved */
		List<ImportResult<UUID>> importResults = discoveryPersistence.persistDependencyDefinitions(context, id1, Collections.singletonList(definition));
		assertEquals(1, importResults.size());
		assertTrue(importResults.get(0).isSuccess());
		final var dependencyId = importResults.get(0).getKey();
		assertTrue(dependencyId.isPresent());
		final List<DependencyDefinitionPojo> depDefs = moduleService.findDependencyDefinitions(q -> q.byId(dependencyId.get()));
		assertEquals(1, depDefs.size());
		discoveryCore.resolveDependency(context, id1, depDefs.get(0));
		final var references = moduleService.findRelationship(q -> q.ofSource(id1).withType(RelationshipType.ACCESSES));
		
		assertEquals(1, references.size());
		assertEqualsReference(references.get(0), id1, dependencyId3, location, RelationshipType.ACCESSES, attributes);
		final var actualConditionalDependencies = assertNotNull(references.get(0).getValidIfReachedFrom());
		assertEquals(2, actualConditionalDependencies.size());
		/* check that the dependency definition has been resolved */
		final List<DependencyDefinitionPojo> definitions = discoveryPersistence.fetchUnresolvedDependencyDefinitions(id1);
		assertEquals(0, definitions.size());
	}
	
	@Test 
	void testResolveDependencyWithMultipleModules() {
		final EntityId id1 = createTestModule("Cobol5", "/cobol/programs/cobol5.pgm", ModuleType.COBOL_PROGRAM);
		final ModuleFilter moduleFilter = new ModuleFilter().setPathPatterns("/cobol/programs/*").setNames("foo", "dependencyModule");
		final Map<String, Object> attributes = new HashMap<>();
		attributes.put(ModelAttributeKey.FILE_ACCESS_OPERATION.name(), ModelAttributeValue.FileAccess.WRITE);
		attributes.put(ModelAttributeKey.SEND_RECEIVE_ACCESS_TYPE.name(), ModelAttributeValue.SendReceiveAccess.SEND);
		final ModuleLocation location = new ModuleLocation(5, 10);
		final var definition = newDependencyDefinition(moduleFilter, Sets.newHashSet(ResolutionFlag.RESOLVE_CASE_INSENSITIVE, 
				ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL), location, RelationshipType.ACCESSES, Binding.LATE, attributes);
		/* persist to the dependency definition to database, so that we can check that it stays unresolved */
		final DependencyDefinitionPojo def = createDependencyDefinition(id1, definition);
		discoveryCore.resolveDependency(context, id1, def);
		final Map<UUID, ModuleRelationshipPojo> references = moduleService.findRelationship(q -> q.ofSource(id1).withType(RelationshipType.ACCESSES)).stream()
																			.collect(Collectors.toMap(ModuleRelationshipPojo::getDstModule, r -> r));
		assertEquals(3, references.size());
		assertEqualsReference(Objects.requireNonNull(references.get(dependencyIdForUpperCase.getUid()), "dependencyIdForUpperCase"),
				id1, dependencyIdForUpperCase, location, RelationshipType.ACCESSES, attributes);
		assertEqualsReference(Objects.requireNonNull(references.get(dependencyIdForLowerCase.getUid()), "dependencyIdForLowerCase"),
				id1, dependencyIdForLowerCase, location, RelationshipType.ACCESSES, attributes);
		assertEqualsReference(Objects.requireNonNull(references.get(dependencyId3.getUid()), "dependencyId3"),
				id1, dependencyId3, location, RelationshipType.ACCESSES, attributes);

		/* For MULTIPLE_MATCH_RESOLVE_ALL, the dependency will stay unresolved */
		final List<DependencyDefinitionPojo> definitions = discoveryPersistence.fetchUnresolvedDependencyDefinitions(id1);
		assertEquals(1, definitions.size());
	}
	
	@Test 
	void testResolveDependencyForPathPatterns() {
		final EntityId id1 = createTestModule("CobolPath", "/cobol/program/cobolPath.pgm", ModuleType.COBOL_PROGRAM);
		final EntityId id2 = createTestModule("foo12", "/extFolder/Ecopies/foo12.cpy", ModuleType.COBOL_COPYBOOK);
		final ModuleFilter moduleFilter = new ModuleFilter().setNames("foo12").setTypes(ModuleType.COBOL_COPYBOOK);
		final ModuleLocation location = new ModuleLocation(5, 10);
		final var def = newDependencyDefinition(moduleFilter, Sets.newHashSet(ResolutionFlag.RESOLVE_CASE_INSENSITIVE,
				ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ERROR), location, RelationshipType.ACCESSES, Binding.LATE, null);
		final DependencyDefinitionPojo definition = createDependencyDefinition(id1, def);
		final DiscoveryContext spyContext = Mockito.spy(context);
		final SearchOrders searchOrders = Mockito.mock(SearchOrders.class);
		when(assertNotNull(spyContext).getSearchOrders()).thenReturn(searchOrders);
		/* create our searchOrder object */
		final Source source = new Source("CobolPath", ModuleType.COBOL_PROGRAM.toString(), "/cobol/program/cobolPath.pgm", null, null);
		final Target target = new Target(null, ResolveTarget.COBOL_COPYBOOK.toString(), null, "/extFolder/Ecopies/*", null);
		final SearchOrder searchOrder1 = new SearchOrder(source, Collections.singletonList(target));
		final List<SearchOrder> searchOrderList = Arrays.asList(searchOrder1);
		when(assertNotNull(spyContext).getSearchOrders().getSearchOrdersList()).thenReturn(searchOrderList);
		discoveryCore.resolveDependency(assertNotNull(spyContext), id1, definition);
		final List<ModuleRelationshipPojo> references = moduleService.findRelationship(q -> q.ofSource(id1).withType(RelationshipType.ACCESSES));
		/* we have 3 dependencies available with name foo, but we should only have one dependency from copies from search order */
		assertEquals(1, references.size());
		assertEqualsReference(references.get(0), id1, id2, location, RelationshipType.ACCESSES, Collections.emptyMap());
	}
	
	@Test 
	void testResolveDependencyWithCaseSensitive() {
		final EntityId id1 = createTestModule("Cobol6", "/cobol/programs/cobol13.pgm", ModuleType.COBOL_PROGRAM);
		final ModuleFilter moduleFilter = new ModuleFilter().setNames("FOO");
		final var def = newDependencyDefinition(moduleFilter, Collections.emptySet(),
				null, RelationshipType.ACCESSES, Binding.LATE, null);
		final DependencyDefinitionPojo definition = createDependencyDefinition(id1, def);

		discoveryCore.resolveDependency(context, id1, definition);
		final List<ModuleRelationshipPojo> references = moduleService.findRelationship(q -> q.ofSource(id1).withType(RelationshipType.ACCESSES));
		/*Only one reference should be created with the module which exactly matches the given name in filter */
		assertEquals(1, references.size());
		assertEqualsReference(references.get(0), id1, dependencyIdForUpperCase, null, RelationshipType.ACCESSES, Collections.emptyMap());
	}
	
	@Test 
	void testResolveDependencyWithCaseInsensitive() {
		final EntityId id1 = createTestModule("Cobol6", "/cobol/programs/cobol6.pgm", ModuleType.COBOL_PROGRAM);
		final ModuleFilter moduleFilter = new ModuleFilter().setNames("FOO");
		final var def = newDependencyDefinition(moduleFilter, Sets.newHashSet(ResolutionFlag.RESOLVE_CASE_INSENSITIVE, 
				ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL), null, RelationshipType.ACCESSES, Binding.LATE, null);
		final DependencyDefinitionPojo definition = createDependencyDefinition(id1, def);

		discoveryCore.resolveDependency(context, id1, definition);
		final Map<UUID, ModuleRelationshipPojo> references = moduleService.findRelationship(q -> q.ofSource(id1).withType(RelationshipType.ACCESSES)).stream()
																.collect(Collectors.toMap(ModuleRelationshipPojo::getDstModule, r -> r));
		/*Only one reference should be created with the module which exactly matches the given name in filter */
		assertEquals(2, references.size());
		assertEqualsReference(Objects.requireNonNull(references.get(dependencyIdForUpperCase.getUid()), "dependencyIdForUpperCase"),
				id1, dependencyIdForUpperCase, null, RelationshipType.ACCESSES, Collections.emptyMap());
		assertEqualsReference(Objects.requireNonNull(references.get(dependencyIdForLowerCase.getUid()), "dependencyIdForLowerCase"),
				id1, dependencyIdForLowerCase, null, RelationshipType.ACCESSES, Collections.emptyMap());
	}

	@Test
	void testResolveDependencyToParentModule() {
		discoveryCache.clearDiscoveryJobCache(context.getJobId());
		final var id1 = createTestModule("TABLE1", null, ModuleType.SQL_TABLE);
		final var id2 = createTestModule("CBLPGM12", null, ModuleType.COBOL_PROGRAM);
		final ModuleRelationshipPojoPrototype containsModule = new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.CONTAINS)
				.setSrcModule(dependencyIdForLowerCase)
				.setSrcLocation(new ModuleLocation(0,0))
				.setDstModule(id1);
		moduleService.createRelationship(containsModule);
		
		final ModuleFilter moduleFilter = new ModuleFilter().setModuleIds(id1);
		final DependencyDefinitionPojoPrototype def = newDependencyDefinition(moduleFilter, Sets.newHashSet(ResolutionFlag.RESOLVE_CASE_INSENSITIVE,
				ResolutionFlag.RESOLVE_TO_PARENT), null, RelationshipType.ACCESSES, Binding.LATE, Collections.emptyMap());
		final DependencyDefinitionPojo definition = createDependencyDefinition(id1, def);
		discoveryCore.resolveDependency(context, id2, definition);
		final List<ModuleRelationshipPojo> references = moduleService.findRelationship(q -> q.ofSource(id2).withType(RelationshipType.ACCESSES));
		assertEquals(1, references.size());
		/* The dependency should be created to the parent */
		assertEquals(dependencyIdForLowerCase.getUid(), references.get(0).getDstModule());
	}
	
	@Test
	void testhandleUnresolvedDependenciesCreateMissingModule() {
		final EntityId id1 = createTestModule("Cobol8", "/cobol/programs/cobol8.pgm", ModuleType.COBOL_PROGRAM);
		final ModuleFilter moduleFilter = new ModuleFilter().setNames("MissingModule")
				.setTypes(ModuleType.COBOL_PROGRAM, ModuleType.COBOL_COPYBOOK);
		final var def = newDependencyDefinition(moduleFilter, Collections.emptySet(),
				null, RelationshipType.ACCESSES, Binding.LATE, null);
		final DependencyDefinitionPojo definition = createDependencyDefinition(id1, def);

		/* check that no dependency is created with resolveDependency method */
		discoveryCore.resolveDependency(context, id1, definition);
		final List<ModuleRelationshipPojo> referencesForResolveDependency = moduleService.findRelationship(q -> q.ofSource(id1).withType(RelationshipType.ACCESSES));
		assertEquals(0, referencesForResolveDependency.size());
		
		/* Missing module should be created when calling handleUnresolvedDependencies */
		discoveryCore.handleUnresolvedDependencies(context, id1, definition);
		final List<ModuleRelationshipPojo> references = moduleService.findRelationship(q -> q.ofSource(id1).withType(RelationshipType.ACCESSES));
		assertEquals(1, references.size());

		final ModulePojo module = moduleService.getModule(EntityId.of(references.get(0).getDstModule()));
		assertEquals("MissingModule", module.getName());
		assertEquals(RelationshipType.ACCESSES, references.get(0).getRelationship());
		assertEquals(Storage.UNDEFINED, module.getStorage());
		assertEquals(Origin.CUSTOM, module.getOrigin());
		assertEquals(Identification.MISSING, module.getIdentification());
		assertEquals(ModuleType.UNKNOWN.getTechnology(), module.getTechnology());
		assertEquals(Optional.of(Representation.VIRTUAL), module.getRepresentation());
	}

	@Test
	void testhandleUnresolvedDependenciesCreateUtilityModule() {
		final EntityId id1 = createTestModule("Cobol9", "/cobol/programs/cobol9.pgm", ModuleType.COBOL_PROGRAM);
		final ModuleFilter moduleFilter = new ModuleFilter().setNames("IEBGENER").
				setTypes(Collections.singleton(ModuleType.COBOL_PROGRAM));
		final var def = newDependencyDefinition(moduleFilter, Collections.emptySet(),
				null, RelationshipType.ACCESSES, Binding.LATE, null);
		final DependencyDefinitionPojo definition = createDependencyDefinition(id1, def);
		final DiscoveryContext spyContext = Mockito.spy(context);
		final Config config = Mockito.spy(context.getConfig());
		final UtilityList utilityList = Mockito.mock(UtilityList.class);
		when(assertNotNull(spyContext).getConfig()).thenReturn(config);
		when(assertNotNull(spyContext).getConfig().getUtilityList()).thenReturn(utilityList);
		when(assertNotNull(spyContext).getConfig().getUtilityList().isUtility("IEBGENER")).thenReturn(true);
		
		/* check that no dependency is created with resolveDependency method */
		discoveryCore.resolveDependency(context, id1, definition);
		final List<ModuleRelationshipPojo> referencesForResolveDependency = moduleService.findRelationship(q -> q.ofSource(id1).withType(RelationshipType.ACCESSES));
		assertEquals(0, referencesForResolveDependency.size());
		
		/* Utility module should be created when calling handleUnresolvedDependencies */
		discoveryCore.handleUnresolvedDependencies(spyContext, id1, definition);
		final List<ModuleRelationshipPojo> references = moduleService.findRelationship(q -> q.ofSource(id1).withType(RelationshipType.ACCESSES));
		assertEquals(1, references.size());
		
		final ModulePojo module = moduleService.getModule(EntityId.of(references.get(0).getDstModule()));
		assertEquals("IEBGENER", module.getName());
		assertEquals(RelationshipType.ACCESSES, references.get(0).getRelationship());
		assertEquals(ModuleType.UNKNOWN.getTechnology(), module.getTechnology());
		assertEquals(Storage.UNDEFINED, module.getStorage());
		assertEquals(Origin.ENVIRONMENT, module.getOrigin());
		assertEquals(Identification.IDENTIFIED, module.getIdentification());
		assertEquals(Optional.of(Representation.VIRTUAL), module.getRepresentation());
	}
	
	@Test
	void testAnchorResultWithResolveAllAndResolveAny() {
		final ContributorResult anchoredResult = getContributorResult(ContributorResult.Type.SUB_MODULE, new ModuleFilter(),
				Sets.newHashSet(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL, ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY));
		
		final IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, 
				() -> discoveryCore.anchorAndImportResult(context, anchoredResult));
		assertTrue(exception.getMessage().contains("Mutually exclusive flags are not allowed"));
	}
	
	@Test
	void testAnchorResultWithErrorFlagAndResolveAnyFlag() {
		final ContributorResult anchoredResult = getContributorResult(ContributorResult.Type.SUB_MODULE, new ModuleFilter(),
				Sets.newHashSet(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ERROR, ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY));
		
		final IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, 
				() -> discoveryCore.anchorAndImportResult(context, anchoredResult));
		assertTrue(exception.getMessage().contains("Mutually exclusive flags are not allowed"));
	}
	
	@Test
	void testAnchorResultWithErrorFlagAndResolveAllFlag() {
		final ContributorResult anchoredResult = getContributorResult(ContributorResult.Type.SUB_MODULE, new ModuleFilter(),
				Sets.newHashSet(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ERROR, ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL));
		
		final IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, 
				() -> discoveryCore.anchorAndImportResult(context, anchoredResult));
		assertTrue(exception.getMessage().contains("Mutually exclusive flags are not allowed"));
	}
	
	@Test
	void testAnchorResultWithAllflags() {
		final ContributorResult anchoredResult = getContributorResult(ContributorResult.Type.SUB_MODULE, new ModuleFilter(),
				Sets.newHashSet(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY, ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL,
						ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ERROR));
		
		final IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, 
				() -> discoveryCore.anchorAndImportResult(context, anchoredResult));
		assertTrue(exception.getMessage().contains("Mutually exclusive flags are not allowed"));
	}

	@Test
	void testAnchorImportWithMultipleExistingModules() {
		createTestModule("duplicateModule", null, ModuleType.UNKNOWN);
		createTestModule("duplicateModule", null, ModuleType.UNKNOWN);
		final ContributorResult anchoredResult = getContributorResult(ContributorResult.Type.EXTERNAL_MODULE, new ModuleFilter().setNames("duplicateModule"),
				Sets.newHashSet(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY));
		/* check that anchor contributor does not throw any exception when multiple modules are found */
		assertDoesNotThrow(() -> discoveryCore.anchorAndImportResult(context, anchoredResult));
	}

	@Test
	void testAnchorResultWithCreateIfMissingflag() {
		final ModulePojoPrototype cobolProgram = new ModulePojoPrototype();
		cobolProgram.setProject(projectId);
		cobolProgram.setName("TestSqlTable");
		cobolProgram.setOrigin(Origin.CUSTOM);
		cobolProgram.setStorage(Storage.FILE);
		cobolProgram.setIdentification(Identification.IDENTIFIED);
		cobolProgram.setPath(null);
		cobolProgram.setTechnology(ModuleType.SQL_VIEW.getTechnology());
		cobolProgram.setType(ModuleType.SQL_VIEW.getType());
		cobolProgram.setCreator(Creator.API);
		final var sqlTableModule = moduleService.getModule(moduleService.create(cobolProgram));
		assertTrue(sqlTableModule.getMetricsDate().isEmpty());
		
		final var definition = newModuleDefinition("someSqlTable", ModuleType.SQL_TABLE, null, null, Storage.DATABASE,
				Representation.VIRTUAL, Collections.emptyList(), Identification.IDENTIFIED, Origin.CUSTOM);

		final ContributorResult anchoredResult = new ContributorResult(ContributorResult.Type.ANCHOR_TO,
						new ModuleFilter().setNames("TestSqlTable").setTypes(ModuleType.SQL_TABLE, ModuleType.SQL_VIEW), 
						Sets.newHashSet(ResolutionFlag.CREATE_IF_MISSING), definition,
						Collections.emptyList(), Collections.emptyList(),
						Collections.emptyList(), Collections.emptyList(), Collections.emptyList());
		
		discoveryCore.anchorAndImportResult(context, anchoredResult);
		final ModulePojo updatedSqlTableModule = moduleService.getModule(sqlTableModule.identity());
		/* The metrics date has been updated from null value which validates to update of the module */
		assertNotNull(assertNotNull(updatedSqlTableModule).getMetricsDate());
		/* This confirms that the module type is not updated, Since we are identifying the module with the name it remains same */
		assertEquals(ModuleType.SQL_VIEW.getType(), assertNotNull(updatedSqlTableModule).getType());
	}
	
	@Test
	void testAnchorResultErrorForMultipleModules() {
		final ContributorResult anchoredResult = getContributorResult(ContributorResult.Type.ROOT_MODULE, new ModuleFilter()
				.setModuleIds(Sets.newHashSet(dependencyIdForUpperCase, dependencyId3)), Collections.emptySet());
		final IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, 
				() -> discoveryCore.anchorAndImportResult(context, anchoredResult));
		assertTrue(exception.getMessage().contains("More than one target anchor found for"));
	}
	
	@Test
	void testAnchorResultForMultipleMatchError() {
		final ContributorResult anchoredResult = getContributorResult(ContributorResult.Type.ROOT_MODULE, new ModuleFilter()
				.setModuleIds(Sets.newHashSet(dependencyIdForUpperCase, dependencyId3)), Sets.newHashSet(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ERROR));
		final IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, 
				() -> discoveryCore.anchorAndImportResult(context, anchoredResult));
		assertTrue(exception.getMessage().contains("More than one target anchor found for"));
	}

	@Test 
	void testAnchorResultForMatchAny() {
		final EntityId id1 = createTestModule("RESOLVEANY1", "/cobol/programs/RESOLVEANY1.cbl", ModuleType.COBOL_PROGRAM);
		final EntityId id2 = createTestModule("RESOLVEANY2", "/cobol/programs/RESOLVEANY2.cbl", ModuleType.COBOL_PROGRAM);
		final ModuleFilter filter = new ModuleFilter().setModuleIds(Sets.newHashSet(id1, id2));
		final ContributorResult anchoredResult = getContributorResult(ContributorResult.Type.ROOT_MODULE, filter, 
				Sets.newHashSet(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY));
		discoveryCore.anchorAndImportResult(context, anchoredResult);
		/* Check that the result is not imported - however we don't know which of the modules has been updated */
		final ModulePojo moduleForId1 = moduleService.getModule(id1);
		final ModulePojo moduleForId2 = moduleService.getModule(id2);

		final EntityId modifiedId;
		if ("RESOLVEANY1".equals(moduleForId1.getName())) {
			modifiedId = id2;
			assertEquals("anchorAndModule", moduleForId2.getName());
		} else {
			modifiedId = id1;
			assertEquals("RESOLVEANY2", moduleForId2.getName());
			assertEquals("anchorAndModule", moduleForId1.getName());
		}

		/* check that the result is imported for modifiedId (id1 or id2) */
		final List<ErrorMarkerPojo> errors = moduleService.findErrorMarkers(q -> q.ofProject(projectId).ofModule(modifiedId));
		assertEquals(1, errors.size());
		final List<ModuleDeadCodePojo> deadCodes = moduleService.findDeadCode(q -> q.ofModule(modifiedId));
		assertEquals(1, deadCodes.size());
		final List<StatementPojo> statements = moduleService.findStatements(q -> q.ofModule(modifiedId));
		assertEquals(1, statements.size());
		final List<DependencyDefinitionPojo> definitions = discoveryPersistence.fetchUnresolvedDependencyDefinitions(modifiedId);
		assertEquals(1, definitions.size());
		final Collection<DeferredActionDefinition> deferredActions = temporaryStorage.getAndRemoveDeferredActions(context.getJobId(), modifiedId);
		assertEquals(1, deferredActions.size());
		/* assert that contributor result is not stored into the temporary storage again */
		final Collection<ContributorResult> anchoredResults = temporaryStorage.getAndRemoveUnanchoredContributorResults(context.getJobId());
		assertEquals(0, anchoredResults.size());
	}
	
	@Test 
	void testAnchorResultForMatchAll() {
		discoveryCache.clearDiscoveryJobCache(context.getJobId());
		final EntityId id1 = createTestModule("RESOLVEALL1", "/cobol/programs/RESOLVEALL1.cbl", ModuleType.COBOL_PROGRAM);
		final EntityId id2 = createTestModule("RESOLVEALL2", "/cobol/programs/RESOLVEALL2.cbl", ModuleType.COBOL_PROGRAM);
		final ModuleFilter filter = new ModuleFilter().setModuleIds(Sets.newHashSet(id1, id2));
		final ContributorResult anchoredResult = getContributorResult(ContributorResult.Type.ROOT_MODULE, filter, 
				Sets.newHashSet(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ALL));
		discoveryCore.anchorAndImportResult(context, anchoredResult);
		final ModulePojo importedModule1 = moduleService.getModule(id1);
		assertEquals("anchorAndModule", importedModule1.getName());
		final ModulePojo importedModule2 = moduleService.getModule(id2);
		assertEquals("anchorAndModule", importedModule2.getName());
		/* Assert that contributor result that is stored inside temporary storage contains filter with NOT IDs */
		final Collection<ContributorResult> actualAnchoredResults = temporaryStorage.getAndRemoveUnanchoredContributorResults(context.getJobId());
		assertEquals(1, actualAnchoredResults.size());
		final ModuleFilter notModuleFilter = actualAnchoredResults.iterator().next().getModuleFilter().getNot().get();
		final Set<EntityId> notIds = notModuleFilter.getModuleIds();
		assertTrue(notIds.containsAll(Sets.newHashSet(id1, id2)));
		discoveryCache.clearDiscoveryJobCache(context.getJobId());
	}

	@Test
	void testAnchorResultNoModuleFound() {
		/* If no module is found for anchored contributor result, test that it is put back into temporary storage */
		final ModuleFilter moduleFilter = new ModuleFilter().setNames("UnavailableModule");
		final ContributorResult anchoredResult = getContributorResult(ContributorResult.Type.ROOT_MODULE, moduleFilter,
				Sets.newHashSet(ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY));
		discoveryCore.anchorAndImportResult(context, anchoredResult);
		final Collection<ContributorResult> actualResults = temporaryStorage.getAndRemoveUnanchoredContributorResults(context.getJobId());
		assertEquals(1, actualResults.size());	
		assertEquals("UnavailableModule", actualResults.iterator().next().getModuleFilter().getNames().iterator().next());
		discoveryCache.clearDiscoveryJobCache(context.getJobId());
	}
	
	private ContributorResult getContributorResult(final ContributorResult.Type type, final ModuleFilter filter, final Set<ResolutionFlag> flags) {
		final var definition = newModuleDefinition("anchorAndModule", ModuleType.BASIC_FUNCTION, null,
				null, Storage.FILE_SECTION, Representation.VIRTUAL,
				Collections.emptyList(), Identification.IDENTIFIED, Origin.CUSTOM);
		final List<ErrorMarker> errors = Collections.singletonList(
				new ErrorMarker(Severity.WARNING, ErrorKey.UNDISCOVERED_DEPENDENCY, "error", null));
		final List<ModuleDeadCodePojoPrototype> deadCodes = Collections.singletonList(new ModuleDeadCodePojoPrototype()
				.setDeadCode("deadCode")
				.setStartingLine(10)
				.setNumberOfLines(11));
		final List<DeferredActionDefinition> deferredActions = Collections.singletonList(
				new DeferredActionDefinition("innowake.mining.server.discovery.dawn.metrics.contributors.TestContributor", "src/cobol/TESTMODULE.cbl"));
		final List<StatementPojoPrototype> statements = Collections
				.singletonList(new StatementPojoPrototype().setType(StatementType.DISPLAY).setText("DISPLAY"));
		final List<DependencyDefinitionPojoPrototype> dependencies = Collections.singletonList(new DependencyDefinitionPojoPrototype()
				.setModuleFilters(List.of(filter))
				.setResolutionFlags(Sets.newHashSet(ResolutionFlag.RESOLVE_CASE_INSENSITIVE))
				.setRelationshipType(RelationshipType.NONE)
				.setBindingType(Binding.LATE));
		return  new ContributorResult(type, filter, flags, definition,
				errors, deadCodes, deferredActions, statements, dependencies);
	}
	
	private EntityId createTestModule(final String name, @Nullable final String path, final ModuleType moduleType) {
		final ModulePojoPrototype cobolProgram = new ModulePojoPrototype();
		cobolProgram.setProject(projectId);
		cobolProgram.setName(name);
		cobolProgram.setOrigin(Origin.CUSTOM);
		cobolProgram.setStorage(Storage.FILE);
		cobolProgram.setIdentification(Identification.IDENTIFIED);
		cobolProgram.setPath(path);
		cobolProgram.setTechnology(moduleType.getTechnology());
		cobolProgram.setType(moduleType.getType());
		cobolProgram.setCreator(Creator.DISCOVERY);
		return moduleService.create(cobolProgram);
	}
	
	private void assertEqualsReference(final ModuleRelationshipPojo actualReference, final EntityId fromId, final EntityId toId, @Nullable final ModuleLocation fromLocation,
			final RelationshipType relationship, final Map<String, Object> attributes) {
		assertEquals(fromId.getUid(), actualReference.getSrcModule());
		assertEquals(toId.getUid(), actualReference.getDstModule());
		assertEquals(fromLocation, actualReference.getSrcLocation().orElse(null));
		assertEquals(relationship, actualReference.getRelationship());
		final Optional<Map<String, Object>> properties = actualReference.getProperties();
		assertTrue(properties.isPresent());
		assertEquals(attributes.size(), properties.get().size());
		attributes.forEach((key, value) ->
			assertEquals(value.toString(), properties.get().get(key).toString().replaceAll("\"", ""))
		);
	}
	
	private void assertEqualsError(final ErrorMarkerPojo actualError, final ErrorKey key, final Severity severity, final String message) {
		assertEquals(key, actualError.getKey());
		assertEquals(severity, actualError.getSeverity());
		assertTrue(assertNotNull(actualError.getCause()).contains(message));
	}
	
	private void testContributorResult(final DiscoveryContributorFromSource contributor, final SourcePojo sourceObject,
			final DiscoveryContext discoveryContext, final ModuleType moduleType, final String moduleName) {
		final List<ContributorResult> contributorResultList = discoveryCore.executeContributorOnSourceObject(contributor, discoveryContext,
				sourceObject);
		assertEquals(1, contributorResultList.size());	
		ContributorResult contributorResult = contributorResultList.get(0);
		assertEquals(ContributorResult.Type.ROOT_MODULE, contributorResult.getType());
		assertEquals(moduleType.getTechnology(), contributorResult.getModuleDefinition().technology.get());
		assertEquals(moduleType.getType(), contributorResult.getModuleDefinition().type.get());
		assertEquals(moduleName, contributorResult.getModuleDefinition().name.get());
	}
	
	@Test
	void testExecuteDeferredActionWithNoSourceContributor() {
		final String name = "ModuleDeferred";
		final EntityId id = createTestModule(name, null, ModuleType.C_HEADER);
		final DeferredActionDefinition deferredAction = new DeferredActionDefinition(NoSourceContributor.class.getName(), "sampleDeferredExecution");
		final List<ContributorResult> contributorResults = discoveryCoreForDeferredAction
				.executeDeferredAction(context, id, deferredAction);
		assertEquals(1, contributorResults.size());
		assertEquals(ContributorResult.Type.ANCHOR_TO, contributorResults.get(0).getType());
		assertEquals(id, contributorResults.get(0).getModuleFilter().getModuleIds().iterator().next());
	}
	
	@Test
	void testExecuteDeferredActionWithSourceContributor() {
		final String name = "SourceModuleDeferred";
		final EntityId id = createTestModule(name, "c/program/sourceModule1.c", ModuleType.C_HEADER);
		sourceService.create(new SourcePojoPrototype().setProject(projectId).setName("sourceDeferred").setPath("c/program/sourceModule1.c")
				.setTechnology(Technology.C).setType(Type.HEADER).setContent(new BinaryString("  ")));
		final DeferredActionDefinition deferredAction = new DeferredActionDefinition(SourceContributor.class.getName(), "sourceDeferred");
		final List<ContributorResult> contributorResults = discoveryCoreForDeferredAction
				.executeDeferredAction(context, id, deferredAction);
		assertEquals(1, contributorResults.size());
		assertEquals(ContributorResult.Type.ANCHOR_TO, contributorResults.get(0).getType());
		assertEquals(id, contributorResults.get(0).getModuleFilter().getModuleIds().iterator().next());
	}
	
	@Test
	void testExecuteDeferredActionForSubModule() {
		final String containingPath = "c/program/sourceModule5.c";
		final String rootModuleName = "rootModule";
		final EntityId rootId = createTestModule(rootModuleName, containingPath, ModuleType.C_HEADER);
		sourceService.create(new SourcePojoPrototype().setProject(projectId).setName(rootModuleName).setPath(containingPath)
				.setTechnology(Technology.C).setType(Type.HEADER).setContent(new BinaryString("  ")));
		final String subModuleName = "SourceModuleDeferred";
		final EntityId id = createTestModule(subModuleName, null, ModuleType.C_HEADER);
		final ModulePojo subModule = moduleService.getModule(id);
		final ModulePojo rootModule = moduleService.getModule(rootId);

		final ModuleRelationshipPojoPrototype containsModule = new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.CONTAINS)
				.setSrcModule(rootModule.identity())
				.setSrcLocation(new ModuleLocation(0,10))
				.setDstModule(subModule.identity());
		
		moduleService.createRelationship(containsModule);
		final DeferredActionDefinition deferredAction = new DeferredActionDefinition(NoSourceContributor.class.getName(), "sampleDeferredExecution");
		final List<ContributorResult> contributorResults = discoveryCoreForDeferredAction
				.executeDeferredAction(context, id, deferredAction);
		assertEquals(1, contributorResults.size());
		assertEquals(ContributorResult.Type.ANCHOR_TO, contributorResults.get(0).getType());
		assertEquals(id, contributorResults.get(0).getModuleFilter().getModuleIds().iterator().next());
	}
	
	@Test
	void testExecuteDeferredActionWithPartialParameters() {
		final EntityId id = createTestModule("partialDeferred", "c/program/sourceModule2.c", ModuleType.C_HEADER);
		final DeferredActionDefinition deferredAction = new DeferredActionDefinition(NoSourceContributor.class.getName(), "partialDeferredExecution");
		final List<ContributorResult> contributorResults = discoveryCoreForDeferredAction
				.executeDeferredAction(context, id, deferredAction);
		assertEquals(0, contributorResults.size());
		/* check that the deferred method has correctly executed, the deferred method update sourceMetrics of the module */
		final ModulePojo module = moduleService.getModule(id);
		final Optional<SourceMetricsPojo> sourceMetrics = module.getSourceMetrics();
		assertTrue(sourceMetrics.isPresent());
		assertEquals(6, sourceMetrics.get().getCodeLines());
		assertEquals(10, sourceMetrics.get().getCommentLines());
	}
	
	@Test
	void testExecuteDeferredActionContextParameter() {
		final EntityId id = createTestModule("partialDeferred", "c/program/sourceModule3.c", ModuleType.C_HEADER);
		final CustomContext customContext = new CustomContext(id, "PartialDeferredUpdated");
		final DeferredActionDefinition deferredAction = new DeferredActionDefinition(SourceContributor.class.getName(), "updateModule", customContext);
		final List<ContributorResult> contributorResults = discoveryCoreForDeferredAction.
				executeDeferredAction(context, id, deferredAction);
		assertEquals(0, contributorResults.size());
		/* check that the deferred method has correctly executed, the deferred method update name of the module */
		final ModulePojo module = moduleService.getModule(id);
		assertEquals("PartialDeferredUpdated", module.getName());
	}
	
	@Test
	void testExecuteDeferredActionWithUnsupportedParameter() {
		final EntityId id = createTestModule("partialDeferred", "c/program/sourceModule4.c", ModuleType.C_HEADER);
		final DeferredActionDefinition deferredAction = new DeferredActionDefinition(SourceContributor.class.getName(), "updateModule", "SerializableString");
		final IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
				() -> discoveryCoreForDeferredAction.executeDeferredAction(context, id, deferredAction));
		assertTrue(exception.getMessage().contains("Provided context object of type"));
	}
	
	@Test
	void testExecuteDeferredActionWithoutRequiredContextObject() {
		final DeferredActionDefinition deferredAction = new DeferredActionDefinition(SourceContributor.class.getName(), "updateModule");
		final IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
				() -> discoveryCoreForDeferredAction.executeDeferredAction(context, dependencyId3, deferredAction));
		assertTrue(exception.getMessage().contains("Maybe a required context object was not provided"));
	}
	
	@Test
	void testExecuteDeferredActionWithInvalidClass() {
		final DeferredActionDefinition deferredAction = new DeferredActionDefinition("NonExistantClassName", "dummyMethod");
		final IllegalStateException exception = assertThrows(IllegalStateException.class,
				() -> discoveryCoreForDeferredAction.executeDeferredAction(context, dependencyId3, deferredAction));
		assertTrue(exception.getMessage().contains("No matching contributor class with name"));
	}
	
	@Test
	void testExecuteDeferredActionWithoutValidMethodName() {
		final DeferredActionDefinition deferredAction = new DeferredActionDefinition(NoSourceContributor.class.getName(), "InvalidMethod");
		final IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
				() -> discoveryCoreForDeferredAction.executeDeferredAction(context, dependencyId3, deferredAction));
		assertTrue(exception.getMessage().contains("No method with name InvalidMethod is present inside class"));
	}
	
	@Test
	void testExecuteDeferredActionWithMultipleMatchingMethodNames() {
		final DeferredActionDefinition deferredAction = new DeferredActionDefinition(SourceContributor.class.getName(), "InvalidmultipleMethod");
		final IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
				() -> discoveryCoreForDeferredAction.executeDeferredAction(context, dependencyId3, deferredAction));
		assertTrue(exception.getMessage().contains("defines more than one deferred action named InvalidmultipleMethod"));
	}
	
	@Test
	void testExecuteDeferredActionWithInvalidModule() {
		final EntityId id = createTestModule("partialDeferred", null, ModuleType.C_HEADER);
		final DeferredActionDefinition deferredAction = new DeferredActionDefinition(SourceContributor.class.getName(), "sourceDeferredExecution");
		final IllegalStateException exception = assertThrows(IllegalStateException.class,
				() -> discoveryCoreForDeferredAction.executeDeferredAction(context, id, deferredAction));
		assertTrue(exception.getMessage().contains("Module path/containsPath should be present for source based DiscoveryBuilder"));
	}
	
	@Test
	void testExecuteDeferredActionWithInvalidModuleId() {
		final DeferredActionDefinition deferredAction = new DeferredActionDefinition(SourceContributor.class.getName(), "sourceDeferredExecution");
		final IllegalStateException exception = assertThrows(IllegalStateException.class,
				() -> discoveryCoreForDeferredAction.executeDeferredAction(context, EntityId.of(-1L), deferredAction));
		assertTrue(exception.getMessage().contains("Module with Id:[uid=null,nid=-1] not found"));
	}
	
	@Test
	void testExecuteDeferredActionWithInvalidMethodParameter() {
		final DeferredActionDefinition deferredAction = new DeferredActionDefinition(NoSourceContributor.class.getName(), "methodWithInvalidParameter");
		final IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
				() -> discoveryCoreForDeferredAction.executeDeferredAction(context, dependencyId3, deferredAction));
		assertTrue(exception.getMessage().contains("Unsupported parameter of type"));
	}
	
	@Test
	void testExecuteDeferredActionWithMultipleContextParameters() {
		final DeferredActionDefinition deferredAction = new DeferredActionDefinition(NoSourceContributor.class.getName(), "methodWithMutlipleContextParameters",
				"context object");
		final IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
				() -> discoveryCoreForDeferredAction.executeDeferredAction(context, dependencyId3, deferredAction));
		assertTrue(exception.getMessage().contains("Unsupported parameter of type"));
	}
	
	@Test
	void testCreateIfMissingDefaultModules() {
		final var definition = newModuleDefinition("someSqlTable", ModuleType.SQL_TABLE, null, null, Storage.DATABASE,
				ModulePojo.Representation.VIRTUAL, Collections.emptyList(), Identification.IDENTIFIED, Origin.CUSTOM);
		final ContributorResult anchoredResult = new ContributorResult(ContributorResult.Type.ANCHOR_TO,
						new ModuleFilter().setNames("someSqlTable").setTypes(ModuleType.SQL_TABLE, ModuleType.SQL_VIEW), 
						Collections.emptySet(), definition,
						Collections.emptyList(), Collections.emptyList(),
						Collections.emptyList(), Collections.emptyList(), Collections.emptyList());

		discoveryCore.createIfMissingDefaultModules(context, anchoredResult);
		List<ModulePojo> sqlTableModule = moduleService.findModules(b -> b.ofProject(projectId).withName("someSqlTable"));
		assertEquals(1, sqlTableModule.size());
	}

	@Test
	void testMergeDependencies() {
	/* Create test data */
		final var location = new ModuleLocation(1, 13);
		final Map<String, Object> attributes = Map.of("attribute1", "value1", "attribute2", "value2");
		final var flags = Sets.newHashSet(ResolutionFlag.RESOLVE_CASE_INSENSITIVE, ResolutionFlag.MERGE_DUPLICATES);
		final var dd1 = new DependencyDefinitionPojoPrototype().setBindingType(Binding.LATE).setRelationshipType(RelationshipType.CALLS)
				.setResolutionFlags(flags)
				.setModuleFilters(Collections.singletonList(new ModuleFilter().setNames("Test 1").setTypes(ModuleType.UNKNOWN)))
				.setLocation(location)
				.setAttributes(attributes)
				.setReachedFromModules(Collections.singletonList(new ModuleFilter().setNames("Parent1")));
		final var dd2 = new DependencyDefinitionPojoPrototype().setBindingType(Binding.LATE).setRelationshipType(RelationshipType.CALLS)
				.setResolutionFlags(Sets.newHashSet(ResolutionFlag.CREATE_IF_MISSING))
				.setLocation(location)
				.setAttributes(attributes)
				.setModuleFilters(Collections.singletonList(new ModuleFilter().setNames("Test 2").setTypes(ModuleType.UNKNOWN)));
		final var dd3 = new DependencyDefinitionPojoPrototype().setBindingType(Binding.LATE).setRelationshipType(RelationshipType.CALLS)
				.setResolutionFlags(flags)
				.setLocation(location)
				.setAttributes(attributes)
				.setModuleFilters(Collections.singletonList(new ModuleFilter().setNames("Test 1").setTypes(ModuleType.UNKNOWN)))
				.setReachedFromModules(Collections.singletonList(new ModuleFilter().setNames("Parent1")));
		final var dd4 = new DependencyDefinitionPojoPrototype().setBindingType(Binding.LATE).setRelationshipType(RelationshipType.CALLS)
				.setResolutionFlags(flags)
				.setLocation(location)
				.setAttributes(attributes)
				.setModuleFilters(Collections.singletonList(new ModuleFilter().setNames("Test 1").setTypes(ModuleType.UNKNOWN)))
				.setReachedFromModules(Collections.singletonList(new ModuleFilter().setNames("Parent2")));
		discoveryPersistence.persistDependencyDefinitions(assertNotNull(context), dependencyId3, List.of(dd1,dd2,dd3,dd4));
		/* This will merge the dependencies dd1, dd3 and dd4 which have same dependencies but different reachedFrom*/
		final var unresolvedDependencies = discoveryPersistence.fetchUnresolvedDependencyDefinitions(dependencyId3);
		discoveryCore.mergeDependencies(context, dependencyId3, unresolvedDependencies);
		final var definitionsWithoutDuplicates = discoveryPersistence.fetchUnresolvedDependencyDefinitions(dependencyId3);
		assertEquals(2, definitionsWithoutDuplicates.size());
		final var definition1 = definitionsWithoutDuplicates.get(0);
		assertEquals("Test 2", definition1.getModuleFilters().get(0).getNames().iterator().next());
		final var definition2 = definitionsWithoutDuplicates.get(1);
		assertEquals("Test 1", definition2.getModuleFilters().get(0).getNames().iterator().next());
		assertEquals(2, definition2.getReachedFromModules().size());
		assertThat(definition2.getReachedFromModules().stream().map(filter -> filter.getNames().iterator().next()).collect(Collectors.toList()),
				containsInAnyOrder("Parent1", "Parent2"));
	}

	/**
	 * A dummy class that implements {@link DiscoveryContributor}
	 */
	@SuppressWarnings("unused")
	class NoSourceContributor implements DiscoveryContributor {
		
		@Override
		public void contribute(DiscoveryBuilder builder, DiscoveryContext context) {
		}
		
		@DeferredAction
		public void sampleDeferredExecution(final DiscoveryContext context, final DiscoveryBuilder builder, final ModuleBuilder moduleBuilder,
				final ModulePojo module, final SourcePojo sourceObject) {
			/* does nothing */
		}
		
		@DeferredAction
		public void partialDeferredExecution(final DiscoveryContext context, final ModulePojo module) {
			final SourceMetricsPojoPrototype sourceMetrics = new SourceMetricsPojoPrototype().setCodeLines(6).setCommentLines(10);
			
			final ModulePojoPrototype proto = new ModulePojoPrototype()
					.withId(module.identity())
					.setSourceMetrics(sourceMetrics);
			moduleService.update(proto);
		}
		
		@DeferredAction
		public void methodWithInvalidParameter(final DiscoveryContext context, final ModuleType type) {
			/* does nothing */
		}
		
		@DeferredAction
		public void methodWithMutlipleContextParameters(final String context1, final String context2) {
			/* does nothing */
		}
	}
	
	/**
	 * A dummy class that implements {@link DiscoveryContributorFromSource}
	 */
	@SuppressWarnings("unused")
	class SourceContributor implements DiscoveryContributorFromSource {

		@Override
		public boolean accept(DiscoveryContext context, SourcePojo sourceObject) {
			return true;
		}

		@Override
		public void contribute(DiscoveryBuilderFromSource builder, DiscoveryContext context, SourcePojo sourceObject) {
		}
		
		
		@DeferredAction("sourceDeferred")
		public void sourceDeferredExecution(final DiscoveryContext context, final DiscoveryBuilderFromSource builder, final ModuleBuilder moduleBuilder,
				final ModulePojo module, final SourcePojo sourceObject, final ModuleLightweightPojo moduleLightweight) {
			/* does nothing */
		}
		
		@DeferredAction
		public void updateModule(final CustomContext context) {
			final ModulePojo module = moduleService.getModule(context.getModuleId());
			
			final ModulePojoPrototype proto = new ModulePojoPrototype()
					.withId(module.identity())
					.setName(context.getModuleName());
			
			moduleService.update(proto);
		}
		
		@DeferredAction
		public void InvalidmultipleMethod() { /* does nothing */ }
		
		@DeferredAction
		public void InvalidmultipleMethod(final DiscoveryContext context) { /* does nothing */ }
	}

	private ModulePojoPrototype newModuleDefinition(final String name, @Nullable final ModuleType moduleType, @Nullable final ModuleLocation location, @Nullable final String path,
			@Nullable final Storage storage, @Nullable final Representation representation, final Collection<AdditionalInfo> additionalInfos,
			@Nullable final Identification identification, @Nullable final Origin origin) {
		
		assertTrue(additionalInfos.isEmpty(), "additional infos are not supported");
		
		final ModulePojoPrototype module = new ModulePojoPrototype()
				.setName(name)
				.setLocation(location)
				.setPath(path);
		if (storage != null) {
			module.setStorage(storage);
		}
		if (origin != null) {
			module.setOrigin(origin);
		}
		if (identification != null) {
			module.setIdentification(identification);
		}
		if (moduleType != null) {
			module.setTechnology(moduleType.getTechnology())
				  .setType(moduleType.getType());
		}

		if (representation != null) {
			module.setRepresentation(representation);
		}

		return module;
	}
	
	private DependencyDefinitionPojoPrototype newDependencyDefinition(final ModuleFilter moduleFilter, final Set<ResolutionFlag> resolutionFlags,
			@Nullable final ModuleLocation location, final RelationshipType relationshipType, final Binding bindingType, @Nullable final Map<String, Object> attributes) {
		return newDependencyDefinition(List.of(moduleFilter), resolutionFlags, location, relationshipType, bindingType, attributes);
	}

	private DependencyDefinitionPojoPrototype newDependencyDefinition(final List<ModuleFilter> moduleFilters, final Set<ResolutionFlag> resolutionFlags,
			@Nullable final ModuleLocation location, final RelationshipType relationshipType, final Binding bindingType, @Nullable final Map<String, Object> attributes) {
		final DependencyDefinitionPojoPrototype definition = new DependencyDefinitionPojoPrototype()
				.setModuleFilters(moduleFilters)
				.setResolutionFlags(resolutionFlags)
				.setRelationshipType(relationshipType)
				.setBindingType(bindingType);

		if (location != null) {
			definition.setLocation(location);
		}

		if (attributes != null && ! attributes.isEmpty()) {
			definition.setAttributes(attributes);
		}

		return definition;
	}
	
	/**
	 * A dummy context class for deferred Method that will include info about module's id and and a name to which it has to be updated to. 
	 **/
	class CustomContext implements Serializable {
		final EntityId moduleId;
		final String moduleName;
		
		public CustomContext(final EntityId moduleId, final String moduleName) {
			this.moduleId = moduleId;
			this.moduleName = moduleName;
		}
		
		public EntityId getModuleId() {
			return moduleId;
		}
		
		public String getModuleName() {
			return moduleName;
		}
	}
}
