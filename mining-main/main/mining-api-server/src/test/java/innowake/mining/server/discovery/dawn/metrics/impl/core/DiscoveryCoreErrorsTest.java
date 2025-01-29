package innowake.mining.server.discovery.dawn.metrics.impl.core;

import com.google.common.collect.Sets;
import innowake.mining.data.discovery.metrics.DiscoveryCache;
import innowake.mining.server.discovery.dawn.metrics.api.model.ContributorResult;
import innowake.mining.server.discovery.dawn.metrics.api.persistence.ImportResult;
import innowake.mining.server.discovery.dawn.metrics.impl.persistence.DiscoveryPersistenceImpl;
import innowake.mining.server.discovery.dawn.metrics.test.DiscoveryTestContext;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.DependencyDefinitionPojo;
import innowake.mining.shared.entities.ModuleFilter;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ResolutionFlag;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.testing.SourcePojoDummy;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.sql.SQLException;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

import static innowake.lib.core.lang.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

/**
 * Tests to ensure the errors scenarios are properly handled and reported.
 */

 class DiscoveryCoreErrorsTest extends DatabaseRelatedTest {

    @Autowired
    private AutowireCapableBeanFactory autowireCapableBeanFactory;

    @MockBean
    private DiscoveryPersistenceImpl discoveryPersistence;

    @Autowired
    private DiscoveryCache discoveryCache;

    EntityId projectId = EntityId.of(-1L);

    DiscoveryTestContext context;

    private DiscoveryCoreImpl discoveryCore;

    @BeforeAll
    void initialize() {
        final BinaryString content = new BinaryString("THERE SHOULD BE COBOL IN HERE");
        final SourcePojo testSource = SourcePojoDummy.build(o -> o.setName("TEST1").setPath("/src/cobol/programs/TEST1.cbl")
        		.setTechnology(Technology.COBOL).setType(Type.PROGRAM).setContent(content));
        context = new DiscoveryTestContext(Collections.singletonList(testSource), projectId);
        discoveryCore = new DiscoveryCoreImpl(Collections.emptyList(), Collections.emptyList());
        autowireCapableBeanFactory.autowireBean(discoveryCore);
    }

    @Test
    void testImportContributorRootModulePersistError() {
        when(discoveryPersistence.persistModule(any(), any(), any()))
                .thenReturn(ImportResult.forAmbiguousMatch("unexpected error happened: "));
        final ModulePojoPrototype moduleDefinition = new ModulePojoPrototype()
        		.setName("subModule1")
        		.setTechnology(Technology.ASSEMBLER)
        		.setType(Type.UNKNOWN)
        		.setStorage(Storage.FILE_SECTION)
        		.setRepresentation(Representation.VIRTUAL)
        		.setIdentification(Identification.IDENTIFIED)
        		.setOrigin(Origin.CUSTOM);
        final ContributorResult contributorResultForRoot = new ContributorResult(ContributorResult.Type.ROOT_MODULE,
                new ModuleFilter().setNames("subModule1"),
                Collections.emptySet(), moduleDefinition, Collections.emptyList(),
                Collections.emptyList(), Collections.emptyList(), Collections.emptyList(),
                Collections.emptyList());
        final var results = List.of(contributorResultForRoot);
        final IllegalStateException exception  = assertThrows(IllegalStateException.class, () ->
                discoveryCore.importContributorResults(context, results));
        final String expectedMessage = "While importing the contributor result with moduleFilter: \n"
                + "names: [subModule1]\n" + " Failed to get or create rootModule. Encountered AMBIGUOUS_MATCH: unexpected error happened:";
        assertTrue(exception.getMessage().contains(expectedMessage));
    }

    @Test
    void testUnresolvedDependencyTargetModuleError() {
        final ModuleType moduleType = ModuleType.COBOL_PROGRAM;
		final String key = DiscoveryCache.createLockKeyForParameters("MODULE1".concat(DiscoveryCoreImpl.DISCOVERY_CORE_IMPL_DEPENDENCY),
				moduleType.getTechnology(), moduleType.getType());
        discoveryCache.putValue(context.getJobId(), key, ImportResult.forAmbiguousMatch("unexpected error happened: "));

        final DependencyDefinitionPojo definition = new DependencyDefinitionPojo(
        		UUID.randomUUID(),
        		EntityId.of(5L), null, null,
        		Collections.emptyMap(),
        		Binding.EARLY,
        		new ModuleLocation(10, 100),
        		List.of(new ModuleFilter().setNames("MODULE1").setTypes(moduleType)),
        		RelationshipType.INCLUDES,
        		Sets.newHashSet(ResolutionFlag.RESOLVE_CASE_INSENSITIVE, ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY),
        		false,
        		null
		);
        
        IllegalStateException exception  = assertThrows(IllegalStateException.class, () ->
                discoveryCore.handleUnresolvedDependencies(context, EntityId.of(5L), definition));
        final String expectedMessage = "While creating a reference from Module [uid=null,nid=5] to MISSING target module: " +
                "Failed to get or create target module MODULE1 (COBOL_PROGRAM)" +
                " Encountered AMBIGUOUS_MATCH: unexpected error happened:";
        assertTrue(exception.getMessage().contains(expectedMessage));
        discoveryCache.clearDiscoveryJobCache(context.getJobId());
    }

    @Test
    void testUnresolvedDependencyCreationError() {
        final ModuleType moduleType = ModuleType.COBOL_PROGRAM;
		final String key = DiscoveryCache.createLockKeyForParameters("MODULE1".concat(DiscoveryCoreImpl.DISCOVERY_CORE_IMPL_DEPENDENCY),
				moduleType.getTechnology(), moduleType.getType());
        discoveryCache.putValue(context.getJobId(), key, ImportResult.forSuccessfulCreation(EntityId.of(5L)));
        
        final DependencyDefinitionPojo definition = new DependencyDefinitionPojo(
        		UUID.randomUUID(),
        		EntityId.of(5L), null, null,
        		Collections.emptyMap(),
        		Binding.EARLY,
        		new ModuleLocation(10, 100),
        		List.of(new ModuleFilter().setNames("MODULE1").setTypes(moduleType)),
        		RelationshipType.INCLUDES,
        		Sets.newHashSet(ResolutionFlag.RESOLVE_CASE_INSENSITIVE, ResolutionFlag.MULTIPLE_MATCH_RESOLVE_ANY),
        		false,
        		null
		);

        when(discoveryPersistence.createDependency(any(), any(), any(), any(), any(), any(), any(), any(), any()))
                .thenReturn(ImportResult.forDbError("unexpected error happened: ", new SQLException("Invalid SQL")));

        IllegalStateException exception  = assertThrows(IllegalStateException.class, () ->
                discoveryCore.handleUnresolvedDependencies(context, EntityId.of(5L), definition));
        final String expectedMessage = "Failed to create MISSING dependency from moduleId : [uid=null,nid=5] to target moduleId: [uid=null,nid=5]. Encountered DB_ERROR " +
                ": unexpected error happened: , java.sql.SQLException: Invalid SQL";
        assertTrue(exception.getMessage().contains(expectedMessage));
        discoveryCache.clearDiscoveryJobCache(context.getJobId());
    }
}
