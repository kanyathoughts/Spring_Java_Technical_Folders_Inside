package innowake.mining.data.core.taxonomy;

import innowake.mining.data.core.taxonomy.api.DefaultDependecyModule;
import innowake.mining.data.core.taxonomy.impl.DependencyBasedOperationTaxonomyIdentifier;
import innowake.mining.server.discovery.cache.DiscoveryJobCache;
import innowake.mining.server.service.ProjectServiceImpl;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.config.utility.UtilityEntity;
import innowake.mining.shared.discovery.config.utility.UtilityInterface;
import innowake.mining.shared.discovery.config.utility.UtilityList;
import innowake.mining.shared.discovery.config.utility.UtilityOutbound;
import innowake.mining.shared.discovery.config.utility.UtilityParameter;
import innowake.mining.shared.discovery.config.utility.UtilityTaxonomy;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.TechnicalTaxonomies;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@Tag("mocked")
class TaxonomyIdentificationUtilitiesTest {

	private static final UtilityList UTILITIES = new UtilityList(List.of(
			new UtilityEntity("None", "None", "None", null, List.of(), List.of(), "JZOSPROC", "None", "false", List.of(
					new UtilityTaxonomy("Operations", "Runs JAVA Program")
			), List.of(), List.of("Uncategorized")),
			new UtilityEntity("Integrate migrated application with ASG platform", "None", "None", null, List.of(), List.of(), "INFOPAC", "ASG-ViewDirect",
					"false", List.of(new UtilityTaxonomy("Operations", "Generates Report")
			), List.of(), List.of("Uncategorized")),
			new UtilityEntity("", "Load utility for DB2.", "https://docs.bmc.com/docs/amu11200/home-721204991.html",
					new UtilityInterface("LOADPLUS utility for DB2"), List.of(new UtilityParameter("", new UtilityOutbound(Technology.SQL, Type.TABLE, "1",
					"\\w*\\bINTO\\b\\s*TABLE\\b\\s*(.*)\\b"))), List.of(), "ADUUMAIN", "LOADPLUS for DB2",	"true",
					List.of(new UtilityTaxonomy("DB Access", "Read"), new UtilityTaxonomy("Operations", "Unloads Table")
			), List.of(), List.of("Database Management"))
	));

	@Test
	void testTaxonomyAndTypeForDependencyModule() throws Exception {
		final var tesTDependencyModule = new DefaultDependecyModule(EntityId.VOID, Technology.COBOL, Type.PROGRAM, "TestProgram", null);
		tesTDependencyModule.addOutgoing(new DefaultDependecyModule(Technology.UNKNOWN, Type.UTILITY, "INFOPAC"), RelationshipType.ACCESSES);
		tesTDependencyModule.addOutgoing(new DefaultDependecyModule(Technology.RESOURCE, Type.FILE, "JZO/SPROC"), RelationshipType.ACCESSES);
		tesTDependencyModule.addIncoming(new DefaultDependecyModule(Technology.UNKNOWN, Type.SCRIPT, "TestScript"), RelationshipType.ACCESSES);

		final var cache = mock(DiscoveryJobCache.class);
		final var identifier = new DependencyBasedOperationTaxonomyIdentifier(mock(ProjectServiceImpl.class), cache);

		when(cache.computeValueIfAbsent(any(), any(), any())).thenReturn(UTILITIES);

		final var result = identifier.identify(tesTDependencyModule);
		assertEquals(2, result.size());
		assertEquals(TechnicalTaxonomies.Name.ACCESS_UNIX_FILES, result.get(0).e1);
		assertEquals(TechnicalTaxonomies.Name.GENERATES_REPORT, result.get(1).e1);
	}

	@Test
	void testMultipleUtilityInvocationsAreIgnored() throws Exception {
		final var tesTDependencyModule = new DefaultDependecyModule(EntityId.VOID, Technology.COBOL, Type.PROGRAM, "TestProgram", null);
		tesTDependencyModule.addOutgoing(new DefaultDependecyModule(Technology.UNKNOWN, Type.UTILITY, "INFOPAC"), RelationshipType.ACCESSES);
		tesTDependencyModule.addOutgoing(new DefaultDependecyModule(Technology.UNKNOWN, Type.UTILITY, "JZOSPROC"), RelationshipType.ACCESSES);
		tesTDependencyModule.addIncoming(new DefaultDependecyModule(Technology.UNKNOWN, Type.SCRIPT, "TestScript"), RelationshipType.ACCESSES);

		final var cache = mock(DiscoveryJobCache.class);
		final var identifier = new DependencyBasedOperationTaxonomyIdentifier(mock(ProjectServiceImpl.class), cache);

		when(cache.computeValueIfAbsent(any(), any(), any())).thenReturn(UTILITIES);

		final var result = identifier.identify(tesTDependencyModule);
		assertEquals(1, result.size());
		assertTrue(Set.of(TechnicalTaxonomies.Name.GENERATES_REPORT, TechnicalTaxonomies.Name.RUNS_JAVA_PROGRAM).contains(result.get(0).e1));
	}

	@Test
	void testFileTaxonomyIsAdded() throws Exception {
		final var tesTDependencyModule = new DefaultDependecyModule(EntityId.VOID, Technology.COBOL, Type.PROGRAM, "TestProgram", null);
		tesTDependencyModule.addOutgoing(new DefaultDependecyModule(Technology.RESOURCE, Type.FILE, "JZO/SPROC"), RelationshipType.ACCESSES);

		final var cache = mock(DiscoveryJobCache.class);
		final var identifier = new DependencyBasedOperationTaxonomyIdentifier(mock(ProjectServiceImpl.class), cache);

		when(cache.computeValueIfAbsent(any(), any(), any())).thenReturn(UTILITIES);

		final var result = identifier.identify(tesTDependencyModule);
		assertEquals(1, result.size());
		assertEquals(TechnicalTaxonomies.Name.ACCESS_UNIX_FILES, result.get(0).e1);
	}

	@Test
	void testFileIsIgnoredIfBackSlashIsMissingFromName() throws Exception {
		final var tesTDependencyModule = new DefaultDependecyModule(EntityId.VOID, Technology.COBOL, Type.PROGRAM, "TestProgram", null);
		tesTDependencyModule.addOutgoing(new DefaultDependecyModule(Technology.RESOURCE, Type.FILE, "JZOSPROC"), RelationshipType.ACCESSES);

		final var cache = mock(DiscoveryJobCache.class);
		final var identifier = new DependencyBasedOperationTaxonomyIdentifier(mock(ProjectServiceImpl.class), cache);

		when(cache.computeValueIfAbsent(any(), any(), any())).thenReturn(UTILITIES);

		final var result = identifier.identify(tesTDependencyModule);
		assertEquals(0, result.size());
	}

	@Test
	void testMultipleTaxonomiesAreIdentifiedForUtility() throws Exception {
		final var tesTDependencyModule = new DefaultDependecyModule(EntityId.VOID, Technology.COBOL, Type.PROGRAM, "TestProgram", null);
		tesTDependencyModule.addOutgoing(new DefaultDependecyModule(Technology.UNKNOWN, Type.UTILITY, "ADUUMAIN"), RelationshipType.ACCESSES);

		final var cache = mock(DiscoveryJobCache.class);
		final var identifier = new DependencyBasedOperationTaxonomyIdentifier(mock(ProjectServiceImpl.class), cache);

		when(cache.computeValueIfAbsent(any(), any(), any())).thenReturn(UTILITIES);

		final var result = identifier.identify(tesTDependencyModule);
		assertEquals(2, result.size());
		assertTrue(result.stream().anyMatch(t -> t.e1 == TechnicalTaxonomies.Name.READ && t.e2 == TechnicalTaxonomies.TypeName.DB_ACCESS));
		assertTrue(result.stream().anyMatch(t -> t.e1 == TechnicalTaxonomies.Name.UNLOADS_TABLE && t.e2 == TechnicalTaxonomies.TypeName.OPERATIONS));
	}
}
