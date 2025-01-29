/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.security.test.context.support.WithMockUser;


import innowake.mining.server.discovery.categorize.DiscoverCodeJob;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.FeatureId;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

@WithMockUser
class SqlViewModuleTest extends BaseDiscoveryTest {
	
	@Override
	protected String getTestFolder() {
		return "sqlViewModule";
	}
	
	@BeforeAll
	void initialize() {
		ff4j.enable(FeatureId.INCREMENTAL_SCAN.getId());
	}
	
	@AfterAll
	public void after() {
		ff4j.disable(FeatureId.INCREMENTAL_SCAN.getId());
	}

	@Test
	 void testContainsCobolSqlTableModules() {
		doSqlTableModulesTest(createProject().identity(), "MGOPRGM1", "DEPT");
	}
	
	@Test
	 void testContainsCobolSqlViewModules() {
		doSqlViewModulesTest(createProject().identity(), "MGOPRGM1", "DEPT");
	}

	@Test
	void testContainsPLSqlTableModules() {
		doSqlTableModulesTest(createProject().identity(), "create_table_test", "TESTVIEW");
	}

	@Test
	void testContainsPLSqlViewModules() {
		/*Test for PL SQL for resolving between SQL_VIEW and SQL_TABLE modules with same name*/
		doSqlViewModulesTest(createProject().identity(), "create_table_test", "TESTVIEW");
	}

	@Test
	 void testContainsPl1SqlTableModules() {
		doSqlTableModulesTest(createProject().identity(), "PL1", "LMT.VLMQPA");
	}
	
	@Test
	 void testContainsPl1SqlViewModules() {
		doSqlViewModulesTest(createProject().identity(), "PL1", "LMT.VLMQPA");
	}
	
	@Test
	 void testContainsJclSqlTableModules() {
		doSqlTableModulesTest(createProject().identity(), "JCL.STEP2.EXEC_PGM", "TABLE2");
	}
	
	@Test
	 void testContainsJclSqlViewModules() {
		doSqlViewModulesTest(createProject().identity(), "JCL.STEP2.EXEC_PGM", "TABLE2");
	}
	
	@Test
	 void testContainsEasytrieveSqlTableModules() {
		doSqlTableModulesTest(createProject().identity(), "Easytrieve", "VPOSN_RLSHP");
	}
	
	@Test
	void testContainsEasytieveSqlViewModules() {
		doSqlViewModulesTest(createProject().identity(), "Easytrieve", "VPOSN_RLSHP");
	}
	
	@Test
	void testContainsCSqlTableModules() {
		doSqlTableModulesTest(createProject().identity(), "C", "mytable");
	}
	
	@Test
	void testContainsCSqlViewModules() {
		doSqlViewModulesTest(createProject().identity(), "C", "mytable");
	}
	
	Long performDiscovery(Long projectId) {
		final EntityId entityId = EntityId.of(projectId);
		uploadResources(entityId);
		submitJob(jobManager, tracer, new DiscoverCodeJob(entityId));
		submitJob(jobManager, tracer, new DiscoverMetricsJob(entityId, true));
		return projectId;
	}
	
	private void doSqlTableModulesTest(final EntityId projectId, final String moduleName, final String tableName) {
		performDiscovery(projectId.getNid());

		final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(projectId).withName(moduleName));
		assertTrue(modules.size() > 0);

		final var outgoingDependencies = moduleService.findRelationship(q -> q.ofSource(modules.get(0).identity()).withType(RelationshipType.ACCESSES))
				.stream().map(ModuleRelationshipPojo::getDstModule).collect(Collectors.toList());
		final List<ModulePojo> outgoingModules = moduleService.findModules(b -> b.byUids(outgoingDependencies));
		
		final String actualTableName = outgoingModules.get(0).getName();
		assertEquals(tableName, actualTableName);

		final Technology technology = outgoingModules.get(0).getTechnology();
		assertEquals(Technology.SQL, technology);
		
		final Type type = outgoingModules.get(0).getType();
		assertEquals(Type.TABLE, type);
		
		moduleService.deleteModule(outgoingModules.get(0).identity(), true);
	}
	
	private void doSqlViewModulesTest(final EntityId projectId, final String moduleName, final String tableName) {
		final ModulePojoPrototype sqlViewModule = new ModulePojoPrototype()
				.setName(tableName)
				.setProject(projectId)
				.setTechnology(Technology.SQL)
				.setType(Type.VIEW)
				.setStorage(Storage.DATABASE)
				.setIdentification(Identification.MISSING)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.DISCOVERY)
				.setInfo(Collections.emptyMap());

		moduleService.create(sqlViewModule);
		performDiscovery(projectId.getNid());

		final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(projectId).withName(moduleName));
		final var outgoingDependencies = moduleService.findRelationship(q -> q.ofSource(modules.get(0).identity()).withType(RelationshipType.ACCESSES))
				.stream().map(ModuleRelationshipPojo::getDstModule).collect(Collectors.toList());
		final List<ModulePojo> outgoingModules = moduleService.findModules(b -> b.byUids(outgoingDependencies));

		final List<ModulePojo> updatedModules = moduleService.findModules(b -> b.ofProject(projectId).withName(tableName));
		assertTrue(updatedModules.size() > 0);
		
		final String actualTableName = updatedModules.get(0).getName();
		assertEquals(tableName, actualTableName);

		final Technology technology = outgoingModules.get(0).getTechnology();
		assertEquals(Technology.SQL, technology);

		final Type type = outgoingModules.get(0).getType();
		assertEquals(Type.VIEW, type);
		
		final Storage storage = outgoingModules.get(0).getStorage();
		assertEquals(Storage.DATABASE, storage);
	}
}
