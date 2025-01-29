/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.sql.SQLException;

import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpUriRequest;
import org.junit.jupiter.api.Test;

import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.module.ModuleServiceProvider;
import innowake.mining.data.access.postgres.AstPgDao;
import innowake.mining.shared.entities.ModulePojo;

/**
 * Integration tests concerning AST nodes.
 */
class AstNodeIntegrationTest extends IntegrationTest {

	private final ModuleServiceProvider moduleServiceProvider = MiningApiClient.moduleService(getConnectionInfo());
	
	/**
	 * Tests that deleting all {@linkplain Module Modules} of a project deletes all corresponding AST nodes. 
	 *
	 * @throws IOException see {@link HttpClient#execute(HttpUriRequest)}
	 * @throws SQLException if a database access error occurs
	 */
	@Test
	void astNodesAreDeletedWhenAllModulesAreDeleted() throws IOException, SQLException {
		final Result<ModulePojo> moduleAResult = moduleServiceProvider.findModuleByPath()
				.setPath("src/cobol/programs/PRGA.cbl")
				.setProjectId(Long.valueOf(1))
				.execute();
		final Result<ModulePojo> moduleBResult = moduleServiceProvider.findModuleByPath()
				.setPath("src/cobol/programs/PRGB.cbl")
				.setProjectId(Long.valueOf(1))
				.execute();
		final Result<ModulePojo> moduleEResult = moduleServiceProvider.findModuleByPath()
				.setPath("src/cobol/programs/PRGE.cbl")
				.setProjectId(Long.valueOf(2))
				.execute();
		
		final var astDao = new AstPgDao(getDataSource());
		
		/* Check that no AstNodes exist */
		long astNodeCount = astDao.count(q -> { });
		assertEquals(0, astNodeCount);
		
		final ModulePojo moduleA = moduleAResult.getValue().get();
		/* Call storeAst on Module A */
		moduleServiceProvider.storeAstNodes()
				.setProjectId(moduleA.getProject())
				.setModuleId(moduleA.identity())
				.execute();
		
		final ModulePojo moduleB = moduleBResult.getValue().get();
		/* Call storeAst on Module B */
		moduleServiceProvider.storeAstNodes()
				.setProjectId(moduleB.getProject())
				.setModuleId(moduleB.identity())
				.execute();
		
		final ModulePojo moduleE = moduleEResult.getValue().get();
		/* Call storeAst on Module E */
		moduleServiceProvider.storeAstNodes()
				.setProjectId(moduleE.getProject())
				.setModuleId(moduleE.identity())
				.execute();
		
		/* Check that AstNodes exist */
		astNodeCount = astDao.count(q -> { });
		assertTrue(astNodeCount > 0);
		
		/* Delete all Modules */
		final Result<Void> deletionResult = moduleServiceProvider.deleteAllModules().setProjectId(moduleB.getProject()).execute();
		assertTrue(deletionResult.isValid());
		
		/* Check that no remaining AstNodes all belong to Module E which is part of an other project */
		final var nodes = astDao.find(q -> { });
		astNodeCount = 0;
		for (final var node : nodes) {
			astNodeCount++;
			assertEquals(moduleE.identity(), node.getModule());
		}
		assertNotEquals(0, astNodeCount);
	}
	
	/**
	 * Tests that deleting a single {@link Module} deletes all corresponding AST nodes and only those. 
	 *
	 * @throws IOException see {@link HttpClient#execute(HttpUriRequest)}
	 * @throws SQLException if a database access error occurs
	 */
	@Test
	void astNodesAreDeleteWhenSingleModuleIsDeleted() throws IOException, SQLException {
		final Result<ModulePojo> moduleAResult = moduleServiceProvider.findModuleByPath()
				.setPath("src/cobol/programs/PRGA.cbl")
				.setProjectId(Long.valueOf(1))
				.execute();
		final Result<ModulePojo> moduleBResult = moduleServiceProvider.findModuleByPath()
				.setPath("src/cobol/programs/PRGB.cbl")
				.setProjectId(Long.valueOf(1))
				.execute();
		
		final var astDao = new AstPgDao(getDataSource());
		
		/* Check that no AstNodes exist */
		long astNodeCount = astDao.count(q -> { });
		assertEquals(0, astNodeCount);

		final ModulePojo moduleA = moduleAResult.getValue().get();
		/* Call storeAst on Module A */
		moduleServiceProvider.storeAstNodes()
				.setProjectId(moduleA.getProject())
				.setModuleId(moduleA.identity())
				.execute();

		final ModulePojo moduleB = moduleBResult.getValue().get();
		/* Call storeAst on Module B */
		moduleServiceProvider.storeAstNodes()
				.setProjectId(moduleB.getProject())
				.setModuleId(moduleB.identity())
				.execute();
		
		/* Check that AstNodes exist */
		astNodeCount = astDao.count(q -> { });
		
		/* Delete the Module on which storeAst was called */
		final Result<Void> deletionResult = moduleServiceProvider.deleteModule()
				.setModuleId(moduleB.identity())
				.setProjectId(moduleB.getProject())
				.execute();
		assertTrue(deletionResult.isValid());
		
		/* Check that no remaining AstNodes all belong to Module A, which was not deleted */
		final var nodes = astDao.find(q -> { });
		astNodeCount = 0;
		for (final var node : nodes) {
			astNodeCount++;
			assertEquals(moduleA.identity(), node.getModule());
		}
		assertNotEquals(0, astNodeCount);
	}
}
