/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;
import org.springframework.security.test.context.support.WithMockUser;

import innowake.mining.data.model.discovery.ModelDependency;
import innowake.mining.server.discovery.metrics.IncrementalScanUtility;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ErrorMarkerPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.discovery.ErrorKey;

/**
 * Tests for discovery on a project with incremental changes
 */
@WithMockUser
class IncrementalDiscoveryTest extends BaseIncrementalDiscoveryTest {

	/**
	 * This test has many files with creation, deletion and changes.
	 * @throws IOException IOException
	 */
	@Test
	void testWithManyChanges() throws IOException {
		doTest("migrateV0", "migrateV1");
	}

	/**
	 * Migration test with fewer files.
	 * @throws IOException IOException
	 */
	@Test
	void testMinimal() throws IOException {
		doTest("minimalV0", "minimalV1");
	}

	/**
	 * Test interaction between JCL PROC and JOB files
	 * @throws IOException IOException
	 */
	@Test
	void testJclProc() throws IOException {
		doTest("jclProcV0", "jclProcV1");
	}

	/**
	 * Test where missing dependencies are added.
	 * @throws IOException IOException
	 */
	@Test
	void testCobolAddMissingDependencies() throws IOException {
		doTest("cobolMissingDependenciesV0", "cobolMissingDependenciesV1");
	}

	/**
	 * Test where dependencies are deleted.
	 * @throws IOException IOException
	 */
	@Test
	void testCobolDeleteDependencies() throws IOException {
		doTest("cobolMissingDependenciesV1", "cobolMissingDependenciesV0");
	}
	
	/**
	 * Tests for JOBLIB dependencies for JCL
	 * @throws IOException
	 */
	@Test
	void testJCLJobLibDependencies() throws IOException {
		doTest("WMIN7641AV0", "WMIN7641AV1");
	}
	
	/**
	 * Tests for STEPLIB dependencies for JCL
	 * @throws IOException
	 */
	@Test
	void testJCLStepLibDependencies() throws IOException {
		doTest("WMIN7641BV0", "WMIN7641BV1");
	}

	/**
	 * Tests for JAVA that dependencies are created if the first run uploads module A which has a dependency to module B but B is
	 * only uploaded in the second run.
	 * @throws IOException IOException
	 */
	@Test
	void testWmin2784() throws IOException {
		doTest("WMIN2784V0", "WMIN2784V1");
	}
	
	/**
	 * Tests for deleting the missing module that created in first run, when uploaded a source file with same name, type and technology in the second run.
	 * 
	 * @throws IOException IOException
	 */
	@Test
	void testWmin4839() throws IOException {
		final Consumer<EntityId> firstRunDiscovery = projectId -> {
			final Long missingModulesCount = getMissingModulesCount(projectId, "INDB1");
			assertEquals(1, missingModulesCount);
		};
		final Consumer<EntityId> secondRunDiscovery = projectId -> {
			final Long missingModulesCount = getMissingModulesCount(projectId, "INDB1");
			assertEquals(0, missingModulesCount);
		};
		doTest("WMIN4839V0", "WMIN4839V1", firstRunDiscovery, secondRunDiscovery);
	}
	
	/**
	 * Tests whether the missing natural modules that uses language code recollected during the second run and resolved dependencies with the latest modules.
	 * 
	 * @throws IOException IOException
	 */
	@Test
	void testWmin4839A() throws IOException {
		final Consumer<EntityId> firstRunDiscovery = projectId -> {
			final Long missingModulesCount = getMissingModulesCount(projectId, "MAP&");
			assertEquals(1, missingModulesCount);
		};
		final Consumer<EntityId> secondRunDiscovery = projectId -> {
			final Long missingModulesCount = getMissingModulesCount(projectId, "MAP&");
			assertEquals(0, missingModulesCount);
		};
		doTest("WMIN4839AV0", "WMIN4839AV1", firstRunDiscovery, secondRunDiscovery);
	}

	/**
	 * Tests that virtual modules and dependencies are deleted as well if they have no incoming references anymore.
	 * @throws IOException IOException
	 */
	@Test
	void testWmin2804() throws IOException {
		doTest("WMIN2804V0", "WMIN2804V1");
	}

	/**
	 * Tests that modules and dependencies are re-created properly for batch procs and resource files.
	 * 
	 * <p>In the test TESTJOBA and TESTPRC have not changed. TESTPRC calls NATPRG which has the identification MISSING. Since NATPRG is missing the incremental
	 * change check selects it for a re-scann. As TESTPRC calls NATPRG, TESTPRC is selected as well which means that all its dependencies are deleted. However
	 * the {@code BatchContributor} collect metrics for procs only, when a job is scanned which calls this proc. So when a procs gets selected for a re-scan,
	 * the incremental change check also has to select the calling jobs.</p>
	 * <pre>
	 *  TESTJOBA -----> TESTPRC -----> NATPRG 
	 * (unchanged)    (unchanged)     (missing)
	 * </pre>
	 * Same issue exists if a job / cobol program is calling another cobol program, that has a dependency to target with identification MISSING.
	 * @throws IOException IOException
	 */
	@Test
	void testWmin2818A() throws IOException {
		doTest("WMIN2818AV0", "WMIN2818AV1");
	}

	/**
	 * Tests that the {@code BatchResourceCollector} doesn't create modules for DDs having '&' in their name which get lost during the incremental discovery.
	 * The issue was fixed in WMIN-2849. This test ensures that if {@code BatchResourceCollector} changes, we have a test for the missing modules issue.
	 * @throws IOException IOException
	 */
	@Test
	void testWmin2818B() throws IOException {
		doTest("WMIN2818BV0", "WMIN2818BV1");
	}

	/**
	 * Tests that the incremental discovery deletes undiscovered CSD_LIST modules.
	 * @throws IOException IOException
	 */
	@Test
	void testWMIN2886() throws IOException {
		/* Using custom Discovery_Config.xml to set the "CSD_PARSER_DUMPXML" config property to "false", because otherwise an unintended
		 * "file.xml" is showing up as a Module on the second run.
		 */
		doTest("WMIN2886V0", "WMIN2886V1");
	}

	/**
	 * <p>CSD: Tests that the incremental discovery deletes undiscovered for CSD_EXTRACT modules.</p>
	 * <p>Batch: Tests if PROCs are deleted if there are no calling JOBs.</p>
	 * @throws IOException IOException
	 */
	@Test
	void testWMIN2886B() throws IOException {
		doTest("WMIN2886BV0", "WMIN2886BV1");
	}

	/**
	 * Tests that the module {@code MMRS7111} got scanned in the first run but not deleted and re-scanned in the second, incremental scan. In both runs the
	 * copybook is missing.
	 * 
	 * @throws IOException IOException
	 */
	@Test
	void testWMIN2954() throws IOException {
		final Set<UUID> moduleUids = new HashSet<>();
		final Consumer<EntityId> firstRunDiscovery = projectId -> {
			final List<ModulePojo> modules = moduleService.findModules(builder -> builder.ofProject(projectId));
			assertEquals(3, modules.size(), "Project should have 3 modules");
			/* Collect RIDs of all scanned modules */
			modules.forEach(module -> {
				assertNotNull(module.getUid(), "Module UID must not be null");
				moduleUids.add(module.getUid());
			});
		};
		final Consumer<EntityId> secondRunDiscovery = projectId -> {
			final List<ModulePojo> modules = moduleService.findModules(builder -> builder.ofProject(projectId));
			assertEquals(moduleUids.size(), modules.size(), "Project should have same number of modules after incremental scan");
			/* Remove RIDs of all existing module */
			modules.forEach(module -> assertTrue(moduleUids.remove(module.getUid()), "UID must be present in the module UID collection"));
			/* set is empty if no module got deleted and re-scanned */
			assertTrue(moduleUids.isEmpty(), "The module RID collection must be empty");
		};

		doTest("WMIN2954V0", "WMIN2954V1", firstRunDiscovery, secondRunDiscovery);
	}

	/**
	 * Tests that the Cobol missing {@code COPYBOOK}s that were created in the first run get deleted and re-scanned in the second, incremental scan as
	 * {@code COPYPROC}s since the {@code COPYLIB}s are present then.
	 *
	 * @throws IOException IOException
	 */
	@Test
	void testWMIN2954A() throws IOException {
		doTest("WMIN2954AV0", "WMIN2954AV1");
	}

	/**
	 * Tests that the Cobol {@code COPYPROC}s that were created in the first run get deleted and re-scanned in the second, incremental scan as {@code COPYBOOK}s
	 * since the {@code COPYLIB} is missing then.
	 *
	 * @throws IOException IOException
	 */
	@Test
	void testWMIN2954B() throws IOException {
		doTest("WMIN2954BV0", "WMIN2954BV1");
	}

	/**
	 * Tests that the PL1 {@code COPYBOOK} {@code BAS001B} gets deleted and re-scanned correctly. The SourceObject of the PL {@code COPYBOOK} has the type
	 * {@code PROGRAM}.
	 *
	 * @throws IOException IOException
	 */
	@Test
	void testWMIN2954C() throws IOException {
		doTest("WMIN2954CV0", "WMIN2954CV1");
	}

	/**
	 * Cobol: Tests that the incremental discovery deletes all virtual {@link ModelDependency ModelDependencies} of copybooks,
	 * if the copybook is not included by any other module.
	 * 
	 * @throws IOException IOException
	 */
	@Test
	void testWMIN2961() throws IOException {
		doTest("WMIN2961V0", "WMIN2961V1");
	}
	
	/**
	 * Test that the modules created with RESOURCE and VSAM_FILE of technology and type respectively and establish dependencies properly,
	 * if we add respective files in second run.
	 * 
	 * @throws IOException IOException
	 */
	@Test
	void testWMIN7663A() throws IOException {
		doTest("WMIN7663AV0", "WMIN7663AV1");
	}
	
	/**
	 * Test that the modules related to the CSD technology are re scanned properly and established dependencies, 
	 * if we change existing files in second run.
	 * 
	 * @throws IOException IOException
	 */
	@Test
	void testWMIN7663B() throws IOException {
		doTest("WMIN7663BV0", "WMIN7663BV1");
	}
	
	/**
	 * Tests that the modules that are having errors with UNDISCOVERED_DEPENDENCY key have deleted and re-scanned correctly.
	 *
	 * @throws IOException IOException
	 */
	@Test
	void testWMIN6252() throws IOException {
		final Consumer<EntityId> firstRunDiscovery = projectId -> {
			final List<String> modelErrorCauses = getModelErrorsCause(projectId);
			assertTrue(modelErrorCauses.contains("Unable to resolve file TESTSYSIN to actual data set."));
		};
		final Consumer<EntityId> secondRunDiscovery = projectId -> assertTrue(getModelErrorsCause(projectId).isEmpty());
		doTest("WMIN6252V0", "WMIN6252V1", firstRunDiscovery, secondRunDiscovery);
	}
	
	/**
	 * Tests that the modules that are created in first run should not be deleted, if that module and its dependent module updated/deleted 
	 * in second run of incremental scan.
	 *
	 * @throws IOException IOException
	 */
	@Test
	void testWMIN6254() throws IOException {
		final Set<UUID> moduleUids = new HashSet<>();
		final Consumer<EntityId> firstRunDiscovery = projectId -> {
			final List<ModulePojo> modules = moduleService.findModules(builder -> builder.ofProject(projectId));
			assertEquals(7, modules.size(), "Project should have 7 modules");
			/* Collect RIDs of all scanned modules */
			modules.forEach(module -> {
				assertNotNull(module.getUid(), "Module UID must not be null");
				moduleUids.add(module.getUid());
			});
		};
		final Consumer<EntityId> secondRunDiscovery = projectId -> {
			final List<ModulePojo> modules = moduleService.findModules(builder -> builder.ofProject(projectId));
			assertEquals(moduleUids.size(), modules.size(), "Project should have same number of modules after incremental scan");
		};

		doTest("WMIN6254V0", "WMIN6254V1", firstRunDiscovery, secondRunDiscovery);
	}

	/**
	 * Tests that file is removed from the "Undiscovered" sheet if it has been removed from the project.
	 * @throws IOException IOException
	 */
	@Test
	void testUndiscovered() throws IOException {
		final Consumer<EntityId> firstRunVerifier = projectId -> {
			assertEquals(1, moduleService.countUndiscovered(q -> q.ofProject(projectId)));
		};
		final Consumer<EntityId> secondRunVerifier = projectId -> {
			assertEquals(0, moduleService.countUndiscovered(q -> q.ofProject(projectId)));
		};

		/* Dirty workaround: "undiscoveredV1" is an empty directory and GIT doesn't commit empty dirs by default */
		final Path v1 = TEST_SOURCE_FOLDER.resolve("undiscoveredV1");
		final boolean exists = Files.exists(v1);
		try {
			if ( ! exists) {
				Files.createDirectories(v1);
			}
			doTest("undiscoveredV0", "undiscoveredV1", firstRunVerifier, secondRunVerifier);
		} finally {
			if ( ! exists) {
				Files.deleteIfExists(v1);
			}
		}
	}
	
	/**
	 * Tests that the that the {@link IncrementalScanUtility} doesn't delete modules with {@code creator = 'API'}.
	 * 
	 * <p>The {@link IncrementalScanUtility} doesn't delete the 'API' module {@code com.demo.Class3}. In the second run when {@code Interface1} is additionally
	 * collected no dependency is therefore created to the {@code com.demo.Interface1} module.</p>
	 * <p>Copy of WMIN2784 test but without dependency:
	 * <pre>
	 * 4.0,"com.demo.Class3",5.0,"com.demo.Interface1","JAVA","JAVA_INTERFACE","EARLY","{}",47.0,10.0,19.0,32.0
	 * </pre>
	 * 
	 * @throws IOException IOException
	 */
	@Test
	void testWmin6537() throws IOException {
		/* Sets the creator to 'API' in all modules after the first Metrics Discovery */
		
		final Consumer<EntityId> setCreator = projectId -> assertEquals(3, moduleService.updateModules(q -> q.ofProject(projectId), 
																										new ModulePojoPrototype()
																												.setCreator(Creator.API)));

		/* Because the Module Class3 is updated with creator=API, the IncrementalScanUtility doesn't delete it and therefore it is not recollected
		 * which results in no dependency between Class3 and Interface1. If the update is not executed the Dependency would be created */
		doTest("WMIN6537V0", "WMIN6537V1", setCreator, null);
	}
	
	/**
	 * Cobol: Tests when a source file is deleted and replaced with the "MISSING" module and incremental scan is performed.
	 * 
	 * @throws IOException IOException
	 */
	@Test
	void testWMIN4843() throws IOException {
		final Map<String, UUID> firstRunDiscoveryModules = new HashMap<>();
		final Consumer<EntityId> firstRunDiscovery = projectId -> {
			final List<ModulePojo> moduleList = moduleService.findModules(builder -> builder.ofProject(projectId));
			assertEquals(2, moduleList.size(), "Project should have 2 modules");
			/* Collect Module Name And Record Id of all scanned modules */
			moduleList.forEach(module -> {
				assertEquals(Identification.IDENTIFIED, module.getIdentification(), "Module must be identified");
				assertNotNull(module.getName(), "Module name must not be null");
				firstRunDiscoveryModules.put(module.getName(), module.getUid());
			});
		};
		final Consumer<EntityId> secondRunDiscovery = projectId -> {
			final List<ModulePojo> moduleList = moduleService.findModules(builder -> builder.ofProject(projectId));
			assertEquals(firstRunDiscoveryModules.size(), moduleList.size(), "Project should have same number of modules after incremental scan");
			ModulePojo prog1Module = moduleList.stream().filter(m -> m.getName().equals("PROG1")).findFirst().get();
			ModulePojo prog2Module = moduleList.stream().filter(m -> m.getName().equals("PROG2")).findFirst().get();
			/* Module is deleted if Record Id of module in second incremental scan is different from first incremental scan */
			assertNotEquals(firstRunDiscoveryModules.get(prog2Module.getName()), prog2Module.getUid(),
					"Module for " + prog2Module.getName() + " is not deleted");
			/* Collect Reference of deleted module */

			final var references = moduleService.findRelationship(q -> q.ofProject(projectId)
																		.ofModuleInDirection(prog2Module.identity(), RelationshipDirection.IN)
																		.withTypes(RelationshipType.DEPENDENCY_TYPES));
			assertFalse(references.isEmpty(), "References must be present for module");

			assertEquals(prog1Module.getUid(), references.get(0).getSrcModule());
			/* Module is rescanned if Record Id of module in second incremental scan is different from first incremental scan */
			assertNotEquals(firstRunDiscoveryModules.get(prog1Module.getName()), prog1Module.getUid(),
					"Module for " + prog1Module.getName() + " is not rescanned");	
		};

		doTest("WMIN4843V0", "WMIN4843V1", firstRunDiscovery, secondRunDiscovery);
	}

	/**
	 * Cobol: Tests whether a missing module is replaced and actual module is getting created and re-scanned correctly.
	 * 
	 * @throws IOException IOException
	 */
	@Test
	void testWMIN4841() throws IOException {
		final Map<String, UUID> firstRunDiscoveryModules = new HashMap<>();
		
		final Consumer<EntityId> firstRunDiscovery = projectId -> {
			final List<ModulePojo> modules = moduleService.findModules(builder -> builder.ofProject(projectId));
			assertEquals(2, modules.size(), "Project should have 2 modules");
			/* Collect all modules to be deleted and referencing module for re-scan */
			final Optional<ModulePojo> prog2Module = modules.stream().filter(module -> module.getName().equals("PROG2")).findAny();
			assertTrue(prog2Module.isPresent());
			assertEquals(Identification.MISSING, prog2Module.get().getIdentification(), prog2Module.get().getName() + " must not be identified");
			assertEquals(Technology.UNKNOWN, prog2Module.get().getTechnology(), "Technology for " + prog2Module.get().getName() + " is not UNKNOWN");
			firstRunDiscoveryModules.put(prog2Module.get().getName(), prog2Module.get().getUid());

			/* Collect Reference of deleted module and mark the reference module for re-scan */
			final var references = moduleService.findRelationship(q -> q.ofProject(projectId)
																		.ofModuleInDirection(prog2Module.get().identity(), RelationshipDirection.IN)
																		.withTypes(RelationshipType.DEPENDENCY_TYPES));
			assertFalse(references.isEmpty(), "References must be present for module");

			final Optional<ModulePojo> prog1Module = modules.stream().filter(module -> module.getName().equals("PROG1")).findAny();
			assertTrue(prog1Module.isPresent());

			assertEquals(prog1Module.get().getUid(), references.get(0).getSrcModule(), "Module for " + prog1Module.get().getName()
							+ " is not marked for re-scan by collecting referencing module for " + prog2Module.get().getName());
			assertEquals(RelationshipType.CALLS, references.get(0).getRelationship(), "Module for PROG1 doesn't have Reference edge to "
					+ prog2Module.get().getName());
			firstRunDiscoveryModules.put(prog1Module.get().getName(), references.get(0).getSrcModule());
		};
		
		final Consumer<EntityId> secondRunDiscovery = projectId -> {
			final List<ModulePojo> modules = moduleService.findModules(builder -> builder.ofProject(projectId));
			assertEquals(firstRunDiscoveryModules.size(), modules.size(), "Project should have same number of modules after incremental scan");
			final Optional<ModulePojo> prog1Module = modules.stream().filter(module -> module.getName().equals("PROG1")).findAny();
			final Optional<ModulePojo> prog2Module = modules.stream().filter(module -> module.getName().equals("PROG2")).findAny();

			assertTrue(prog1Module.isPresent());
			assertTrue(prog2Module.isPresent());

			assertEquals(Identification.IDENTIFIED, prog1Module.get().getIdentification());
			assertEquals(Identification.IDENTIFIED, prog2Module.get().getIdentification());
			
			/* Uid of re-scanned module in second incremental scan is different from first incremental scan as it got marked for re-scan and 
			 * newly created */
			assertNotEquals(firstRunDiscoveryModules.get("PROG1"), prog1Module.get().getUid(),
					"Module for " + prog1Module.get().getName() + " is not re-scanned");

			final var references = moduleService.findRelationship(q -> q.ofProject(projectId)
																		.ofModuleInDirection(prog2Module.get().identity(), RelationshipDirection.IN)
																		.withTypes(RelationshipType.DEPENDENCY_TYPES));
			assertFalse(references.isEmpty(), "References must be present for module");

			assertEquals(prog1Module.get().getUid(),references.get(0).getSrcModule());
			assertEquals(RelationshipType.CALLS, references.get(0).getRelationship(), "Module for " + prog1Module.get().getName() + "doesn't have Call edge to "
					+ prog2Module.get().getName());
			
			/* Record Id of MISSING module in second incremental scan is different from first incremental scan as it got marked for deletion and newly 
			 * created */
			assertNotEquals(firstRunDiscoveryModules.get("PROG2"), prog2Module.get().getUid(),
					"Module for " + prog2Module.get().getName() + " is not deleted");
			};
	
		doTest("WMIN4841V0", "WMIN4841V1", firstRunDiscovery, secondRunDiscovery);
	}

	/**
	 * Tests that Cobol programs that include missing copybooks are not rescanned if a Cobol copylib is present but did not change in the incremental scan.
	 * Additionally checks, that the {@code CobolReadWriteAccessContributor} does not update Cobol modules that were not rescanned in the incremental scan.
	 * 
	 * @throws IOException IOException
	 */
	@Test
	void testWmin8012A() throws IOException {
		final Instant[] fstMetricsDate = new Instant[1];
		final String moduleName = "WMIN8012";

		final Consumer<EntityId> fstRunDiscovery = projectId -> {
			fstMetricsDate[0] = getMetricsDates(projectId, moduleName);

			/* Wait two seconds so we can compare the modules' metrics date */
			try {
				TimeUnit.SECONDS.sleep(2);
			} catch (final InterruptedException e) {
				throw new IllegalStateException(e);
			}
		};

		final Consumer<EntityId> snRunDiscovery = projectId -> {
			final Instant metricsDates = getMetricsDates(projectId, moduleName);
			assertEquals(fstMetricsDate[0], metricsDates, "Metrics date of module WMIN8012 must not have changed");
		};

		doTest("WMIN8012AV0", "WMIN8012AV1", fstRunDiscovery, snRunDiscovery);
	}

	/**
	 * Tests that Cobol programs that include missing copybooks are rescanned if a Cobol copylib is present, which was changed after the first scan.
	 * 
	 * @throws IOException IOException
	 */
	@Test
	void testWmin8012B() throws IOException {
		final Instant[] fstMetricsDate = new Instant[1];
		final String moduleName = "WMIN8012";

		final Consumer<EntityId> fstRunDiscovery = projectId -> {
			fstMetricsDate[0] = getMetricsDates(projectId, moduleName);

			/* Wait two seconds so we can compare the modules' metrics date */
			try {
				TimeUnit.SECONDS.sleep(2);
			} catch (final InterruptedException e) {
				throw new IllegalStateException(e);
			}
		};

		final Consumer<EntityId> snRunDiscovery = projectId -> {
			final Instant metricsDates = getMetricsDates(projectId, moduleName);
			assertNotEquals(fstMetricsDate[0], metricsDates, "Metrics date of module WMIN8012 must have changed");
		};

		doTest("WMIN8012BV0", "WMIN8012BV1", fstRunDiscovery, snRunDiscovery);
	}

	/**
	 * Test that the modules created with RESOURCE and RESOURCE_GDG_FILE of technology and type respectively and establish dependencies properly,
	 * if we add respective files in second run.
	 *
	 * @throws IOException IOException
	 */
	@Test
	void testWMIN7647A() throws IOException {
		doTest("WMIN7647V0", "WMIN7647V1");
	}
	
	/**
	 * Tests that the missing module will be ignored and create a new missing module while resolving dependency for COBOL with EXEC CICS XCTL,
	 * when we add respective files in 2 runs.
	 * 
	 * @throws IOException IOException
	 */
	@Test
	void testWmin8490() throws IOException {
		doTest("WMIN8490V0", "WMIN8490V1");
	}

	private Instant getMetricsDates(final EntityId projectId, final String moduleName) {
		final List<ModulePojo> modules = moduleService.findModules(b -> b.ofProject(projectId).withName(moduleName));
		assertEquals(1, modules.size(), "Number of findings for module must be one: " + moduleName);
		final Instant metricsDate = modules.get(0).getMetricsDate().orElse(null);
		assertNotNull(metricsDate, "Metrics date of module must not be null: " + moduleName);
		return metricsDate;
	}

	/**
	 * Tests discovery on a project, changing the source objects of the projects and running discovery again
	 *
	 * @param v0 folder with the files of the initial project
	 * @param v1 folder with the files of the project in its changes state
	 * @throws IOException in case of error during accessing files
	 */
	private void doTest(final String v0, final String v1) throws IOException {
		doTest(v0, v1, null, null);
	}
	
	private Long getMissingModulesCount(final EntityId projectId, final String moduleName) {
		return Long.valueOf(moduleService.countModules(b -> b.ofProject(projectId).withName(moduleName).withIdentified(false)));
	}
	
	private List<String> getModelErrorsCause(final EntityId projectId) {
		return moduleService.findErrorMarkers(q -> q.ofProject(projectId).withKey(ErrorKey.UNDISCOVERED_DEPENDENCY)).stream()
							.map(ErrorMarkerPojo::getCause)
							.collect(Collectors.toList());
	}

}
