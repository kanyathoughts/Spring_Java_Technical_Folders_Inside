/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.importer;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.client.MiningApiClient.moduleService;
import static innowake.mining.client.MiningApiClient.referenceService;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.Functions.FailableRunnable;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.swt.widgets.Shell;
import org.junit.Test;
import org.mockito.ArgumentMatchers;
import org.mockito.Mockito;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.Result;
import innowake.mining.plugin.IntegrationBaseTest;
import innowake.mining.plugin.base.ValidationException;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.SourceMetricsPojo;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ReferenceAttributes;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests {@link DiscoveryExcelImporter} regarding the successful import of Excel files.
 */
public class ExcelImportTest extends IntegrationBaseTest {
	
	private static final Long PROJECT_ID = Long.valueOf(1);
	
	/**
	 * Imports all modules from {@code src/test/resources/innowake/mining/plugin/module/importer/discovery_wmin-496.xlsx}
	 * and counts modules (4000) and references (0).
	 */
	@Test
	public void testParallelImport() {
		doImport("src/test/resources/innowake/mining/plugin/module/importer/discovery_wmin-496.xlsx");
		final ModulePojo[] modules = assertNotNull(getModules());
		
		assertEquals(4000, modules.length);
		
		try {
			int referenceCount = 0;
			for (final ModulePojo module : modules) {
				final Result<ModuleRelationshipPojo[]> moduleResult = referenceService(getConnectionInfo())
						.findAllReferencesForModule()
						.setProjectId(PROJECT_ID)
						.setDirection(RelationshipDirection.OUT)
						.setModuleId(module.identity())
						.execute();
				final Optional<ModuleRelationshipPojo[]> optionalReference = moduleResult.getValue();
				if (optionalReference.isPresent()) {
					final ModuleRelationshipPojo[] references = optionalReference.get();
					referenceCount += references.length;
				}
			}

			assertEquals(0, referenceCount);
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}
	}
	
	/**
	 * Imports all modules an checks whether different modules, 
	 * for ambiguous names but different types/technologies, are created.
	 */
	@Test
	public void testAmbiguousReferences() {
		doImport("src/test/resources/innowake/mining/plugin/module/importer/discovery_wmin-769.xlsx");
		final ModulePojo[] modules = assertNotNull(getModules());
		
		assertEquals(7, modules.length);
		
		try {
			int referenceCount = 0;
			for (final ModulePojo module : modules) {
				final Result<ModuleRelationshipPojo[]> moduleResult = referenceService(getConnectionInfo())
						.findAllReferencesForModule()
						.setProjectId(PROJECT_ID)
						.setDirection(RelationshipDirection.OUT)
						.setModuleId(module.identity())
						.execute();
				final Optional<ModuleRelationshipPojo[]> optionalReference = moduleResult.getValue();
				if (optionalReference.isPresent()) {
					final ModuleRelationshipPojo[] references = optionalReference.get();
					referenceCount += references.length;
				}
			}

			assertEquals(5, referenceCount);
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}
	}

	/**
	 * Imports all modules from {@code src/test/resources/innowake/mining/plugin/module/importer/discovery_2019-04-16_09-08-16.xlsx}
	 * and counts modules and references.
	 */
	@Test
	public void testTheRealDeal() {
		doTestBoth("src/test/resources/innowake/mining/plugin/module/importer/discovery_2019-04-16_09-08-16.xlsx", () -> {
			final ModulePojo[] modules = assertNotNull(getModules());
			
			/*
			 * 120 actual modules
			 * 8 missing modules
			 */
			assertEquals(129, modules.length);
			
			int referenceCount = 0;
			for (final ModulePojo module : modules) {
				
				if ("MMRS7101".equals(module.getName())) {
					final Optional<SourceMetricsPojo> sourceMetrics = module.getSourceMetrics();
					assertTrue("SourceMetrics must be present", sourceMetrics.isPresent());
					assertEquals(Integer.valueOf(3), sourceMetrics.get().getComplexityMcCabe());
					assertEquals(Integer.valueOf(107), sourceMetrics.get().getCodeLines());
					assertEquals(Integer.valueOf(46), sourceMetrics.get().getCommentLines());
				}
				
				final Result<ModuleRelationshipPojo[]> moduleResult = referenceService(getConnectionInfo())
						.findAllReferencesForModule()
						.setProjectId(PROJECT_ID)
						.setDirection(RelationshipDirection.OUT)
						.setModuleId(module.identity())
						.execute();
				
				final ModuleRelationshipPojo[] references = moduleResult.getValue().get();
				referenceCount += references.length;
			}
			
			assertEquals(201, referenceCount);
		});
	}
	
	/**
	 * Imports all modules from {@code src/test/resources/innowake/mining/plugin/module/importer/discovery_wmin-140.xlsx}
	 * and checks that a reference with the attributes DB_ACCESS_TYPES is created.
	 */
	@Test
	public void testReferenceAttributesAccessType() {
		doTestBoth("src/test/resources/innowake/mining/plugin/module/importer/discovery_wmin-140.xlsx", () -> {
			final ModulePojo[] modules = assertNotNull(getModules());
			assertEquals(2, modules.length);

			ModuleRelationshipPojo foundReference = null;
			int referenceCount = 0;
			for (final ModulePojo module : modules) {
				final Result<ModuleRelationshipPojo[]> moduleResult = referenceService(getConnectionInfo())
						.findAllReferencesForModule()
						.setProjectId(PROJECT_ID)
						.setDirection(RelationshipDirection.OUT)
						.setModuleId(module.identity())
						.execute();
				
				final ModuleRelationshipPojo[] references = moduleResult.getValue().get();
				referenceCount += references.length;
				if (references.length > 0) {
					foundReference = references[0]; /* exactly one reference is created and expected */
				}
			}
			assertEquals(1, referenceCount);
			final Optional<Map<String, Object>> properties = assertNotNull(foundReference).getProperties();
			assertTrue("Relationship properties must be present", properties.isPresent());

			final String accessTypes = (String) properties.get().get(ReferenceAttributes.DB_ACCESS_TYPES.getReferenceAttributeExcelName());
			assertNotNull(accessTypes);
			final String[] splittedTypes = accessTypes.split(",");
			assertEquals(2, splittedTypes.length);
			assertEquals(splittedTypes[0], DatabaseAccessType.READ.name());
			assertEquals(splittedTypes[1], DatabaseAccessType.OTHER.name());

			final String dbStmts = (String) properties.get().get(ReferenceAttributes.DB_ACCESS_OPERATIONS.getReferenceAttributeExcelName());
			assertEquals(dbStmts, "DECLARE_TABLE");
		});
	}

	/**
	 * Imports all modules from {@code src/test/resources/innowake/mining/plugin/module/importer/discovery_wmin-69.xlsx}
	 * and checks the module path. Path must only be filled when {@code Representation == PHYSICAL}.
	 * This is the expected behavior of {@link DiscoveryExcelImporter} which imports data as it is. 
	 */
	@Test
	public void testPath() {
		doTestServerside("src/test/resources/innowake/mining/plugin/module/importer/discovery_wmin-69.xlsx", () -> {
			
			final ModulePojo[] modules = assertNotNull(getModules());
			/*
			 * 2 actual modules
			 */
			assertEquals(2, modules.length);
			
			int fileCount = 0;
			int fileSection = 0;
			for (int i = 0; i < modules.length; i++) {
				final Optional<Representation> representation = modules[i].getRepresentation();
				assertTrue("Representation must be present", representation.isPresent());

				if (Representation.PHYSICAL.equals(representation.get())) {
					fileCount++;
					assertNotNull(modules[i].getPath());
				}
				if (Representation.VIRTUAL.equals(representation.get())) {
					fileSection++;
					assertTrue(modules[i].getPath().isEmpty());
				}
			}
			
			assertEquals(1, fileCount);
			assertEquals(1, fileSection);
		});
	}
	
	/**
	 * Imports all modules from {@code src/test/resources/innowake/mining/plugin/module/importer/discovery_wmin-100.xlsx}
	 */
	@Test
	public void testIMSImport() {
		doTestBoth("src/test/resources/innowake/mining/plugin/module/importer/discovery_wmin-100.xlsx", () -> {
			final ModulePojo[] modules = assertNotNull(getModules());
			
			assertEquals(3, modules.length);
			
			ModuleRelationshipPojo foundReference = null;
			int referenceCount = 0;
			for (final ModulePojo module : modules) {
				final Result<ModuleRelationshipPojo[]> moduleResult = referenceService(getConnectionInfo())
						.findAllReferencesForModule()
						.setProjectId(PROJECT_ID)
						.setDirection(RelationshipDirection.OUT)
						.setModuleId(module.identity())
						.execute();
				
				final ModuleRelationshipPojo[] references = moduleResult.getValue().get();
				referenceCount += references.length;
				if (references.length > 0) {
					foundReference = references[0]; /* exactly one reference is created and expected */
				}
			}
			assertEquals(1, referenceCount);
			assertEquals(RelationshipType.REFERENCES, assertNotNull(foundReference).getRelationship());
		});
	}
	
	/**
	 * Imports all modules from {@code src/test/resources/innowake/mining/plugin/module/importer/discovery_wmin-174.xlsx}
	 * and checks that a reference of the utility IKJEFT01 is created.
	 */
	@Test
	public void testReferenceIKJEFT01() {
		doTestBoth("src/test/resources/innowake/mining/plugin/module/importer/discovery_wmin-174.xlsx", () -> {
			final ModulePojo[] modules = assertNotNull(getModules());
			
			assertEquals(3, modules.length);
			
			ModuleRelationshipPojo[] foundReferences = null;
			int referenceCount = 0;
			for (final ModulePojo module : modules) {
				final Result<ModuleRelationshipPojo[]> moduleResult = referenceService(getConnectionInfo())
						.findAllReferencesForModule()
						.setProjectId(PROJECT_ID)
						.setDirection(RelationshipDirection.OUT)
						.setModuleId(module.identity())
						.execute();
				
				final ModuleRelationshipPojo[] references = moduleResult.getValue().get();
				referenceCount += references.length;
				if (references.length > 0) {
					foundReferences = references; /* exactly one reference is created and expected */
				}
			}
			assertNotNull(foundReferences);
			assertEquals(2, referenceCount);
			
			final ModuleRelationshipPojo referenceIndexZero = assertNotNull(foundReferences)[0];
			final ModuleRelationshipPojo referenceIndexOne = assertNotNull(foundReferences)[1];

			assertNotNull(referenceIndexZero);
			assertNotNull(referenceIndexOne);
			
			final ModulePojo[] ikjef01 = moduleService(getConnectionInfo())
					.findModuleByName()
					.setName("IKJEFT01")
					.setProjectId(Long.valueOf(1))
					.execute()
					.getValue()
					.get();
			
			final ModulePojo[] cobolModule = moduleService(getConnectionInfo())
					.findModuleByName()
					.setName("BHP18601")
					.setProjectId(PROJECT_ID)
					.execute()
					.getValue()
					.get();
			final UUID utilityId = ikjef01[0].getUid();
			if (utilityId.equals(assertNotNull(foundReferences)[0].getDstModule())) {
				assertEquals(cobolModule[0].identity(), assertNotNull(foundReferences)[1].getDstModule());
			} else {
				assertEquals(cobolModule[0].identity(), assertNotNull(foundReferences)[0].getDstModule());
				assertEquals(utilityId, assertNotNull(foundReferences)[1].getDstModule());
			}
		});
	}

	/**
	 * Imports all modules from {@code src/test/resources/innowake/mining/plugin/module/importer/discovery_wmin-184.xlsx}
	 * and checks for properties.
	 */
	@Test
	public void testProperties() {
		doTestBoth("src/test/resources/innowake/mining/plugin/module/importer/discovery_wmin-184.xlsx", () -> {
			assertEquals(5, assertNotNull(getModules()).length);
			
			Map<String, Object> properties = getPropertiesForModule("ABCDEF1");
			assertEquals(4, assertNotNull(properties).size());
			assertProperty("IMS_DBD_NAME", "ABCD02PA", assertNotNull(properties));
			assertProperty("IMS_PCB_PROCOPT", "GOTP", assertNotNull(properties));
			assertProperty("IMS_PCB_SENSEG", "EFGHCASE EFGHPART EFGHRELC EFGHHIST ", assertNotNull(properties));
			assertProperty("IMS_PCB_TYPE", "DB", assertNotNull(properties));
			
			properties = getPropertiesForModule("ABCDEF2");
			assertEquals(3, assertNotNull(properties).size());
			assertProperty("DISP", "NEW,CATLG,DELETE", assertNotNull(properties));
			assertProperty("SPACE", "TRK,(1,5)", assertNotNull(properties));
			assertProperty("DCB", "RECFM=F,LRECL=80", assertNotNull(properties));
			
			properties = getPropertiesForModule("ABCDEF3");
			assertEquals(1, assertNotNull(properties).size());
			assertProperty("CALL_TYPE", "LINK,CALL", assertNotNull(properties));
		});
	}

	/**
	 * Imports all modules from {@code src/test/resources/innowake/mining/plugin/module/importer/discovery_wmin-298.xlsx}
	 * and checks for {@code INCLUDES} relationship.
	 */
	@Test
	public void testJclInclude() {
		doTestBoth("src/test/resources/innowake/mining/plugin/module/importer/discovery_wmin-298.xlsx", () -> {
			final ModulePojo[] modules = assertNotNull(getModules());
			assertEquals(2, modules.length);
			
			for (final ModulePojo module : modules) {
				final RelationshipDirection direction;
				if ("ABEND".equals(module.getName())) {
					assertEquals(Storage.FILE, module.getStorage());
					assertEquals(Type.INCLUDE, module.getType());
					assertEquals(Technology.JCL, module.getTechnology());
					assertNotNull(module.getPath());
					direction = RelationshipDirection.IN;
				} else {
					direction = RelationshipDirection.OUT;
				}
				
				final Result<ModuleRelationshipPojo[]> moduleResult = referenceService(getConnectionInfo())
						.findAllReferencesForModule()
						.setProjectId(PROJECT_ID)
						.setDirection(direction)
						.setModuleId(module.identity())
						.execute();
				
				final ModuleRelationshipPojo[] references = moduleResult.getValue().get();
				
				assertEquals(1, references.length);
				assertEquals(RelationshipType.INCLUDES, references[0].getRelationship());
			}
		});
	}
	
	/**
	 * Imports all modules from {@code src/test/resources/innowake/mining/plugin/module/importer/discovery_wmin-271.xlsx}
	 * and counts modules and references. There are 2 mapsets, each linked to a map with the same name and a Resourc_File.
	 */
	@Test
	public void testDuplicateMapName() {
		doTestBoth("src/test/resources/innowake/mining/plugin/module/importer/discovery_wmin-271.xlsx", () -> {
			final ModulePojo[] modules = assertNotNull(getModules());
			
			assertEquals(5, modules.length);
			
			int outgoingReferenceCount = 0;
			for (final ModulePojo module : modules) {
				final Result<ModuleRelationshipPojo[]> moduleResult = referenceService(getConnectionInfo())
						.findAllReferencesForModule()
						.setProjectId(PROJECT_ID)
						.setDirection(RelationshipDirection.OUT)
						.setModuleId(module.identity())
						.execute();
				
				final ModuleRelationshipPojo[] references = moduleResult.getValue().get();
				outgoingReferenceCount += references.length;
			}
			
			assertEquals(2, outgoingReferenceCount);
			
			int incomingReferenceCount = 0;
			for (final ModulePojo module : modules) {
				final Result<ModuleRelationshipPojo[]> moduleResult = referenceService(getConnectionInfo())
						.findAllReferencesForModule()
						.setProjectId(PROJECT_ID)
						.setDirection(RelationshipDirection.IN)
						.setModuleId(module.identity())
						.execute();
				
				final ModuleRelationshipPojo[] references = moduleResult.getValue().get();
				incomingReferenceCount += references.length;
			}
			
			assertEquals(2, incomingReferenceCount);
		});
	}

	/**
	 * Imports all modules from {@code src/test/resources/innowake/mining/plugin/module/importer/discovery_WMIN-657.xlsx}
	 * and counts modules.
	 */
	@Test
	public void testTechnologyC() {
		doTestBoth("src/test/resources/innowake/mining/plugin/module/importer/discovery_WMIN-657.xlsx", () -> {
			final ModulePojo[] modules = assertNotNull(getModules());
			
			assertEquals(2, modules.length);
		});
	}
	
	/**
	 * Imports all modules from {@code src/test/resources/innowake/mining/plugin/module/importer/discovery_WMIN-724.xlsx}
	 * and counts modules.
	 */
	@Test
	public void testTechnologyResourceAndIMS() {
		doTestBoth("src/test/resources/innowake/mining/plugin/module/importer/discovery_wmin-724.xlsx", () -> {
			final ModulePojo[] modules = assertNotNull(getModules());
			assertEquals(8, modules.length);

			for (final ModulePojo module : modules) {
				final String moduleName = module.getName();
				final Storage moduleStorage = module.getStorage();
				final Type moduleType = module.getType();
				final Technology moduleTechnology = module.getTechnology();
				final Result<ModuleRelationshipPojo[]> moduleResult = referenceService(getConnectionInfo()).findAllReferencesForModule().setProjectId(PROJECT_ID)
						.setModuleId(module.identity()).execute();

				final ModuleRelationshipPojo[] references = moduleResult.getValue().get();
				assertEquals(1, references.length);
				if ("Test1".equals(moduleName)) {
					assertEquals(Storage.FILE, moduleStorage);
					assertEquals(Type.LISTCAT, moduleType);
					assertEquals(Technology.RESOURCE, moduleTechnology);
					assertNotNull(module.getPath());
					assertEquals(RelationshipType.ACCESSES, references[0].getRelationship());
				} else if ("CSSL".equals(moduleName)) {
					assertEquals(Storage.QUEUE, moduleStorage);
					assertEquals(Type.TDQ, moduleType);
					assertEquals(Technology.CICS, moduleTechnology);
					assertEquals(RelationshipType.ACCESSES, references[0].getRelationship());
				} else if ("WS-TSQUEUE1".equals(moduleName)) {
					assertEquals(Storage.QUEUE, moduleStorage);
					assertEquals(Type.TSQ, moduleType);
					assertEquals(Technology.CICS, moduleTechnology);
					assertEquals(RelationshipType.ACCESSES, references[0].getRelationship());
				} else if ("SMX97".equals(moduleName)) {
					assertEquals(Storage.FILE, moduleStorage);
					assertEquals(Type.TDFXTRCT, moduleType);
					assertEquals(Technology.IMS, moduleTechnology);
					assertNotNull(module.getPath());
					assertEquals(RelationshipType.REFERENCES, references[0].getRelationship());
				}
			}
		});
	}
	
	/**
	 * Stored procedures are imported having an incoming {@link RelationshipType#CALLS} dependency and the following attributes:
	 * <ul>
	 *  <li>{@link Technology#RESOURCE}
	 *  <li>{@link Type#STORED_PROCEDURE}
	 *  <li>{@link Storage#DATABASE}
	 */
	@Test
	public void testStoredProcedureAsVirtualModuleAndDependency() {
		doTestBoth("src/test/resources/innowake/mining/plugin/module/importer/discovery_wdis-221-stored-procedures.xlsx", () -> {
			final ModulePojo[] modules = assertNotNull(getModules());
			assertEquals(2, modules.length);
			assertEquals(1, referenceService(getConnectionInfo()).findAllReferences().setProjectId(PROJECT_ID).execute().getValue().get().length);
			
			final ModulePojo storedProcedure = moduleService(getConnectionInfo())
					.findModuleByName()
					.setName("MYPROC")
					.setProjectId(PROJECT_ID)
					.execute()
					.getValue()
					.get()[0];
			
			assertEquals(Technology.RESOURCE, storedProcedure.getTechnology());
			assertEquals(Type.STORED_PROCEDURE, storedProcedure.getType());
			assertEquals(Storage.DATABASE, storedProcedure.getStorage());
			
			final Result<ModuleRelationshipPojo[]> incomingReferenceResult = referenceService(getConnectionInfo())
					.findAllReferencesForModule()
					.setProjectId(PROJECT_ID)
					.setDirection(RelationshipDirection.IN)
					.setModuleId(storedProcedure.identity())
					.execute();
			
			final ModuleRelationshipPojo[] incomingReferences = incomingReferenceResult.getValue().get();
			assertEquals(1, incomingReferences.length);
			assertEquals(RelationshipType.CALLS, incomingReferences[0].getRelationship());
		});
	}
	
	/**
	 * Tests if Modules and Dependencies sheet gets the locations imported.
	 */
	@Test
	public void testLocationsInModulesAndDependencies() {
		doImportServerside("src/test/resources/innowake/mining/plugin/module/importer/discovery_wmin-2673_2.xlsx");
		final ModulePojo[] modules = assertNotNull(getModules());
		
		assertEquals(2, modules.length);
		final AtomicInteger referenceCount = new AtomicInteger();
		Arrays.stream(modules).forEach(module -> {
			final Optional<ModuleLocation> location = module.getLocation();
			assertTrue("Module location must be present", location.isPresent());
			assertEquals(0, location.get().getOffset().intValue());
			assertTrue(location.get().getLength().intValue() > 0);
			
			try {
				final Result<ModuleRelationshipPojo[]> referenceResult = referenceService(getConnectionInfo())
						.findAllReferencesForModule()
						.setProjectId(PROJECT_ID)
						.setDirection(RelationshipDirection.OUT)
						.setModuleId(module.identity())
						.execute();

				final Optional<ModuleRelationshipPojo[]> optionalReference = referenceResult.getValue();
				if (optionalReference.isPresent()) {
					final ModuleRelationshipPojo[] references = optionalReference.get();
					Arrays.stream(references).forEach(reference -> {
						referenceCount.incrementAndGet();
						final Optional<ModuleLocation> srcLocation = reference.getSrcLocation();
						assertTrue("Source location must be present", srcLocation.isPresent());
						assertEquals(-1, srcLocation.get().getOffset().intValue());
						assertEquals(-1, srcLocation.get().getLength().intValue());
						final Optional<ModuleLocation> dstLocation = reference.getDstLocation();
						assertTrue("Destination location must be present", srcLocation.isPresent());
						assertEquals(0, dstLocation.get().getOffset().intValue());
						assertTrue(dstLocation.get().getLength().intValue() > 0);
					});
				}
			} catch (final IOException e) {
				fail(e.getMessage());
			}

		});

		assertEquals(1, referenceCount.get());
	}
	
	private void doTestBoth(final String path, final FailableRunnable<Exception> runnable) {
		deleteAllModules();
		doTest(path, runnable);
		doTestServerside(path, runnable);
	}
	
	private void doTest(final String path, final FailableRunnable<Exception> runnable) {
		doImport(path);
		try {
			runnable.run();
		} catch (final Exception e) {
			throw new IllegalStateException(e);
		}
	}
	
	private void doTestServerside(final String path, final FailableRunnable<Exception> runnable) {
		doImportServerside(path);
		try {
			runnable.run();
		} catch (final Exception e) {
			throw new IllegalStateException(e);
		}
	}

	private void doImport(final String path) {
		final IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject("mining-test-plugin");
		try {
			DiscoveryExcelImporter.doImport(
					path, 
					PROJECT_ID, 
					getConnectionInfo(),
					value -> Boolean.TRUE,
					new NullProgressMonitor(),
					Optional.of(project),
					new Shell());
		} catch (final ValidationException e) {
			throw new IllegalStateException(e);
		}
	}

	private void doImportServerside(final String path) {
		final String pathAsString = Paths.get(System.getProperty("user.dir"), path).toString();
		try {
			DiscoveryExcelValidator.validateAndReturnModulePaths(pathAsString);
		} catch (final ValidationException e) {
			throw new IllegalStateException(e);
		}

		deleteAllModules();
		
		try (final FileInputStream inputStream = FileUtils.openInputStream(new File(pathAsString))) {
			final Result<Void> result = MiningApiClient.ioService(getConnectionInfo())
					.importExcel()
					.setProjectId(PROJECT_ID)
					.setInputStreamId(path)
					.setInputStream(inputStream)
					.execute();

			if ( ! result.isValid()) {
				throw new IllegalStateException(result.getExtendedStatusMessage());
			}
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}
	}

	private void deleteAllModules() {
		try {
			final Result<Void> result = MiningApiClient.moduleService(getConnectionInfo())
					 .deleteAllModules()
					 .setProjectId(PROJECT_ID)
					 .execute();
			if ( ! result.isValid()) {
				throw new IllegalStateException(String.format("Could not delete modules for project:%n%s", result.getExtendedStatusMessage()));
			}
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}
	}

	@Nullable
	private ModulePojo[] getModules() {
		try {
			final Optional<ModulePojo[]> optionalModules = moduleService(getConnectionInfo())
					.findAllModules()
					.setProjectId(PROJECT_ID)
					.execute()
					.getValue();
			if (optionalModules.isPresent()) {
				return optionalModules.get();
			}
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}
		return new ModulePojo[0];
	}

	private Map<String, Object> getPropertiesForModule(final String moduleName) throws IOException {
		final Optional<ModulePojo[]> abcdef1Optional = moduleService(getConnectionInfo())
				.findModuleByName()
				.setName(moduleName)
				.setProjectId(PROJECT_ID)
				.execute()
				.getValue();

		if (abcdef1Optional.isPresent()) {
			final ModulePojo[] abcdef1 = abcdef1Optional.get();
			assertEquals(1, abcdef1.length);
			
			final Optional<ModuleRelationshipPojo[]> referencesOptional = referenceService(getConnectionInfo())
					.findAllReferencesForModule()
					.setProjectId(PROJECT_ID)
					.setDirection(RelationshipDirection.OUT)
					.setModuleId(abcdef1[0].identity())
					.execute()
					.getValue();
			if (referencesOptional.isPresent()) {
				final ModuleRelationshipPojo[] references = referencesOptional.get();
				assertEquals(1, references.length);
				
				final Optional<Map<String, Object>> properties = references[0].getProperties();
				assertTrue(properties.isPresent());
				return properties.get();
			}
		}
		return Collections.emptyMap();
	}
	
	private void assertProperty(final String key, final Object value, final Map<String, Object> properties) {
		assertTrue(properties.containsKey(key));
		assertEquals(value, properties.get(key));
	}
	
	/**
	 * Imports all modules from {@code src/test/resources/innowake/mining/plugin/module/importer/discovery_15_2022-02-28_16-19-52.xlsx} and also attach the
	 * source code with module if it is already exists with the same path and checks whether the modules and source code attached or not.
	 */
	@Test
	public void testSourceCodeImportWithMetrics() {
		final String path1 = "src/easytrieve/YD0SC015.ezt";
		final String path2 = "src/jcl/jobs/YDECSD0S.job";
		final String content1 = "blah";
		final String content2 = "blub";
		final IFile file1 = mockFile(path1, content1);
		final IFile file2 = mockFile(path2, content2);
		final List<IFile> iFiles = Arrays.asList(file1, file2);
		final List<String> paths = Arrays.asList(path1, path2);

		doImport("src/test/resources/innowake/mining/plugin/module/importer/discovery_15_2022-02-28_16-19-52.xlsx", iFiles, paths);
		final ModulePojo[] modules = assertNotNull(getModules());
		assertEquals(5, modules.length);
		assertEquals(content1, SourceObjectImportUtility.getModuleSource(path1));
		assertEquals(content2, SourceObjectImportUtility.getModuleSource(path2));
	}
	
	private void doImport(final String path, final List<IFile> mockFiles, final List<String> paths) {
		final IProject mockProject = Mockito.mock(IProject.class);
		doReturn(TEST_PROJECT_NAME).when(mockProject).getName();
		final File tempDirectory = new File(StringUtils.EMPTY);
		Mockito.when(assertNotNull(mockProject).getWorkingLocation(ArgumentMatchers.anyString())).thenReturn(new Path(tempDirectory.getAbsolutePath()));
		for (int i = 0; i < mockFiles.size(); i++) {
			Mockito.when(assertNotNull(mockProject).getFile(paths.get(i))).thenReturn(mockFiles.get(i));
		}
		try {
			DiscoveryExcelImporter.doImport(
					path, 
					PROJECT_ID, 
					getConnectionInfo(),
					value -> Boolean.TRUE,
					new NullProgressMonitor(),
					Optional.of(mockProject),
					new Shell());
		} catch (final ValidationException e) {
			throw new IllegalStateException(e);
		}
	}
	
	private IFile mockFile(final String path, final String content) {
		final IFile mock = mock(IFile.class);
		doReturn(Boolean.TRUE).when(mock).exists();
		doReturn(Path.fromOSString(path)).when(mock).getProjectRelativePath();
		final IPath mockPath = mock(IPath.class);
		doReturn(new File("temp")).when(mockPath).toFile();
		doReturn(mockPath).when(mock).getLocation();
		try {
			doReturn(IOUtils.toInputStream(content, StandardCharsets.UTF_8)).when(mock).getContents();
		} catch (final CoreException e) {
			throw new IllegalStateException(e);
		}
		return mock;
	}
	
	/**
	 * Tests the backwards compatibility of the {@link DiscoveryExcelValidator} for LoC metrics.
	 * In WMIN-7099 the LoC metrics column headers were changed.
	 */
	@Test
	public void testImportLocCompatilityHeaders() {
		doTestBoth("src/test/resources/innowake/mining/plugin/module/importer/discovery-with-old-loc-headers.xlsx", () -> {
			final ModulePojo[] modules = assertNotNull(getModules());
			assertEquals(5, modules.length);
		});
	}
	
	/**
	 * Tests the backwards compatibility of discovery XLSX import without new column Relationship.
	 * In WMIN-8830 the Dependency metrics column headers were changed.
	 */
	@Test
	public void testDiscoveryExcelImportWithoutRelationship() {
		doTestBoth("src/test/resources/innowake/mining/plugin/module/importer/import-discovery-WMIN8830A.xlsx", () -> 
			testExcelImportRelationship(RelationshipType.ACCESSES)
		);
	}
	
	/**
	 * Tests import of discovery XLSX import files with new column Relationship.
	 * In WMIN-8830 the Dependency metrics column headers were changed.
	 */
	@Test
	public void testDiscoveryExcelImportWithRelationship() {
		doTestBoth("src/test/resources/innowake/mining/plugin/module/importer/import-discovery-WMIN8830B.xlsx", () -> 
			testExcelImportRelationship(RelationshipType.CALLS)
		);
	}
	
	private void testExcelImportRelationship(final RelationshipType target) {
		try {
			final ModulePojo[] modules = assertNotNull(getModules());
			assertEquals(8, modules.length);
			final List<ModulePojo> module = Arrays.asList(modules).stream().filter(obj -> obj.getName().equals("MMRS00C.A.LOADLIB")).collect(Collectors.toList());
			assertEquals(1, module.size());
			final Result<ModuleRelationshipPojo[]> moduleResult = referenceService(getConnectionInfo())
					.findAllReferencesForModule()
					.setProjectId(PROJECT_ID)
					.setDirection(RelationshipDirection.BOTH)
					.setModuleId(module.iterator().next().identity())
					.execute();
			final Optional<ModuleRelationshipPojo[]> optionalReference = moduleResult.getValue();
			assertTrue(optionalReference.isPresent());
			if (optionalReference.isPresent()) {
				final ModuleRelationshipPojo[] references = optionalReference.get();
				assertEquals(1, references.length);
				assertEquals(target, references[0].getRelationship());
			}	
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}
	}
}
