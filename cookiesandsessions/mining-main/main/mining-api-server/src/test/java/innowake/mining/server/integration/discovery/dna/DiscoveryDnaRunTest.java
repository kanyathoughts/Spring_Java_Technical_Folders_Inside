/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.integration.discovery.dna;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.Date;
import java.util.Optional;
import java.util.TimeZone;

import innowake.mining.shared.model.Creator;
import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.test.context.support.WithMockUser;

import com.google.common.io.Files;

import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.data.model.discovery.dna.DnaConfig;
import innowake.mining.data.model.discovery.dna.ModelDna;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.server.service.DnaModelService;
import innowake.mining.shared.access.DnaDataService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.dna.DnaSnapshotPojo;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Tests if {@link DatabaseRelatedTest} for creating data of {@link DnaString}, {@link DnaStringElement}, {@link DnaSimilarity}, {@link DnaCommunity} and
 * {@link DnaSnapshot} for providing {@link ModelDna} for the given source code for different types of {@link DiscoveryDnaRunType} runs.
 *
 */
@WithMockUser
class DiscoveryDnaRunTest extends DiscoverDnaJobTest {

	private static final String TEST_FOLDER = Paths.get("DNA/WMIN4599").toString();
	private static final TimeZone DEFAULT_TIMEZONE = TimeZone.getDefault();
	
	@Autowired
	protected DnaDataService dnaData;
	@Autowired
	protected DnaModelService dnaModel;

	/**
	 * Test to check auto wiring.
	 */
	@Test
	void autowiredNotNullTest() {
		assertNotNull(projectService);
		assertNotNull(moduleService);
		assertNotNull(dnaData);
	}
	
	private EntityId getTestProjectId() {
		return assertNotNull(projectId);
	}

	/**
	 * Tests the new discovery DNA run.
	 */
	@Test
	void testDiscoveryDnaForNew() {
		assertCount(1, 4, 198, 2, 0);
	}

	/**
	 * Tests the discovery DNA rerun with metric changes.
	 *
	 *  @throws IOException
	 */
	@Test
	void testDiscoveryDnaRerunWithMetricChange() throws IOException {
		final long oneMinInMillis = 60000L;
		final Date metricsDate = new Date(new Date().getTime() + oneMinInMillis);
		/* creating sample Module 1 */
		createSampleModule("WMIN5309", "DPB011.cbl", metricsDate);
		/* creating sample Module 2 */
		createSampleModule("WMIN5309", "DPB031.cbl", metricsDate);
		updateProjectConfigurationsAndRunDna("0.2", Optional.of(metricsDate));
		assertCount(2, 8, 738, 4, 4);
	}

	/**
	 * Tests the discovery DNA rerun with configuration changes.
	 */
	@Test
	void testDiscoveryDnaRerunWithConfigChange() {
		updateProjectConfigurationsAndRunDna("0.4", Optional.empty());
		assertCount(2, 4, 198, 2, 1);
		final DnaSnapshotPojo dnaSnapshotAfterRerun = dnaData.latestSnapshot(getTestProjectId()).get();
		assertEquals(0.4, DnaConfig.fromMap(dnaSnapshotAfterRerun.getDnaConfig()).getSimilarityThreshold());
	}

	/**
	 * Tests the discovery DNA rerun without any configuration or metric changes.
	 */
	@Test
	void testDiscoveryDnaRerunWithNoConfigAndNoMetricsChange() {
		final DnaSnapshotPojo dnaSnapshotBeforeRerun = dnaData.latestSnapshot(getTestProjectId()).get();
		updateProjectConfigurationsAndRunDna("0.85", Optional.empty());
		
		assertCount(1, 4, 198, 2, 0);
		final DnaSnapshotPojo dnaSnapshotAfterRerun = dnaData.latestSnapshot(getTestProjectId()).get();
		assertEquals(dnaSnapshotAfterRerun.getId(), dnaSnapshotBeforeRerun.getId());
		assertTrue(dnaSnapshotAfterRerun.getUpdated().isAfter(dnaSnapshotBeforeRerun.getUpdated()));
	}

	/**
	 * Tests the discovery DNA rerun with the previously executed configuration.
	 */
	@Test
	void testDiscoveryDnaRerunWithPreviousConfig() {
		final DnaSnapshotPojo dnaSnapshotBeforeRerun = dnaData.latestSnapshot(getTestProjectId()).get();
		updateProjectConfigurationsAndRunDna("0.4", Optional.empty());
		/* changing the configurations of the project and Rerun Discovery DNA job with previous configurations */
		updateProjectConfigurationsAndRunDna("0.85", Optional.empty());
		assertCount(2, 4, 198, 2, 0);
		final DnaSnapshotPojo dnaSnapshotAfterRerun = dnaData.latestSnapshot(getTestProjectId()).get();
		assertEquals(dnaSnapshotAfterRerun.getId(), dnaSnapshotBeforeRerun.getId());
		assertTrue(dnaSnapshotAfterRerun.getUpdated().isAfter(dnaSnapshotBeforeRerun.getUpdated()));
		assertEquals(0.85, DnaConfig.fromMap(dnaSnapshotAfterRerun.getDnaConfig()).getSimilarityThreshold());
	}

	/**
	 * Tests the deletion of DNA entities like {@link DnaSnapshot}, {@link DnaString}, {@link DnaStringElement}, {@link DnaSimilarity} and {@link DnaCommunity},
	 * when there is an error occurred during the process of discover DNA for the current run.
	 *
	 * @throws IOException
	 */
	@Test
	void testDeletionOfDnaEntitiesForCurrentRunOnFailure() throws IOException {
		assertCount(1, 4, 198, 2, 0);
		/* deleting all the modules for the current project before triggering the rerun */
		moduleService.deleteModules(assertNotNull(projectId), true, false);
		final long oneMinInMillis = 60000L;
		final Date metricsDate = new Date(new Date().getTime() + oneMinInMillis);
		/* creating only one sample Module */
		createSampleModule("WMIN5117", "M12428B.pl1", metricsDate);
		updateProjectConfigurationsAndRunDna("0.85", Optional.of(metricsDate));
		/* since the error occurred in rerun so, we will be having DNA entities regarding to the previous run */
		assertCount(0, 0, 0, 0, 0);
	}

	/**
	 * Tests the discovery DNA for the {@link Modules}s, which are having valid number of {@link DnaString}s present for some {@link SequrncerId}s and
	 * invalid number of {@link DnaString}s present for some other {@link SequrncerId}s.
	 * For example: If there is only one {@link DnaString} present for one {@link SequrncerId} and valid number of {@link DnaString}s present for other
	 * {@link SequrncerId}s, in this case, we will process the discovery DNA for {@link SequrncerId}s which are having valid number of {@link DnaString}s and
	 * show a warning message for invalid one.
	 * @throws IOException
	 */
	@Test
	void testDiscoveryDnaByAddingSingleFileForSequencerId() throws IOException{
		assertCount(1, 4, 198, 2, 0);
		final long oneMinInMillis = 60000L;
		final Date metricsDate = new Date(new Date().getTime() + oneMinInMillis);
		/* creating only one sample Module */
		createSampleModule("WMIN5117", "M12428B.pl1", metricsDate);
		updateProjectConfigurationsAndRunDna("0.2", Optional.of(metricsDate));
		assertCount(2, 6, 198, 2, 2);
	}

	/**
	 * Tests the deletion of all DNA related entities like {@link DnaString}, {@link DnaStringElement}, {@link DnaSimilarity} on deletion of all
	 * {@link Modules}s for the given {@link Project}.
	 */
	@Test
	void testDeleteAllDnaEntities() {
		assertCount(1, 4, 198, 2, 0);
		moduleService.deleteModules(assertNotNull(projectId), false, false);
		assertCount(0, 0, 0, 0, 0);
	}

	/**
	 * Tests if the latest {@link ModelDna} retrieved is empty when the latest run of Discover metrics and Discover Dna is not Discover Dna.
	 */
	@Test
	void testMetricsRerunButNotDna() {
		final Optional<ModelDna> beforeMetricsRerun = dnaModel.getLatestDna(getTestProjectId());
		assertTrue(beforeMetricsRerun.isPresent());
		submitJob(jobManager, tracer, new DiscoverMetricsJob(assertNotNull(projectId), false));
		final Optional<ModelDna> afterMetricsRerun = dnaModel.getLatestDna(getTestProjectId());
		assertFalse(afterMetricsRerun.isPresent());
	}

	/**
	 * Tests the deletion of DNA related entities like {@link DnaString}, {@link DnaStringElement}, {@link DnaSimilarity} on deletion of a specific
	 * {@link Modules} for the given {@link Project}.
	 */
	@Test
	void testDeleteDnaEntitiesOnDeletionOfSingleModule() {
		final var moduleId = moduleService.findAnyModuleId(b -> b.ofProject(assertNotNull(projectId))
																.withPath("src/cobol/WMIN4599/programs/DPB030.cbl")
																.withTechnology(Technology.COBOL)
																.withType(Type.PROGRAM))
				.orElseThrow(() -> new MiningEntityNotFoundException("Module with path must exist: src/cobol/WMIN4599/programs/DPB030.cbl"));

		assertEquals(2, getDnaStringCountByModuleId(moduleId));
		assertEquals(126, getDnaStringElementCountByModuleId(moduleId));
		assertEquals(2, getDnaSimilarityCountByModuleId(moduleId));
		moduleService.deleteModule(moduleId, true);
		assertEquals(0, getDnaStringCountByModuleId(moduleId));
		assertEquals(0, getDnaStringElementCountByModuleId(moduleId));
		assertEquals(0, getDnaSimilarityCountByModuleId(moduleId));
	}

	private void createTestModule(final String name, final String path, final String content, final Date metricsDate) {
		final ModulePojoPrototype testModule = new ModulePojoPrototype();
		testModule.setProject(assertNotNull(projectId));
		testModule.setName(name);
		testModule.setDescription(name);
		testModule.setTechnology(Technology.COBOL);
		testModule.setType(Type.PROGRAM);
		testModule.setStorage(Storage.FILE);
		testModule.setIdentification(Identification.IDENTIFIED);
		testModule.setOrigin(Origin.CUSTOM);
		testModule.setRepresentation(Representation.PHYSICAL);
		testModule.setContent(content);
		testModule.setPath(path);
		testModule.setMetricsDate(metricsDate.toInstant());
		testModule.setCreator(Creator.DISCOVERY);
		moduleService.create(testModule);
	}

	private void createSampleModule(final String folderName, final String fileNameWithExtension, final Date metricsDate) throws IOException {
		/* Sets the sample data */
		final String sampleFile1 = BaseDiscoveryTest.SOURCE_FOLDER + "/DNA/"+ folderName +"/" + fileNameWithExtension;
		final String contentOfSampleFile1 = FileUtils.readFileToString(new File(sampleFile1), StandardCharsets.UTF_8);
		createTestModule(Files.getNameWithoutExtension(fileNameWithExtension), "programs/" + fileNameWithExtension, contentOfSampleFile1, metricsDate);
	}

	private long getDnaStringCountByModuleId(final EntityId moduleId) {
		return dnaDataService.getDnaStringCount(builder -> builder.byModule(moduleId));
	}

	private long getDnaStringElementCountByModuleId(final EntityId moduleId) {
		return dnaDataService.getDnaStringElementCount(builder -> builder.byModule(moduleId));
	}

	private long getDnaSimilarityCountByModuleId(final EntityId moduleId) {
		return dnaDataService.getDnaSimilarityCount(builder -> builder.byModule(moduleId));
	}

	@Override
	protected String getTestFolder() {
		return TEST_FOLDER;
	}

	@BeforeAll
	private void changeTimeZone() {
		final TimeZone timeZone = DEFAULT_TIMEZONE.getID().equals("UTC") ? TimeZone.getTimeZone("Asia/Kolkata") : TimeZone.getTimeZone("UTC");
		TimeZone.setDefault(timeZone);
	}

	@AfterAll
	private void resetTimeZone() {
		TimeZone.setDefault(DEFAULT_TIMEZONE);
	}
}
