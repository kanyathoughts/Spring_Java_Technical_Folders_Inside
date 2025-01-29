/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.entities.ErrorMarkerPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.entities.SourceMetricsPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleStatisticsResponse;
import innowake.mining.shared.model.ModuleStatisticsResponse.ModuleTechnologyStatistic;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * Tests for {@link ModuleService}.
 */
class ModuleServiceTest extends DatabaseRelatedTest {

	/* Module name for testing special character escaping for Lucene text search */
	private static final String BRKG_TRD_FIXED = "brkg.trd.fixed({[]})";
	private static final Integer LINE_OF_CODE = Integer.valueOf(1000);
	private static final EntityId PROJECT_ID = EntityId.of(4L);

	/* use an empty project */

	@Autowired
	private ModuleService moduleService;

	/**
	 * Tests that {@link ModuleService#findModules(BuildingConsumer)} escapes special characters before performing the query with Lucene.
	 */
	@Test
	void testFindAllByNameWithSpecialCharacters() {
		final EntityId projectId = createProjectWithTestModule();
		final Paged<ModulePojo> result = moduleService.findModules(Pagination.subset(0, 10), q -> q.ofProject(projectId).withNames(List.of(BRKG_TRD_FIXED), false));
		final List<ModulePojo> content = result.getContent();
		assertEquals(1, content.size(), () -> String.format("findAll() for name='%s' must return one matching module", BRKG_TRD_FIXED));
		assertEquals(BRKG_TRD_FIXED, content.get(0).getName());
	}

	@Test
	void testCalculateStatistics() {
		createTestModulesForStatistics();
		ModuleStatisticsResponse modulestatistics = moduleService.calculateStatistics(PROJECT_ID);
		assertEquals(6000l, modulestatistics.getSourceCodeLineCount(), "Lines of code doesn't match");
		List<ModuleTechnologyStatistic> tec = modulestatistics.getTechnologies();
		assertEquals(3, assertNotNull(tec).size());
		Optional<Long> LinesOfCodeForCICS = assertNotNull(tec).stream().filter(technology -> assertNotNull(technology.getName()).equals(Technology.CICS.toString()))
				.map(stat -> stat.getCount()).findFirst();
		assertEquals(4000, LinesOfCodeForCICS.get(), "Lines of code for this technology doesn't match");
		assertEquals(2, modulestatistics.getWithErrorsCount(), "Number of errors doesn't match");
		assertEquals(6, modulestatistics.getSourceFileCount(), "Source files count doesn't match");
		assertEquals(7, modulestatistics.getMissingCount(), "Missing module count doesn't match");
	}

	private EntityId createProjectWithTestModule() {
		ProjectPojo project = projectService.create(new ProjectPojoPrototype()
				.setName("TEST PROJECT xyz")
				.setClient(EntityId.of(1L))
				.setNatures(Collections.emptySet()));
		
		createTestModule(BRKG_TRD_FIXED, null, project.identity());
		final List<ModulePojo> modules = moduleService.findModules(builder -> builder.ofProject(project.identity()));
		assertEquals(1, modules.size(), () -> String.format("Module with name '%s' must have been created", BRKG_TRD_FIXED));
		
		return project.identity();
	}

	private EntityId createTestModule(final String name, @Nullable final String path, final EntityId projectId) {
		final ModulePojoPrototype cobolProgram = new ModulePojoPrototype();
		cobolProgram.setProject(projectId);
		cobolProgram.setName(name);
		cobolProgram.setTechnology(Technology.COBOL);
		cobolProgram.setType(Type.PROGRAM);
		cobolProgram.setOrigin(Origin.CUSTOM);
		cobolProgram.setStorage(Storage.FILE);
		cobolProgram.setIdentification(Identification.IDENTIFIED);
		cobolProgram.setCreator(Creator.DISCOVERY);
		/* must be same as path of SourceObject, so that sourceAttachmentLink is established */
		cobolProgram.setPath(path);
		return moduleService.create(cobolProgram);
	}

	private EntityId createTestModules(String name, Technology technology, Integer linesOfCode, String path , final EntityId projectId,
			final Representation representation) {
		final ModulePojoPrototype testModule = new ModulePojoPrototype();
		testModule.setName(name);
		testModule.setProject(projectId);
		testModule.setTechnology(technology);
		testModule.setType(Type.fromName("NATURAL_PROGRAM"));
		testModule.setStorage(Storage.fromName("DATABASE"));
		testModule.setIdentification(Identification.MISSING);
		testModule.setOrigin(Origin.valueOf("ENVIRONMENT"));
		testModule.setCreator(Creator.DISCOVERY);
		testModule.setRepresentation(representation);
		final SourceMetricsPojoPrototype sourceMetrics = new SourceMetricsPojoPrototype();
		sourceMetrics.setCodeLines(linesOfCode);
		testModule.setSourceMetrics(sourceMetrics);
		testModule.setDescription(
				"Was drawing natural fat respect husband. " + "An as noisy an offer drawn blush place. " + "These tried for way joy wrote witty. "
						+ "In mr began music weeks after at begin. " + "Education no dejection so direction pretended household do to.");
		testModule.setPath(path);
		return moduleService.create(testModule);
	}
	
	private void createTestModulesForStatistics() {
		final EntityId firstModule = createTestModules("TEST MODULE 3", Technology.COBOL, LINE_OF_CODE, "programs3/DPG2.cbl", PROJECT_ID,
				Representation.PHYSICAL);
		final EntityId secontModule = createTestModules("TEST MODULE 4", Technology.COBOL, LINE_OF_CODE, "programs4/DPG2.cbl", PROJECT_ID,
				Representation.PHYSICAL);
		createTestModules("TEST MODULE 5", Technology.NATURAL, LINE_OF_CODE, "programs5/DPG2.cbl", PROJECT_ID, Representation.PHYSICAL);
		createTestModules("TEST MODULE 6", Technology.CICS, LINE_OF_CODE, "programs6/DPG2.cbl", PROJECT_ID, Representation.PHYSICAL);
		createTestModules("TEST MODULE 7", Technology.CICS, LINE_OF_CODE, "programs7/DPG2.cbl", PROJECT_ID, Representation.PHYSICAL);
		createTestModules("TEST MODULE 8", Technology.CICS, LINE_OF_CODE, "programs8/DPG2.cbl", PROJECT_ID, Representation.PHYSICAL);
		createTestModules("TEST Virtual MODULE 9", Technology.CICS, LINE_OF_CODE, StringUtils.EMPTY, PROJECT_ID, Representation.VIRTUAL);
		
		moduleService.createErrorMarker(new ErrorMarkerPojoPrototype()
				.setProject(PROJECT_ID)
				.setModule(firstModule)
				.setCause("baz"));
		moduleService.createErrorMarker(new ErrorMarkerPojoPrototype()
				.setProject(PROJECT_ID)
				.setModule(secontModule)
				.setCause("baz"));
		moduleService.createErrorMarker(new ErrorMarkerPojoPrototype()
				.setProject(PROJECT_ID)
				.setModule(secontModule)
				.setCause("baz"));
	}
}
