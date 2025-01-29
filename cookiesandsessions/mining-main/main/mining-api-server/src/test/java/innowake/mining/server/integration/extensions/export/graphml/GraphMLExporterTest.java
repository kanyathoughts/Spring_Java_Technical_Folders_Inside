/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration.extensions.export.graphml;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.InputStream;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import innowake.mining.shared.model.Creator;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import innowake.mining.extensions.export.graphml.GraphMLExportJob;
import innowake.mining.server.service.MiningJobService.JobResult;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.TaxonomyCategoryPojoPrototype;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;
import innowake.mining.shared.entities.TaxonomyTypePojoPrototype;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;

/**
 * Tests for {@link GraphMLExportJob}.
 */
class GraphMLExporterTest extends GraphMLTest {
	
	final boolean featureToggleFlag = true;

	@BeforeAll
	public void insertTestData() {
		final EntityId rootJob = moduleService.create(new ModulePojoPrototype()
				.setNid(1L)
				.setProject(TEST_PROJECT_ID)
				.setName("ROOTJOB")
				.setTechnology(Technology.COBOL)
				.setType(Type.COPYBOOK)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.DISCOVERY)
			);
		
		final EntityId stepA = moduleService.create(new ModulePojoPrototype()
				.setNid(2L)
				.setProject(TEST_PROJECT_ID)
				.setName("ROOTJOB.STEPA")
				.setTechnology(Technology.COBOL)
				.setType(Type.EXEC_PGM)
				.setStorage(Storage.FILE)
				.setIdentification(Identification.IDENTIFIED)
				.setOrigin(Origin.CUSTOM)
				.setCreator(Creator.DISCOVERY)
			);
		
		final ModuleLocation moduleLocation = new ModuleLocation(1, 2);
		
		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.CALLS)
				.setSrcModule(rootJob)
				.setSrcLocation(moduleLocation)
				.setDstModule(stepA)
				.setDstLocation(moduleLocation)
			);
		
		annotationService.create(new AnnotationPojoPrototype()
				.setModule(EntityId.of(2000L))
				.setLocation(new ModuleLocation(1, 2))
				.setNid(1L)
				.setName("Test")
				.setCategoryId(2L)
				.setState(WorkingState.CANDIDATE)
				.setType(AnnotationType.DEAD_CODE)
				.setSourceAttachment(BinaryString.EMPTY)
				.setCreatedByUserId("")
				.setUpdatedByUserId("")
			);
		
		final Long taxonomyCategory = taxonomyService.upsertCategory(new TaxonomyCategoryPojoPrototype()
				.setName("Business Taxonomies")
				.setProject(TEST_PROJECT_ID));
		
		final UUID type = taxonomyService.createType(new TaxonomyTypePojoPrototype()
				.setCategoryId(taxonomyCategory)
				.setProject(TEST_PROJECT_ID)
				.setName("TESTTYPE"));
		
		final List<EntityId> taxonomies = taxonomyService.create(List.of(new TaxonomyPojoPrototype()
				.setName("TESTTAXONOMY1")
				.setNid(Long.valueOf(1L))
				.setType(type)
				.setProject(TEST_PROJECT_ID),
				new TaxonomyPojoPrototype()
				.setName("TESTTAXONOMY2")
				.setNid(Long.valueOf(2L))
				.setType(type)
				.setProject(TEST_PROJECT_ID)
				));
		
		taxonomyService.createModuleLinks(rootJob, taxonomies);

	}

	@Test
	void testExportGraphML() throws Exception {
		final GraphMLExportJob job = new GraphMLExportJob(TEST_PROJECT_ID, Collections.emptyMap(), true);
		final Optional<JobResult> jobResult = getJobResult(submitJob(job));
		
		assertTrue(jobResult.isPresent(), "Job Result must exist");

		final InputStream content = jobResult.get().getContent();
		final String value = toString(content);
		
		assertTrue(value.contains("ROOTJOB"), "ROOTJOB must be contained");
		assertTrue(value.contains("ROOTJOB.STEPA"), "ROOTJOB.STEPA must be contained");

		/* Annotation should not be present in the graph */
		assertFalse(value.contains("Test"), "Test must not be contained");

		/* Existing project data should not be present in the graph */
		assertFalse(value.contains("Demo Project"), "Demo Project must not be contained");
		
			/* Asserting taxonomies */
		assertTrue(value.contains("TESTTAXONOMY1"));
		assertTrue(value.contains("TESTTAXONOMY2"));
		assertTrue(value.contains("<key id=\"Tax: TESTTYPE\""));
		assertTrue(value.contains("<data key=\"Tax: TESTTYPE\">TESTTAXONOMY1</data>"));
		assertFalse(value.contains("taxonomies"));
	}
}
