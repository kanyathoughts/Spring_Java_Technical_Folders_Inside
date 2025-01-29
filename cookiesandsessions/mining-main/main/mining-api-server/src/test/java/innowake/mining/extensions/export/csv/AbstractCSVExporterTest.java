/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.export.csv;

import java.util.List;

import org.junit.jupiter.api.BeforeAll;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.model.AnnotationCategory;
import innowake.mining.shared.model.AnnotationType;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.WorkingState;

/**
 * Infrastructure for testing the CSV export
 */
public abstract class AbstractCSVExporterTest extends DatabaseRelatedTest {
	
	/* Existing project */
	protected static final EntityId EXISTING_PROJECT_ID = EntityId.of(1l);

	/* empty project */
	protected static final EntityId TEST_PROJECT_ID = EntityId.of(4l);
	protected static final int MODULE_LOCATION_OFFSET = 1;
	protected static final int MODULE_LOCATION_LENGTH = 10;

	protected static class TestData {
		public final EntityId rootJob;
		public final EntityId stepA;

		public TestData(final EntityId rootJob, final EntityId stepA) {
			this.rootJob = rootJob;
			this.stepA = stepA;
		}
	}

	@Autowired
	protected ModuleService moduleService;

	@Autowired
	protected AnnotationService annotationService;
	
	@Autowired
	protected DataDictionaryService dataDictionaryService;
	
	protected TestData testData = new TestData(EntityId.VOID, EntityId.VOID);

	@Override
	protected ResetScriptFile getScriptFile() {
		return ResetScriptFile.COMPLETE;
	}
	
	@BeforeAll
	public void insertTestData() {

		ModulePojoPrototype module = new ModulePojoPrototype();
		module.setNid(Long.valueOf(1l));
		module.setProject(TEST_PROJECT_ID);
		module.setName("ROOTJOB");
		module.setTechnology(Technology.COBOL);
		module.setType(Type.COPYBOOK);
		module.setStorage(Storage.FILE);
		module.setPath("ROOTJOB.CBL");
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(Origin.CUSTOM);
		module.setCreator(Creator.DISCOVERY);
		final EntityId rootJob = moduleService.create(module);

		module = new ModulePojoPrototype();
		module.setNid(Long.valueOf(2l));
		module.setProject(TEST_PROJECT_ID);
		module.setName("ROOTJOB.STEPA");
		module.setTechnology(Technology.COBOL);
		module.setType(Type.EXEC_PGM);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(Origin.CUSTOM);
		module.setCreator(Creator.DISCOVERY);
		final EntityId stepA = moduleService.create(module);

		module = new ModulePojoPrototype();
		module.setNid(3L);
		module.setProject(TEST_PROJECT_ID);
		module.setName("DUMMYPRG");
		module.setTechnology(Technology.BASIC);
		module.setType(Type.PROGRAM);
		module.setStorage(Storage.FILE);
		module.setIdentification(Identification.IDENTIFIED);
		module.setOrigin(Origin.CUSTOM);
		module.setCreator(Creator.DISCOVERY);
		moduleService.create(module);

		final List<AnnotationCategory> categories = annotationService.findCategories(q -> q.ofProjectWithDefault(EXISTING_PROJECT_ID));
		/* We sort so we can get IDs of categories not created by migration V1.2.94 */
		categories.sort((final AnnotationCategory a, final AnnotationCategory b) -> a.getId().compareTo(b.getId()));
		
		final ModuleLocation moduleLocation = new ModuleLocation(1, 2);
		
		annotationService.create(new AnnotationPojoPrototype()
				.setModule(EntityId.of(2000l))
				.setLocation(moduleLocation)
				.setName("Test")
				.setNid(1l)
				.setCategoryId(categories.get(categories.size() - 2).getId())
				.setState(WorkingState.CANDIDATE)
				.setType(AnnotationType.DEAD_CODE)
				.setSourceAttachment(new BinaryString(""))
				.setCreatedByUserId("")
				.setUpdatedByUserId(""));
		
		final ModuleRelationshipPojoPrototype reference = new ModuleRelationshipPojoPrototype();
		reference.setRelationship(RelationshipType.CALLS);
		reference.setSrcModule(rootJob);
		reference.setDstModule(stepA);
		reference.setSrcLocation(moduleLocation);
		reference.setDstLocation(moduleLocation);
		moduleService.createRelationship(reference);
		
		final DataDictionaryPojoPrototype entry = new DataDictionaryPojoPrototype()
				.setName("PROGRAM-NAME")
				.setDescription("My Description")
				.setModule(EntityId.of(2000L))
				.setFormat("PICX")
				.setCreatedByUserId("")
				.setLocation(new ModuleLocation(MODULE_LOCATION_OFFSET, MODULE_LOCATION_LENGTH));
		dataDictionaryService.create(entry);

		testData = new TestData(rootJob, stepA);
	}

}
