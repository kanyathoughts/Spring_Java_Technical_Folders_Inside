/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller.cfg;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.collection.IsEmptyCollection.empty;
import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import innowake.mining.shared.model.*;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.server.cfg.CfgDatasetDependenciesMetadata;
import innowake.mining.server.cfg.CfgNodeMetadata;
import innowake.mining.server.cfg.CfgNodeMetadata.MetadataType;
import innowake.mining.server.cfg.JclCfgMetadataProvider;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;

/**
 * Tests whether the JclCfgMetadataProvider provides the correct data for a given JCL-Step-module
 */
class JclCfgMetadataProviderTest extends DatabaseRelatedTest {
	
	private static final EntityId TEST_PROJECT_ID = EntityId.of(4l);
	
	@Nullable
	ModulePojo testModuleNoMetadata;
	@Nullable
	ModulePojo testModuleWithMetadata;
	@Nullable
	ModulePojo inputFile;
	@Nullable
	ModulePojo outputFile;
	@Autowired
	private ModuleService moduleService;
	
	@Autowired
	
	private static final String INPUT_FILE_NAME = "INPUT_FILE";
	private static final String OUTPUT_FILE_NAME = "OUTPUT_FILE";
	private static final String STEP_WITHOUT_METADATA_NAME = "STEP_WITHOUT_METADATA";
	private static final String STEP_WITH_METADATA_NAME = "STEP_WITH_METADATA";
			
	@BeforeAll
	void prepareTestData() {
		/* Create file modules */
		ModulePojoPrototype module = new ModulePojoPrototype();
		module.setName(INPUT_FILE_NAME);
		module.setTechnology(Technology.RESOURCE);
		module.setType(Type.FILE);
		module.setStorage(Storage.FILE);
		module.setOrigin(Origin.CUSTOM);
		module.setIdentification(Identification.IDENTIFIED);
		inputFile = create(TEST_PROJECT_ID, module);
		
		module = new ModulePojoPrototype();
		module.setName(OUTPUT_FILE_NAME);
		module.setTechnology(Technology.RESOURCE);
		module.setType(Type.FILE);
		module.setStorage(Storage.FILE);
		module.setOrigin(Origin.CUSTOM);

		module.setIdentification(Identification.IDENTIFIED);
		outputFile = create(TEST_PROJECT_ID, module);
		
		/* Create step modules */
		module = new ModulePojoPrototype();
		module.setName(STEP_WITHOUT_METADATA_NAME);
		module.setTechnology(Technology.JCL);
		module.setType(Type.EXEC_PGM);
		module.setStorage(Storage.FILE);
		module.setOrigin(Origin.CUSTOM);
		module.setIdentification(Identification.IDENTIFIED);
		testModuleNoMetadata = create(TEST_PROJECT_ID, module);
		
		module = new ModulePojoPrototype();
		module.setName(STEP_WITH_METADATA_NAME);
		module.setTechnology(Technology.JCL);
		module.setType(Type.EXEC_PGM);
		module.setStorage(Storage.FILE);
		module.setOrigin(Origin.CUSTOM);
		module.setIdentification(Identification.IDENTIFIED);
		
		testModuleWithMetadata = create(TEST_PROJECT_ID, module);
		
		/* Create ReadsWrites objects and add them to step module */
		Map<String, Object> properties = new HashMap<>();
		properties.put(ModelAttributeKey.FILE_ACCESS_TYPE.name(), "READ");
		final ModuleLocation testML = new ModuleLocation(0, 0);
		final ModuleRelationshipPojoPrototype read = new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.ACCESSES)
				.setSrcModule(Objects.requireNonNull(testModuleWithMetadata).identity())
				.setSrcLocation(testML)
				.setDstModule(Objects.requireNonNull(inputFile).identity())
				.setDstLocation(testML)
				.setProperties(properties);
		moduleService.createRelationship(read);

		properties = new HashMap<>();
		properties.put(ModelAttributeKey.FILE_ACCESS_TYPE.name(), "WRITE");
		final ModuleRelationshipPojoPrototype write = new ModuleRelationshipPojoPrototype()
				.setRelationship(RelationshipType.ACCESSES)
				.setSrcModule(Objects.requireNonNull(testModuleWithMetadata).identity())
				.setSrcLocation(testML)
				.setDstModule(Objects.requireNonNull(outputFile).identity())
				.setDstLocation(testML)
				.setProperties(properties);
		moduleService.createRelationship(write);
	}
	
	@Test
	void testGetMetadata() {
		final JclCfgMetadataProvider provider = new JclCfgMetadataProvider(moduleService);
		assertNotNull(testModuleWithMetadata);
		final List<CfgNodeMetadata> metadata = provider.getMetadata(testModuleWithMetadata, MetadataType.DATASET_DEPENDENCIES);
		assertEquals(1, metadata.size());
		final CfgNodeMetadata cfgNodeMetadata = metadata.get(0);
		assertThat(cfgNodeMetadata, instanceOf(CfgDatasetDependenciesMetadata.class));
		final CfgDatasetDependenciesMetadata datasetsMetadata = (CfgDatasetDependenciesMetadata) cfgNodeMetadata;
		assertEquals(1, datasetsMetadata.getInputDatasets().size());
		final ModulePojo actualInputFile = datasetsMetadata.getInputDatasets().get(0);
		assertNotNull(inputFile);
		assertEquals(INPUT_FILE_NAME, actualInputFile.getName());
		assertEquals(1, datasetsMetadata.getOutputDatasets().size());
		final ModulePojo actualOutputFile = datasetsMetadata.getOutputDatasets().get(0);
		assertNotNull(outputFile);
		assertEquals(OUTPUT_FILE_NAME, actualOutputFile.getName());
	}
	
	@Test
	void testGetMetadataNoMetadata() {
		final JclCfgMetadataProvider provider = new JclCfgMetadataProvider(moduleService);
		final List<CfgNodeMetadata> metadata = provider.getMetadata(testModuleNoMetadata, MetadataType.DATASET_DEPENDENCIES);
		assertThat(metadata, is(empty()));
	}
	
	private ModulePojo create(final EntityId projectId, final ModulePojoPrototype module) {
		module.setProject(projectId);
		if (module.creator.orElse(null) == null) {
			module.setCreator(Creator.API);
		}
		return moduleService.getModule(moduleService.create(module));
	}
}