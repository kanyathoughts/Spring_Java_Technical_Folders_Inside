/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.identification;

import static innowake.mining.shared.model.Identification.IDENTIFIED;
import static innowake.mining.shared.model.Origin.CUSTOM;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.concurrent.atomic.AtomicInteger;

import org.springframework.beans.factory.annotation.Autowired;

import brave.Tracer;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.management.JobManager;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.integration.discovery.BaseDiscoveryTest;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.ModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Base class for StoreAstTest, ModuleDescriptionIdentificationTest, TaxonomyIdentificationIntegrationTest.
 */
public abstract class AbstractIdentificationTest extends DatabaseRelatedTest {
	
	protected static final String RESOURCE_PATH = "/test-resources/innowake/mining/server/job/identification/";
	protected static final String RESOURCE_PATH_PL1 = "/test-resources/innowake/mining/server/job/identification/PL1";
	protected static final EntityId PROJECT_ID_1 = EntityId.of(1L);
	protected static final EntityId PROJECT_ID_2 = EntityId.of(2L);
	protected static final EntityId PROJECT_ID_3 = EntityId.of(3L);
	protected static final EntityId PROJECT_ID_4 = EntityId.of(4L);
	private static final AtomicInteger PROJECT_IDX = new AtomicInteger(0);
	
	@Autowired
	private Tracer tracer;

	@Autowired
	protected ModuleService moduleService;

	/**
	 * Creates a module with Storage as FILE_SECTION.
	 *
	 * @param projectId the project id
	 * @param dir the file directory
	 * @param filename the filename
	 * @param content the file content
	 * @param technology the technology of the Module
	 * @param type the type of the Module
	 * @return the moduleId
	 */
	protected EntityId createFileSection(final EntityId projectId, final String dir, final String filename, final String content,
			final Technology technology, final Type type) {
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setName(filename);
		module.setProject(projectId);
		module.setTechnology(technology);
		module.setType(type);
		module.setStorage(Storage.FILE_SECTION);
		module.setIdentification(IDENTIFIED);
		module.setOrigin(CUSTOM);
		module.setPath(dir + filename);
		module.setContent(content);
		module.setCreator(Creator.DISCOVERY);
		return moduleService.create(module);
	}
	
	/**
	 * Creates a module with Storage as FILE.
	 *
	 * @param projectId the project id
	 * @param name the filename
	 * @param file the filename with extension
	 * @param dir the file directory
	 * @param technology the technology of the Module
	 * @param type the type of the Module
	 * @return the moduleId
	 */
	protected EntityId createModule(final EntityId projectId, final String name, final String file, final String dir, final Technology technology, final Type type) {
		final String content = getContent(file, dir);
		final ModulePojoPrototype module = new ModulePojoPrototype();
		module.setName(name);
		module.setProject(projectId);
		module.setTechnology(technology);
		module.setType(type);
		module.setStorage(Storage.FILE);
		module.setIdentification(IDENTIFIED);
		module.setOrigin(CUSTOM);
		module.setPath(dir + file);
		module.setContent(content);
		module.setCreator(Creator.DISCOVERY);
		return moduleService.create(module);
	}
	
	/**
	 * Creates a COBOL PROGRAM Module
	 *
	 * @param projectId the project id
	 * @param name the filename
	 * @param file the filename with extension
	 * @param dir the file directory
	 * @return the created module ID
	 */
	protected EntityId createCobolProgram(final EntityId projectId, final String name, final String file, final String dir) {
		return createModule(projectId, name, file, dir, Technology.COBOL, Type.PROGRAM);
	}
	
	/**
	 * Creates a Natural PROGRAM Module
	 *
	 * @param projectId the project id
	 * @param name the filename
	 * @param file the filename with extension
	 * @param dir the file directory
	 * @return the created module ID
	 */
	protected EntityId createNaturalProgram(final EntityId projectId, final String name, final String file, final String dir) {
		return createModule(projectId, name, file, dir, Technology.NATURAL, Type.PROGRAM);
	}
	
	/**
	 * Creates a COBOL COPYBOOK Module
	 *
	 * @param projectId the project id
	 * @param name the filename
	 * @param file the filename with extension
	 * @param dir the file directory
	 * @return the created module ID
	 */
	protected EntityId createCobolCopybook(final EntityId projectId, final String name, final String file, final String dir) {
		return createModule(projectId, name, file, dir, Technology.COBOL, Type.COPYBOOK);
	}
	
	/**
	 * Creates a Reference between two modules.
	 *
	 * @param relationship the {@link RelationshipType}
	 * @param fromId the fromId to create the reference
	 * @param toId the toId to create the reference
	 */
	protected void createReference(final RelationshipType relationship, final EntityId fromId, final EntityId toId) {
		moduleService.createRelationship(new ModuleRelationshipPojoPrototype()
				.setRelationship(relationship)
				.setSrcModule(fromId)
				.setDstModule(toId));
	}
	
	/**
	 * submits the job.
	 *
	 * @param jobManager the {@link JobManager}
	 * @param job the job to submit
	 */
	protected void submitJob(final JobManager jobManager, final Job<?> job) {
		BaseDiscoveryTest.submitJob(jobManager, tracer, job);
	}
	
	/**
	 * Gets the content of the file.
	 * 
	 * @param file the filename
	 * @return the content
	 */
	protected String getContent(final String file) {
		return getContent(file, RESOURCE_PATH);
	}

	/**
	 * Gets the content of the file.
	 * 
	 * @param file the filename
	 * @param dir the directory
	 * @return the content
	 */
	protected String getContent(final String file, final String dir) {
		final Path path = Paths.get(System.getProperty("user.dir"), dir, file);
		try {
			return new String(Files.readAllBytes(path), StandardCharsets.UTF_8).replaceAll("\\r\\n", "\n").replaceAll("\\r", "\n");
		} catch (final IOException e) {
			fail(e.getLocalizedMessage());
			throw new IllegalStateException("Exception occured while file reading", e);
		}
	}
	
	/**
	 * Creates the Project.
	 * 
	 * @return the Project
	 */
	protected ProjectPojo createProject() {
		return projectService.create(new ProjectPojoPrototype()
				.setName("Test Project " + PROJECT_IDX.incrementAndGet())
				.setClient(EntityId.of(1L))
				.setNatures(Collections.emptySet()));
	}
}
