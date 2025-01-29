/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.Assert.fail;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.junit.jupiter.api.BeforeEach;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.PersistingSourceObjectResolver;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.discovery.metrics.SourceObjectDependency;
import innowake.mining.server.discovery.metrics.SourceObjectManager;
import innowake.mining.server.discovery.metrics.cobol.CobolSourceObjectManager;
import innowake.mining.server.discovery.metrics.natural.NaturalSourceObjectManager;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.entities.testing.SourcePojoDummy;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Base class for {@link NaturalSourceObjectManager} and {@link CobolSourceObjectManager} tests.
 */
public abstract class AbstractSourceObjectManagerTest extends DatabaseResettingTest {

	private static final String RESOURCE_PATH = "/test-resources/innowake/mining/server/integration/sourceobjectmanager/";

	protected static final EntityId PROJECT_ONE = EntityId.of(Long.valueOf(1));

	@Autowired
	protected SourceCachingService sourceService;

	@Nullable
	protected SourceObjectResolver objectResolver;

	@BeforeEach
	public void setupObjectResolver() {
		final ProjectPojo project = projectService.get(PROJECT_ONE);
		objectResolver = new PersistingSourceObjectResolver(sourceService, new SearchOrders(assertNotNull(project).getSearchOrders()));
		assertNotNull(objectResolver);
		sourceService.resetCaches();
	}

	/**
	 * Provides a {@link SourceObjectManager}.
	 *
	 * @return the {@link SourceObjectManager}
	 */
	protected abstract SourceObjectManager<?> getObjectManager();

	protected void assertSingleDependency(final SourcePojo source, final String name, final Type type, final boolean targetExists) {
		final List<?> dependencies = getObjectManager().getOutgoingDependencies(source);
		assertDependenciesSize(1, dependencies);
		assertDependency(dependencies, 0, name, type, targetExists);
	}

	protected void assertDependenciesSize(final int size, final List<?> dependencies) {
		assertEquals(size, dependencies.size());
	}

	protected void assertDependency(
			final List<?> dependencies,
			final int index,
			final String name,
			final Type type,
			final boolean targetExists) {

		final SourceObjectDependency dependency = (SourceObjectDependency) dependencies.get(index);
		assertEquals(name, dependency.getTargetName());
		assertEquals(targetExists, dependency.targetExists());
		if (targetExists) {
			assertEquals(type, dependency.getTargets().iterator().next().getType());
		} else {
			assertEquals(type, dependency.getTargetType());
		}
	}

	protected SourcePojo createSourceObject(
			final String mainFolder,
			final String folder,
			final String name,
			final String extension,
			final Type type,
			final Technology technology) {

		final String path = "sourceobjectmanager/" + mainFolder + "/" + folder + "/" + name + "." + extension;
		final String content = getContent(mainFolder, folder, name + "." + extension);
		final SourcePojoPrototype proto = new SourcePojoDummy() 
				.setProject(PROJECT_ONE)
				.setName(name)
				.setPath(path)
				.setTechnology(technology)
				.setType(type)
				.setContent(new BinaryString(content));
		sourceService.create(proto);
		return assertNotNull(sourceService.cachingByProjectPath(PROJECT_ONE.getNid(), path));
	}

	protected String getContent(final String mainFolder, final String folder, final String file) {
		final Path path = Paths.get(System.getProperty("user.dir"), RESOURCE_PATH, mainFolder, folder, file);
		try {
			return IOUtils.toString(Files.newBufferedReader(path, Charset.forName("Cp1252")));
		} catch (final IOException e) {
			fail(e.getLocalizedMessage());
			throw new IllegalStateException(e);
		}
	}
}
