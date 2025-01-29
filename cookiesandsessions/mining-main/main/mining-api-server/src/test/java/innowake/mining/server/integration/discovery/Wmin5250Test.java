/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.integration.discovery;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.apache.commons.io.FilenameUtils;
import org.apache.poi.EncryptedDocumentException;
import org.junit.jupiter.api.Test;
import org.springframework.security.test.context.support.WithMockUser;

import com.orientechnologies.orient.core.storage.ORecordDuplicatedException;

import innowake.mining.data.io.sourceobject.FileExtension;
import innowake.mining.server.discovery.DiscoveryExcelUtil;
import innowake.mining.server.discovery.metrics.DiscoverMetricsJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.model.ModuleType;

/**
 * Test class for testing WMIN-5250: 'ORecordDuplicatedException.'
 */
@WithMockUser
class Wmin5250Test extends BaseDiscoveryTest {

	/**
	 * Tests that when the JCL {@code INSPR763.proc} gets collected, that the {@link JclJobContributor} doesn't fetch the BASIC includes
	 * {@link SourceObject SourceObjects} like {@code VSTID6.INC} when searching for control card include members, which would cause an
	 * {@link ORecordDuplicatedException} when storing the {@link Module Modules} for JCL.
	 */
	@Test
	void testDiscovery() throws EncryptedDocumentException, IOException {
		projectId = createProject().identity();
		uploadResources(projectId);

		submitJob(jobManager, tracer, new DiscoverMetricsJob(assertNotNull(projectId), false));

		final Path expectedFile = getExpectedFile(getExpectedFileName());
		if (isWriteExpected()) {
			writeExpected(expectedFile, getMetricsContentAsCsv(assertNotNull(projectId)));
		} else {
			DiscoveryExcelUtil.compareIgnoreOrder(read(expectedFile), getMetricsContentAsCsv(assertNotNull(projectId)));
		}
	}
	
	@Override
	protected void uploadResources(final EntityId projectId, final Predicate<Path> pathFilter) {
		try (final Stream<Path> walk = Files.walk(getSourcePath())) {
			walk.filter(pathFilter)
			.map(path -> {
				final String content;
				try {
					content = getFileContent(path);
				} catch (final IOException e) {
					throw new IllegalStateException(e);
				}
				String pathToSet = Paths.get("temp").resolve(getSourcePath().getParent().relativize(path)).toString();
				
				if (pathToSet.contains("INSPR763.proc")) {
					pathToSet = "src/jcl/procs/INSPR763.proc";
				} else {
					pathToSet = "src/basic/include/" + FilenameUtils.getName(pathToSet);
				}
				final ModuleType moduleType = FileExtension.resolve(pathToSet);
				final SourcePojoPrototype sourceObject = createSourceObject(projectId, path, content, moduleType);
				sourceObject.setPath(pathToSet);
				sourceObject.setName(FilenameUtils.getBaseName(pathToSet));
				return sourceObject;
			}).forEach(this::uploadSourceObject);

			importConfiguration(projectId);
		} catch (final Exception e) {
			e.printStackTrace();
			fail("Error while uploading resource: " + e.getMessage());
		}
	}

	@Override
	protected String getTestFolder() {
		return "WMIN5250";
	}
}
