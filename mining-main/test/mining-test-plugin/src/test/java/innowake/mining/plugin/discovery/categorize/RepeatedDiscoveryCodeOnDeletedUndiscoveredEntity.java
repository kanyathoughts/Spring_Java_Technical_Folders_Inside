/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.plugin.discovery.categorize;

import static org.junit.Assert.assertFalse;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.apache.commons.io.FileUtils;
import org.eclipse.core.runtime.CoreException;
import org.junit.Test;

/**
 * Test the deleted undiscovered entities are not duplicated on the undiscovered entities folder on re-run of discovery code.
 */
public class RepeatedDiscoveryCodeOnDeletedUndiscoveredEntity extends AbstractDiscoverCodeTest {

	@Override
	protected String getTestFolder() {
		return "undiscoverableEntities";
	}

	@Test
	public void testRepeatedDiscoveryCodeOnDeletedUndiscoveredEntity() throws IOException, CoreException {
		final Path project = getTargetTestResourcesProjectPath();
		final Path undiscovered = project.resolve("undiscovered-entities");
		final Path originalFolder = project.resolve("undiscoverableEntities");
		
		/* Verifies that the files are removed to "undiscovered-entities" */
		assertFolder(undiscovered, 1);
		assertFolder(undiscovered, "Sample.txt");
		
		/* Verifies that the original files are removed */
		assertFalse(String.format("Original files are not removed from %s", originalFolder), Files.exists(originalFolder));

		FileUtils.forceDelete(undiscovered.toAbsolutePath().toFile());
		assertFalse(String.format("Folder %s still exists after deletion", undiscovered), Files.exists(undiscovered));
		copyResources();
		executeDiscoverCode();

		final Path duplicateUndiscovered = project.resolve("undiscovered-entities").resolve("duplicates");
		/* Verifies that no "duplicates" folder is created inside undiscovered-entities */
		assertFalse(String.format("Duplicate folder %s should not be created", duplicateUndiscovered), Files.exists(duplicateUndiscovered));
		
		/* Verifies that the files move to "undiscovered-entities" again */
		assertFolder(undiscovered, 1);
		assertFolder(undiscovered, "Sample.txt");
		
		/* Verifies that the original files are removed */
		assertFalse(String.format("Original files are not removed from %s", originalFolder), Files.exists(originalFolder));
	}

}
