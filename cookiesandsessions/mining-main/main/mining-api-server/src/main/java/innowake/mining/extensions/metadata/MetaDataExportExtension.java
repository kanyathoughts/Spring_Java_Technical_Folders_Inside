/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.metadata;

import java.util.List;
import java.util.Map;

import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Component;

import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.Job;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;

/**
 * Job extension for meta data back up. It exports meta data such as module description, Annotation, DataDictionaryEntry, Taxonomy, etc.
 * associated with a Module to a JSON file.
 * <p>
 * The actual implementation is done in {@link MetaDataExportJob}.
 * <p>
 * The result type is declared as {@code FileSystemResult} because the job returns a JSON file.
 */
@Component
public class MetaDataExportExtension implements MiningJobExtension<FileSystemResult> {

	@Override
	public NatureType getRequiredNature() {
		return NatureType.MINING;
	}

	@Override
	public RoleType getRequiredRole() {
		return RoleType.ADMIN;
	}

	@Override
	public String getIdentifier() {
		return "mining-metadata-export";
	}

	@Override
	public String getDescription() {
		return "Exports Annotations, DataDictionaryEntries, Taxonomies, Module Descriptions and Functional Blocks for Backup Purposes";
	}

	@Override
	public Job<FileSystemResult> createJob(final EntityId projectId, final Map<String, List<String>> parameters, final HttpEntity<byte[]> inputData) {
		return new MetaDataExportJob(projectId, parameters);
	}

}
