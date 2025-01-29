/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.metadata;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Map;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.configuration.GenericConfiguration;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.io.SecuredZipInputStream;
import innowake.mining.shared.io.UploadDescription;

import org.apache.commons.compress.utils.IOUtils;
import org.apache.commons.io.FileUtils;
import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Component;

import innowake.lib.job.api.Job;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.extensions.metadata.model.MetaDataBackup;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;

/**
 * Job extension to restore meta data from {@linkplain MetaDataBackup back up}. It restores meta data such as module description, Annotation,
 * DataDictionaryEntry, Taxonomy, etc. associated with a {@link ModulePojo} from a JSON back up file.
 * <p>
 * The actual implementation is done in {@link MetaDataImportJob}.
 * <p>
 * The result type is declared as {@code String} because the job returns a progress status.
 */
@Component
public class MetaDataImportExtension implements MiningJobExtension<String> {
	
	private final static String IMPORT_FORMAT = "importFormat";
	private final static String COMPRESSED = "compressed";

	private final GenericConfiguration configProperties;
	private final JobConfigurationProperties jobConfigProperties;
	
	public MetaDataImportExtension(final GenericConfiguration configProperties, final JobConfigurationProperties jobConfigProperties) {
		super();
		this.configProperties = configProperties;
		this.jobConfigProperties = jobConfigProperties;
	}

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
		return "mining-metadata-import";
	}

	@Override
	public String getDescription() {
		return "Imports Annotations, DataDictionaryEntries, Taxonomies, Module Descriptions and Functional Blocks from a Backup";
	}

	@Override
	public UploadDescription getUploadDescription() {
		return UploadDescription.with("Metadata JSON", "JSON file containing mining metadata backup", true,
				".json,application/json");
	}

	@Override
	public Job<String> createJob(final EntityId projectId, final Map<String, List<String>> parameters, final HttpEntity<byte[]> inputData) {
		if ( ! inputData.hasBody()) {
			throw new IllegalArgumentException("Input data is mandatory and must contain the data to be imported in JSON format.");
		}
		final byte[] body = inputData.getBody();
		if (body == null) {
			throw new IllegalArgumentException("Input data is mandatory and must contain the data to be imported in JSON format.");
		}

		final String content;
		if ( parameters.containsKey(IMPORT_FORMAT) && parameters.get(IMPORT_FORMAT).contains(COMPRESSED)) {
			content = unassembleZip(body);
		} else {
			content = new String(assertNotNull(inputData.getBody()), StandardCharsets.UTF_8);
		}
		
		final String timeStamp = DateTimeFormatter.ofPattern("yyyy-MM-dd-HH-mm-ss").format(LocalDateTime.now());
		final File tempMetadata = Paths.get(jobConfigProperties.getJobResultFolder(), "tempMetaDataBackup", timeStamp).toFile();

		try {
			FileUtils.writeStringToFile(tempMetadata, content, StandardCharsets.UTF_8);
		} catch (final IOException e) {
			throw new IllegalStateException("While trying to import the MetaData, the creation of the temp file has failed with: " + e);
		}
		
		return new MetaDataImportJob(tempMetadata, projectId);
	}

	private String unassembleZip(final byte[] body) {
		final StringBuilder s = new StringBuilder();
		try (final SecuredZipInputStream zipIn = new SecuredZipInputStream(new ByteArrayInputStream(body), StandardCharsets.UTF_8)) {
			/* x1000000 beacause limitSize() expects a byte value */
			zipIn.limitSize(configProperties.getMetadataMaxZipSizeinMB() * 1_000_000);
			
			while ((zipIn.getNextEntry()) != null) {
				s.append(handleEntry(zipIn));
				zipIn.closeEntry();
			}
			
		} catch (final SecuredZipInputStream.SecurityException se) {
			throw new IllegalStateException("Error during unzipping: " + se + " Try increasing the maxZipImportSize value in application.yml");
		} catch (final IOException e) {
			throw new IllegalStateException("Error during the unzipping of the file: " + e);
		}
		
		return s.toString();
	}

	private BinaryString handleEntry(final SecuredZipInputStream zipIn) {
		try (final ByteArrayOutputStream entryStream = new ByteArrayOutputStream()) {
			IOUtils.copy(zipIn, entryStream);
			return new BinaryString(entryStream.toByteArray());
		} catch (final IOException e) {
			throw new IllegalStateException("Error during unzipping. Try the unzipped upload");
		}
	}
}
