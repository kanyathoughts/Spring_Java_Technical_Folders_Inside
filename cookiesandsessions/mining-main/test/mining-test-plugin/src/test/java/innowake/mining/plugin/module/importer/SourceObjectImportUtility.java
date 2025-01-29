/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.plugin.module.importer;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.apache.commons.io.IOUtils;

import com.fasterxml.jackson.databind.ObjectMapper;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.Result;
import innowake.mining.plugin.IntegrationBaseTest;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.io.MiningFileIndex;

/**
 * Utility class for source Object import.
 */
public class SourceObjectImportUtility {
	
	static class FileIndexEntry {
		private final MiningFileIndex.File file;
		private @Nullable String content;
		
		public FileIndexEntry(MiningFileIndex.File file) {
			this.file = file;
		}
		
		public MiningFileIndex.File getFile() {
			return file;
		}
		
		public @Nullable String getContent() {
			return content;
		}
		
		public void setContent(@Nullable String content) {
			this.content = content;
		}
	}
	
	private SourceObjectImportUtility() {
		/* no instances allowed */
	}
	
	private static final Long PROJECT_ID = Long.valueOf(1);
	
	/**
	 * Get the count of source object with projectId.
	 * 
	 * @return count of source objects
	 */
	public static Map<String, FileIndexEntry> getSourceObjects() {
		HashMap<String, FileIndexEntry> i = new HashMap<>();
		try {
			MiningApiClient.ioService(IntegrationBaseTest.getConnectionInfo()).exportSourceObjects().setProjectId(PROJECT_ID).execute();
			
			final Result<byte[]> result = MiningApiClient.ioService(IntegrationBaseTest.getConnectionInfo())
					.exportSourceObjects().setProjectId(PROJECT_ID).execute();
			
			if ( ! result.isValid()) {
				throw new IllegalStateException(String.format("Could not query SourceObject count:%n%s", result.getExtendedStatusMessage()));
			}
			
			final ZipInputStream zip = new ZipInputStream(new ByteArrayInputStream(result.getValue().orElseThrow()));
			
			ZipEntry z;
			boolean first = true;
			while ((z = zip.getNextEntry()) != null) {
				if (first) {
					final MiningFileIndex idx = new ObjectMapper().readValue(IOUtils.toString(zip, StandardCharsets.UTF_8), MiningFileIndex.class);
					for (MiningFileIndex.File f : idx.getFiles()) {
						i.put(f.getPath(), new FileIndexEntry(f));
					}
					first = false;
				} else {
					final FileIndexEntry e = i.get(z.getName());
					if (e != null) {
						e.setContent(IOUtils.toString(zip, StandardCharsets.UTF_8));
					} else {
						throw new IllegalStateException("Non-indexed content " + z.getName());
					}
				}
			}
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}
		return i;
	}
	
	/**
	 * Get the content of SourceObject.
	 * 
	 * @param path SourceObject path
	 * @return content of SourceObject
	 */
	public static @Nullable String getModuleSource(final String path) {
		try {
			final Result<ModulePojo> moduleResult = MiningApiClient.moduleService(IntegrationBaseTest.getConnectionInfo())
					.findModuleByPath()
					.setProjectId(PROJECT_ID)
					.setPath(path)
					.setIncludeContent(true)
					.execute();

			if ( ! moduleResult.isValid()) {
				throw new IllegalStateException(String.format("Could not query Module:%n%s", moduleResult.getExtendedStatusMessage()));
			}
			final Optional<ModulePojo> module = moduleResult.getValue();
			if (module.isPresent()) {
				return module.get().getContent().orElse(null);
			}
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}
		return null;
	}

}
