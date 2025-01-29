/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.io.sourceobject;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.Logging;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.io.MiningFileIndex;
import innowake.mining.shared.io.MiningFileIndex.File;

/**
 * Service for exporting source objects to a ZIP file.
 */
@Service
public class SourceObjectExportService {
	
	private static final Logger LOG = LoggerFactory.getLogger(Logging.IO);
	
	@Autowired
	private SourceService sourceService;
	
	@Autowired
	private ProjectService projectService;
	
	/**
	 * Prepares the miningFileIndex by setting the sourceCodeRevision and by adding the associated
	 * {@link innowake.mining.shared.io.MiningFileIndex.File} 
	 * A ZIP file containing the miningFileIndex and the Source is written into {@link ServletOutputStream} of {@link HttpServletResponse}.
	 * The ZIP files are written if the contentRevision on the Source is greater than the given baseRevision.
	 *
	 * @param projectId The given projectId.
	 * @param baseRevision The given baseRevision.
	 * @param outputStream The {@link ServletOutputStream} of {@link HttpServletResponse}
	 * @throws IOException In case of an error.
	 */
	public void exportSourceObjects(final EntityId projectId, @Nullable final Long baseRevision, final ServletOutputStream outputStream) throws IOException {
		LOG.info("Exporting source objects for project {} with base revision {}", projectId, baseRevision == null ? "n/a" : baseRevision);
		final ProjectPojo project = projectService.get(projectId);
		final List<SourcePojo> sourceObjects = sourceService.find(q -> q.ofProject(project.identity()).includeContent(true));
		final List<File> files = sourceObjects.stream()
												.map(File::setFromSourceObject)
												.collect(Collectors.toList());
		
		final MiningFileIndex index = new MiningFileIndex();
		index.setScope("/");
		index.setSourceCodeRevision(assertNotNull(project.getSourceCodeRevision()));
		index.setFiles(files);
		try (final ZipOutputStream zipOut = new ZipOutputStream(outputStream)) {
			final ObjectMapper mapper = new ObjectMapper();
			final byte[] byteArray = mapper.writeValueAsBytes(index);
			zipOut.putNextEntry(new ZipEntry(MiningFileIndex.NAME));
			zipOut.write(byteArray);
			
			sourceObjects.stream()
				.filter(sourceObject -> (baseRevision == null || sourceObject.getContentRevision() > baseRevision))
				.forEach(sourceObject -> {
					final String fileName = sourceObject.getPath();
					LOG.debug("Exporting source object content {}", fileName);
					final byte[] fileContent = sourceObject.getContent().get();
					final ZipEntry zipEntry = new ZipEntry(fileName);
					try {
						zipOut.putNextEntry(zipEntry);
						zipOut.write(fileContent);
					} catch (final IOException e) {
						final String message = String.format("Error while accessing file %s: %s", fileName, e.getMessage());
						LOG.error(message, e);
						throw new IllegalStateException(message, e);
					}
				});
		}
	}
}
