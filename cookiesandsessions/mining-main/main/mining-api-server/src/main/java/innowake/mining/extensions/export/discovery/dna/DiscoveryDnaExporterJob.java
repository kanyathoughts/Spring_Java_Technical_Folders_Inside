/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.extensions.export.discovery.dna;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.mining.extensions.export.discovery.dna.DiscoveryDnaExporter.File;
import innowake.mining.shared.access.EntityId;
import innowake.mining.server.job.MiningJob;
import innowake.mining.shared.discovery.Tuple2;

/**
 * Job for exporting Excel. This job can be serialized.
 */
public class DiscoveryDnaExporterJob extends MiningJob<FileSystemResult> {

	public static final String CONTENT_TYPE = "application/zip";
	private static final Charset UTF_8 = StandardCharsets.UTF_8;

	private final File files[];

	@Autowired
	private transient NeighboringModulesGenerator neighboringModulesGenerator;

	public DiscoveryDnaExporterJob(final EntityId projectId, final File... files) {
		super(projectId);
		this.files = files;
	}

	@Override
	protected Result<FileSystemResult> run(final ProgressMonitor progressMonitor) {
		progressMonitor.setJobDescription("Export Discovery Dna files");

		final List<DiscoverDnaGenerator> generators = new ArrayList<>(files.length);
		for (final File file : files) {
			switch (file) {
				case NEIGHBORING_MODULES:
					generators.add(neighboringModulesGenerator);
					break;
				default:
					throw new UnsupportedOperationException("The type of DNA export is not supported");
			}
		}
		try (final ZipOutputStream zipOut = new ZipOutputStream(createResultFile(), UTF_8)) {
			for (final DiscoverDnaGenerator generator : generators) {
				for (final Tuple2<String, byte[]> generatedOutput : generator.build(projectId)) {
					zipOut.putNextEntry(new ZipEntry(generatedOutput.e1));
					zipOut.write(generatedOutput.e2);
				}
			}
		} catch (final IOException e) {
			return new Result<>(new Status(e));
		}

		return new Result<>(new FileSystemResult(CONTENT_TYPE, "Dna_files.zip"));
	}

	@Override
	public String getJobName() {
		return "Discovery DNA Exporter";
	}
}
