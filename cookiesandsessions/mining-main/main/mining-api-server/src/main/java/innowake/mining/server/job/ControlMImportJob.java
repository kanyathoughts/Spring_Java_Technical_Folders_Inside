/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.job;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.mining.shared.entities.scheduler.SchedulerImportPojoPrototype;
import innowake.mining.shared.model.job.Message;
import innowake.mining.server.service.ControlMImportService;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

import static innowake.mining.server.util.ExporterUtil.CONTENT_TYPE_PLAIN;

/**
 * Job to import data from Control-M.
 */
public class ControlMImportJob extends SchedulerImportJob<FileSystemResult> {

	private static final Logger LOG = LoggerFactory.getLogger(ControlMImportJob.class);
	@Autowired
	private transient ControlMImportService importService;
	private final SchedulerImportPojoPrototype prototype;

	public ControlMImportJob(final SchedulerImportPojoPrototype prototype) {
		super(prototype.project.getNonNull());
		this.prototype = prototype;
	}

	@Override
	protected Result<FileSystemResult> run(final ProgressMonitor progressMonitor) {
		final List<String> warnings = new ArrayList<>();
		try {
			final Consumer<String> warningHandler = (final String warningMessage) -> {
				/* this handler can be used to capture the warnings into the scheduler_import table */
				LOG.warn(warningMessage);
				warnings.add(warningMessage);
				if (jobMonitor != null) {
					this.jobMonitor.addMessage(new Message(Message.Severity.WARNING, warningMessage));
				}
			};

			importService.importData(prototype, progressMonitor, warningHandler);
		} catch (final IOException e) {
			return new Result<>(new Status(e));
		}
		if ( ! warnings.isEmpty()) {
			try (final OutputStream out = new BufferedOutputStream(createResultFile())) {
				out.write(warnings.stream()
						.collect(StringBuilder::new, (sb, warning) -> sb.append(warning)
								.append('\n'), StringBuilder::append)
						.toString()
						.getBytes());
				return new Result<>(new Status(innowake.lib.job.api.Severity.WARNING), new FileSystemResult(CONTENT_TYPE_PLAIN, "warnings.txt"));
			} catch (final IOException e) {
				return new Result<>(new Status(e));
			}
		}
		return new Result<>(Status.OK);
	}
}
