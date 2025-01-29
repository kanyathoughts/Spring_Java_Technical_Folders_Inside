/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.job.result;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;

import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;

/**
 * The test job for writing results to the file system.
 */
public class JobResultTestJob extends Job<FileSystemResult> {

	@Override
	protected Result<FileSystemResult> run(final ProgressMonitor progressMonitor) {
		try (final BufferedWriter out = new BufferedWriter(new OutputStreamWriter(createResultFile(), StandardCharsets.UTF_8))) {
			out.write("Hello, World!");
		} catch (IOException e) {
			return new Result<>(new Status(e));
		}
		
		return new Result<>(new FileSystemResult("text/plain", "hello.txt"));
	}
}
