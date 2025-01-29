/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.job.file;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.concurrent.TimeUnit;
import org.apache.commons.io.IOUtils;
import innowake.lib.job.api.FileJob;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;

/**
 * Job for {@link FileJob} tests. The job's run method reads the content of a test file, waits for 5 seconds and returns the content in the job
 * {@link Result}.
 */
public class LocalFileJob extends innowake.lib.job.api.FileJob<String> {

	/**
	 * Constructor.
	 * <p>The given jobConfigurationProperties is used to get the job result folder on the server's file system.</p>
	 * 
	 * @param jobConfigurationProperties The {@link JobConfigurationProperties}
	 */
	public LocalFileJob(final JobConfigurationProperties jobConfigurationProperties) {
		super(jobConfigurationProperties);
	}

	/**
	 * Creates the test file of this job and writes the given {@code content} to it.
	 *
	 * @throws IOException if the create or write operation failed with an I/O error
	 */
	void createTestFile(final String content) throws IOException {
		try (final OutputStream outputStream = createFile(getFileName())) {
			outputStream.write(content.getBytes(Charset.forName("UTF-8")));
		}
	}

	/**
	 * Returns the content of the test file of this job.
	 *
	 * @throws IOException if the read operation failed with an I/O error
	 */
	String getTestFile() throws IOException {
		try (final InputStream inputStream = openFile(getFileName())) {
			return new String(IOUtils.toByteArray(inputStream), Charset.forName("UTF-8"));
		}
	}

	/**
	 * Deletes the test file of this job.
	 *
	 * @throws IOException if the delete operation failed with an I/O error
	 */
	void deleteTestFile() throws IOException {
		deleteFile(getFileName());
	}

	private String getFileName() {
		return getJobId() + "_test.txt";
	}

	@Override
	protected final Result<String> run(final ProgressMonitor progressMonitor) {
		progressMonitor.setJobDescription("FileJob");
		progressMonitor.begin(1);
		
		final String content;
		try {
			content = getTestFile();
		} catch (IOException e1) {
			throw new IllegalStateException(e1);
		}

		try {
			TimeUnit.SECONDS.sleep(5);
		} catch (final InterruptedException e) {
			Thread.currentThread().interrupt();
		}

		progressMonitor.worked(1);

		return new Result<String>(content);
	}
}
