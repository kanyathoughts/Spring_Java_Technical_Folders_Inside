/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;

/**
 * Base implementation for all jobs that require file access (read/write/delete).
 * <p>By default this class uses the job result folder on the server's file system for all file operations,
 * see {@link JobConfigurationProperties#getJobResultFolder()}.</p>
 * 
 * @param <X> the concrete result type of the job
 */
public abstract class FileJob<X extends Serializable> extends Job<X> {
	
	private static final long serialVersionUID = 1L;
	private final Set<String> files = new HashSet<>();

	/**
	 * Constructor.
	 * <p>The given jobConfigurationProperties is used to get the job result folder on the server's file system.</p>
	 * 
	 * @param jobConfigurationProperties The {@link JobConfigurationProperties}
	 */
	protected FileJob(final JobConfigurationProperties jobConfigurationProperties) {
		super();
		this.jobConfigurationProperties = jobConfigurationProperties;
	}

	/**
	 * Creates a file with the given {@ode name} and returns an output stream for writing bytes to the file.
	 * <p>The file is deleted automatically when the job has finished by the {@link FileJob#afterRun()} method.</p>
	 * <p>Since the same job can be started multiple times and run concurrently, the given file {@ode name} should contain the job Id.</p>
	 * <p>The file is created in the job result folder on the server's file system, see {@link JobConfigurationProperties#getJobResultFolder()}.</p>
	 * 
	 * @param name The name of the to be created file
	 * @return a writable {@code OutputStream} of the created file
	 * @throws IOException if the output file can not be created or opened
	 */
	protected OutputStream createFile(final String name) throws IOException {
		files.add(name);
		return createFileInResultFolder(name);
	}

	/**
	 * Opens the file for the given {@code name} and returns an input stream to read from the file.
	 * <p>This methods looks up the file in the job result folder on the server's file system, see {@link JobConfigurationProperties#getJobResultFolder()}.</p>
	 * 
	 * @param name The name of the to be created file
	 * @return a writable {@code OutputStream} of the created file
	 * @throws IOException if the file doesn't exists or can not be opened
	 */
	protected InputStream openFile(final String name) throws IOException {
		return Files.newInputStream(Paths.get(jobConfigurationProperties.getJobResultFolder(), name));
	}

	/**
	 * Deletes the file for the given {@code name}, if it exists.
	 * <p>This methods looks up the file in the job result folder on the server's file system, see {@link JobConfigurationProperties#getJobResultFolder()}.</p>
	 * 
	 * @param name The name of the to be created file
	 * @return {@code true} if the file was deleted; {@code false} if the file does not exist
	 * @throws IOException if an I/O error occurred
	 */
	protected boolean deleteFile(final String name) throws IOException {
		files.remove(name);
		return Files.deleteIfExists(Paths.get(jobConfigurationProperties.getJobResultFolder(), name));
	}

	@Override
	protected void afterRun() {
		super.afterRun();

		new ArrayList<>(files).forEach(fileName -> {
			try {
				deleteFile(fileName);
			} catch (final Exception exception) {
				final String name = fileName;
				LOG.error(() -> "Error when deleting job file " + name, exception);
			}
		});
	}
}
