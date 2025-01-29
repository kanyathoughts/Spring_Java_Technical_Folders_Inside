/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.performance;

import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class TestExecutor {
	private final Runnable method;
	private final int warmupCount;
	private final int testCount;
	private final boolean sysout;
	private final Timetracking timer;
	private final Path outputFile;

	public TestExecutor(
			final Runnable method,
			final int warmupCount, 
			final int testCount,
			final boolean sysout,
			final Path outputFile, 
			final String resultNote) {
		this.method = method;
		this.warmupCount = warmupCount;
		this.testCount = testCount;
		this.sysout = sysout;
		this.outputFile = outputFile;
		this.timer = new Timetracking(resultNote);
	}

	public void runTest() {
		sysout("Starting Warmup");
		timer.start("warmup", false);
		for (int i = 0; i < warmupCount; i++) {
			timer.start("warmup #" + i, false);
			method.run();
			timer.stop();
		}
		sysout(warmupCount + " warmup runs took: " + timer.stop().getTime());
		
		sysout("Starting Measurement Runs");
		timer.start("measurement", false);
		for (int i = 0; i < testCount; i++) {
			timer.start("measurement #" + i);
			method.run();
			timer.stop();
		}
		sysout(testCount + " measurement runs took: " + timer.stop().getTime());
		sysout(Timetracking.csvHeader());
		final String csv = timer.toCompleteCsv();
		sysout(csv);
		sysout(timer.toReadableString());

		print(csv);
	}
	
	private void print(final String message) {
		if (outputFile != null) {
			writeToFile(outputFile, message);
		}
	}

	private void sysout(final String message) {
		if (sysout) {
			System.out.println(message);
		}
	}
	
	public static class Builder {
		private Runnable method = () -> System.out.println("No method defined");
		private int warmupCount = 0;
		private int testCount = 1;
		private boolean sysout = true;
		private Path outputFile;
		private String resultNote;

		public Builder test(final Runnable method) {
			this.method = method;
			return this;
		}
		
		public Builder warmup(final int count) {
			this.warmupCount = count;
			return this;
		}
		
		public Builder testCount(final int count) {
			this.testCount = count;
			return this;
		}

		public Builder sysout(final boolean sysout) {
			this.sysout = sysout;
			return this;
		}

		/**
		 * @param file the file to write to, must not exist
		 * @return this builder
		 * @throws IllegalStateException if the files doesn't exist and the creation or writing the header failed.
		 */
		public Builder appendResultTo(final Path file) {
			if (Files.exists(file)) {
				assertTrue(Files.isRegularFile(file));
			} else {
				try {
					Files.createFile(file);
					writeToFile(file, Timetracking.csvHeader());
				} catch (final IOException e) {
					throw new IllegalStateException(e);
				}
			}
			this.outputFile = file;
			return this;
		}

		public Builder resultNote(final String resultNote) {
			this.resultNote = resultNote;
			return this;
		}
		
		public TestExecutor build() {
			return new TestExecutor(method, warmupCount, testCount, sysout, outputFile, resultNote);
		}
	}
	
	private static void writeToFile(final Path file, final String message) {
		try {
			Files.write(file, message.getBytes());
		} catch (IOException e) {
			throw new IllegalStateException(e);
		}
	}
}
