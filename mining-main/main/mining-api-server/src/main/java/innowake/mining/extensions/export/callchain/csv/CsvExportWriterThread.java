/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.export.callchain.csv;

import innowake.mining.extensions.export.callchain.model.CallChain;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 * A {@link Thread} implementation for writing {@link CallChain Callchains} with a {@link CallChainCSVWriter}.
 * <p>
 * Use {@link #put(CallChain)} and {@link #waitForFinish()} to interact with the thread.
 */
public class CsvExportWriterThread extends Thread {

	/* Used for 'poison pill shutdown' */
	public static final CallChain STOP_SIGNAL = new CallChain(CallChain.CallChainDirection.IN);

	private final CountDownLatch finishLatch = new CountDownLatch(1);
	private final BlockingQueue<CallChain> callChainQueue = new ArrayBlockingQueue<>(1);
	private final CallChainCSVWriter writer;

	public CsvExportWriterThread(final CallChainCSVWriter writer) {
		super("CallChain CSV export thread");
		this.writer = writer;
	}

	@Override
	public final void run() {
		try {
			CallChain callChain;
			while ((callChain = callChainQueue.take()) != STOP_SIGNAL) {
				writer.writeCallChain(callChain);
			}
		} catch (final InterruptedException e) {
			Thread.currentThread().interrupt();
		} finally {
			finishLatch.countDown();
		}
	}

	/**
	 * Inserts the given {@code callChain} into the writer queue, waiting if necessary for space to become available.
	 *
	 * @param callChain the {@link CallChain} to write
	 */
	public void put(final CallChain callChain) {
		try {
			callChainQueue.put(callChain);
		} catch (final InterruptedException exc) {
			Thread.currentThread().interrupt();
		}
	}

	/**
	 * Causes the current thread to wait until this {@link CsvExportWriterThread} finished or got interrupted, or five minutes have elapses.
	 *
	 * @return {@code true} if the thread finished gracefully. Otherwise {@code false}
	 */
	public boolean waitForFinish() {
		try {
			return finishLatch.await(5, TimeUnit.MINUTES);
		} catch (final InterruptedException exc) {
			Thread.currentThread().interrupt();
		}

		return false;
	}
}
