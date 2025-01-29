/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.test;

import innowake.mining.data.discovery.metrics.TimedWorker;

import java.util.concurrent.Callable;
import java.util.concurrent.TimeUnit;

/**
 * Timed worker for unit tests which executes the provided task synchronously and does not provide cancellation or timeout features.
 */
public class TestTimedWorker implements TimedWorker {
	@Override
	public <T> T execute(Callable<T> task, long timeout, TimeUnit unit, MessageProvider message) throws WorkerException {
		try {
			return task.call();
		} catch (Exception e) {
			throw new WorkerException(message.get(e.getMessage()), e);
		}
	}

	@Override
	public void shutdown() {
		/* does nothing */
	}
}
