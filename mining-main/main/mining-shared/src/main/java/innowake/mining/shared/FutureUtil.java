/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared;

import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

/**
 * Utility class for awaiting the result of a collection of futures.
 */
public class FutureUtil {

	/**
	 * Await all futures in a blocking queue.
	 * <p>
	 * This methods supports new futures being added while waiting (i.e. concurrent tasks can add more futures).
	 *
	 * @param futures the futures to wait for
	 * @return the number of futures awaited
	 * @throws ExecutionException when one of the futures did not complete successfully
	 * @throws InterruptedException when interrupted while waiting
	 */
	public static long awaitAll(final BlockingQueue<Future<?>> futures) throws InterruptedException, ExecutionException {
		Future<?> future;
		long count = 0;
		while ((future = futures.poll()) != null) {
			future.get();
			count++;
		}
		return count;
	}
	
	/**
	 * Await all futures in a list.
	 * <p>
	 * This method does NOT support new futures being added while waiting (i.e. concurrent tasks must NOT add more futures).
	 *
	 * @param futures the futures to wait for
	 * @return the number of futures awaited
	 * @throws ExecutionException when one of the futures did not complete successfully
	 * @throws InterruptedException when interrupted while waiting
	 */
	public static long awaitAll(final List<Future<?>> futures) throws InterruptedException, ExecutionException {
		for (final Future<?> future : futures) {
			future.get();
		}
		return futures.size();
	}
}
