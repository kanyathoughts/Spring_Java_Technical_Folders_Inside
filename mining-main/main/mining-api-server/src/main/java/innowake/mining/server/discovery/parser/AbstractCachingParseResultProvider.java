/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.parser;

import java.util.concurrent.Callable;
import java.util.concurrent.TimeUnit;

import innowake.lib.core.lang.Assert;
import innowake.mining.data.discovery.metrics.TimedWorker;
import innowake.mining.data.discovery.metrics.TimedWorker.MessageProvider;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerCancellationException;
import innowake.mining.data.discovery.metrics.TimedWorker.WorkerException;
import innowake.mining.server.service.ParseResultCacheService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.SourcePojo;

/**
 * Base class of all language {@link ParseResultProvider ParseResultProviders} that cache the parser & assembling results.
 * @param <P> type of parser result
 */
public abstract class AbstractCachingParseResultProvider<P> implements ParseResultProvider<P> {

	/** The {@link TimeUnit} for the parser timeout, currently seconds. */
	protected static final TimeUnit UNIT = TimeUnit.SECONDS;

	/** The timeout for the parser worker in seconds or -1 when no timeout is required. */
	protected final int timeout;

	/** The {@link TimedWorker} that executes the parser tasks. */
	protected final TimedWorker worker;

	protected final String jobId;

	/** The {@link ParseResultCacheService} that caches the parse result. */
	protected final ParseResultCacheService parseResultCacheService;

	/**
	 * Creates an instance of AbstractCachingParseResultProvider.
	 *
	 * @param worker The {@link TimedWorker} that executes the parser tasks
	 * @param timeout The timeout for the parser worker in seconds or -1 when no timeout is required
	 * @param jobId The job Id
	 * @param parseResultCacheService The {@link ParseResultCacheService} To cache the parser results
	 */
	protected AbstractCachingParseResultProvider(final TimedWorker worker, final int timeout, final String jobId,
			final ParseResultCacheService parseResultCacheService) {
		this.worker = worker;
		this.timeout = timeout;
		this.jobId = jobId;
		this.parseResultCacheService = parseResultCacheService;
	}

	/**
	 * <p>If the cache is enabled first a lookup into the cache is done whether it already contains an entry for the specified {@code source}.
	 * If an entry is available then it is returned immediately without parsing.</p>
	 * <p>If caching is disabled or no entry is available, then the given {@code task} is called to return the parse result for the source content. The result
	 * is put into the cache</p>
	 * <p>The given {@code source} is only used to calculate the cache key </p>
	 *
	 * @param source The source object for calculating the key for caching
	 * @param msg The language specific {@link MessageProvider}
	 * @param task The parser task that does the actual parsing and assembling
	 * @return parse result
	 * @throws WorkerCancellationException when the Worker thread was cancelled
	 * @throws DiscoveryException when the worker thread was interrupted or threw an error
	 */
	protected P getParseResult(final SourcePojo source, final MessageProvider msg, final Callable<P> task) throws DiscoveryException, WorkerCancellationException {
		try {
			return getResult(source, msg, task);
		} catch (final WorkerCancellationException e) {
			throw e;
		} catch (final WorkerException exception) {
			throw new DiscoveryException(exception.getCause() != null ? exception.getCause().getMessage() : exception.getMessage(), exception);
		}
	}
	
	/**
	 * <p>If the cache is enabled first a lookup into the cache is done whether it already contains an entry for the specified {@code source}.
	 * If an entry is available then it is returned immediately without parsing.</p>
	 * <p>If caching is disabled or no entry is available, then the given {@code task} is called to return the parse result for the source content. The result
	 * is put into the cache</p>
	 * <p>The given {@code source} is only used to calculate the cache key </p>
	 * <p>If the task times out with a {@link WorkerCancellationException} then {@link CancellableParserTask#setCancelled(boolean)} is called with {@code true}</p>
	 *
	 * @param source The source object for calculating the key for caching
	 * @param msg The language specific {@link MessageProvider}
	 * @param task The parser task that does the actual parsing and assembling
	 * @return parse result
	 * @throws WorkerCancellationException when the Worker thread was cancelled
	 * @throws DiscoveryException when the worker thread was interrupted or threw an error
	 */
	protected P getParseResult(final SourcePojo source, final MessageProvider msg, final CancellableParserTask<P> task) throws DiscoveryException, WorkerCancellationException {
		try {
			return getResult(source, msg, task);
		} catch (final WorkerCancellationException e) {
			task.cancel();
			throw e;
		} catch (final WorkerException exception) {
			task.cancel();
			throw new DiscoveryException(exception.getCause() != null ? exception.getCause().getMessage() : exception.getMessage(), exception);
		}
	}

	private P getResult(final SourcePojo source, final MessageProvider msg, final Callable<P> task) throws WorkerException {
		final String key = getKey(source);
		P parseResult = parseResultCacheService.getCached(jobId, key);

		if (parseResult == null) {
			parseResult = Assert.assertNotNull(worker.execute(task, timeout, UNIT, msg));
			parseResultCacheService.cache(jobId, key, parseResult);
			return parseResult;
		}

		return parseResult;
	}

	/**
	 * Returns the key for the given {@code sourceObject}.
	 *
	 * @param sourceObject The source object for calculating the key for caching
	 * @return the key for caching
	 */
	protected String getKey(final SourcePojo sourceObject) {
		return sourceObject.getPath() + "$" + this.getClass().getSimpleName();
	}

}
