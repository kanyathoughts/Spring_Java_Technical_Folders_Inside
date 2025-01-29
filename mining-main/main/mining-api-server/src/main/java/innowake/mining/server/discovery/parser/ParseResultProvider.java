/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser;

import innowake.mining.data.discovery.metrics.TimedWorker.WorkerCancellationException;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.SourcePojo;

/**
 * Provides language specific parser & assembling results.
 * @param <P> type of parser result
 */
public interface ParseResultProvider<P> {

	/**
	 * Returns the language specific parse result for the given {@code sourceObject}
	 *
	 * @param sourceObject The source object to parse
	 * @return The parse result
	 * @throws WorkerCancellationException when the worker thread was cancelled
	 * @throws DiscoveryException when the worker thread was interrupted or threw an error
	 */
	P getParseResult(final SourcePojo sourceObject) throws DiscoveryException, WorkerCancellationException;
}
