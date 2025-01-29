/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser;

import java.util.concurrent.Callable;

import innowake.mining.data.discovery.metrics.TimedWorker.WorkerCancellationException;
import innowake.ndt.core.ParserProgressMonitor;

/**
 * A parser task that extends {@link CancellableParserProgressMonitor} for interrupting parsers.
 * 
 * <p>When the task times out with a {@link WorkerCancellationException}, then method {@link #cancel()} is called. To interrupt parsers that are executed in a 
 * {@link CancellableParserTask} set the task instance as the {@link ParserProgressMonitor} when setting up the parsers (either in parsers directly or in their 
 * configurations).</p>
 * 
 * @param <P> type of parser result
 */
public abstract class CancellableParserTask<P> extends CancellableParserProgressMonitor implements Callable<P> {

}
