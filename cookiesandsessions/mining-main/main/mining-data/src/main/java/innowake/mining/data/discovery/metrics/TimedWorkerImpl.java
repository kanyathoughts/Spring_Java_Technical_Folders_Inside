/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.data.discovery.metrics;

import java.util.concurrent.Callable;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import brave.Span;
import brave.Tracer;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;

/**
 * A worker to execute long running tasks with the possibility to set a timeout
 * and cancel the task via a monitor.</br>
 *
 * <b>Note:</b> Every job worker thread is supposed to have its own instance of this class!
 */
public class TimedWorkerImpl implements TimedWorker {

	private static final Logger LOG = LoggerFactory.getLogger(TimedWorker.class.getCanonicalName());

	@Nullable
	private final ProgressMonitor monitor;
	private ScheduledThreadPoolExecutor executor;
	@Nullable
	private final Tracer tracer;
	@Nullable
	private final Span activeSpan;

	/**
	 * Constructor.
	 *
	 * @param monitor the optional {@link ProgressMonitor} to monitor for cancellation
	 * @param tracer the optional {@link Tracer} if tracing should be active for the started threads
	 * @param activeSpan the optional parent {@link Span} if tracing is active
	 */
	public TimedWorkerImpl(@Nullable final ProgressMonitor monitor, @Nullable final Tracer tracer, @Nullable final Span activeSpan) {
		this.monitor = monitor;
		this.executor = new ScheduledThreadPoolExecutor(monitor != null ? 3 : 2);
		this.tracer = tracer;
		this.activeSpan = activeSpan;
	}

	@Override
	public <T> T execute(final Callable<T> task, final long timeout, final TimeUnit unit, final TimedWorker.MessageProvider message) throws WorkerException {
		/* technically not necessary to establish the scope here, since the execute call is most likely already happening in the established scope,
		 * but this is just to be sure. */
		try (final Tracer.SpanInScope parentScope = tracer != null ? tracer.withSpanInScope(Assert.assertNotNull(activeSpan)) : null) {
			/* Execute directly if period is negative. */
			if (timeout < 0) {
				try {
					return task.call();
				} catch (final Exception exception) {
					if (activeSpan != null) {
						activeSpan.error(exception);
					}
					throw new WorkerException(message.get("Worker thread threw an error"), exception);
				}
			}

			final Future<T> future = executor.submit(() -> {
				try (final Tracer.SpanInScope scope = tracer != null ? tracer.withSpanInScope(Assert.assertNotNull(activeSpan)) : null) {
					/* we keep the actual work of the task to be executed under the same parent span, since the additional threads are only
					 * for timeout and cancel purposes. */
					return task.call();
				}
			});

			/* Schedule Timeout */
			final ScheduledFuture<?> timeoutFuture = executor.schedule(() -> {
				try (final Tracer.SpanInScope scope = tracer != null ? tracer.withSpanInScope(Assert.assertNotNull(activeSpan)) : null) {
					/* we keep this in the same parent span, since the additional thread is only for timeout and cancel purposes. */
					future.cancel(true);
					LOG.debug(() -> message.get(String.format("Worker thread was canceled after %d %s", Long.valueOf(timeout), unit.name())));
				}
			}, timeout, unit);

			final Future<?> checkCanceled = checkCanceled(future, monitor);

			try {
				return future.get();
			} catch (final InterruptedException e) {
				if (activeSpan != null) {
					activeSpan.error(e);
				}
				LOG.error(message.get("Worker thread was interrupted"), e);
				Thread.currentThread().interrupt();
				throw new WorkerException(message.get("Worker thread was interrupted"), e);
			} catch (final ExecutionException e) {
				if (activeSpan != null) {
					activeSpan.error(e);
				}
				throw new WorkerException(message.get("Worker thread threw an error"), e);
			} catch (final CancellationException e) {
				final String actualMessage = e.getMessage() != null ? message.get("Worker thread was canceled " + e.getMessage())
						: message.get("Worker thread was cancelled after " + Long.valueOf(timeout) + " " + unit.name() + " probably due to a timeout");
				throw new WorkerCancellationException(actualMessage);
			} finally {
				timeoutFuture.cancel(true);
				checkCanceled.cancel(true);
			}
		}
	}

	/**
	 * Checks the monitor if it's canceled every 3 seconds.
	 *
	 * @param future the task to be canceled
	 * @return the handle of this periodic task
	 */
	private Future<?> checkCanceled(final Future<?> future, @Nullable final ProgressMonitor monitor) {
		if (monitor != null) {
			return executor.scheduleWithFixedDelay(() -> {
				try (final Tracer.SpanInScope scope = tracer != null ? tracer.withSpanInScope(Assert.assertNotNull(activeSpan)) : null) {
					/* we keep this in the same parent span, since the additional thread is only for timeout and cancel purposes. */
					if (monitor.isCanceled()) {
						future.cancel(true);
					}
				}
			}, 3, 3, TimeUnit.SECONDS);
		}
		return CompletableFuture.completedFuture(null);
	}

	@Override
	public void shutdown() {
		executor.shutdownNow();
	}
}
