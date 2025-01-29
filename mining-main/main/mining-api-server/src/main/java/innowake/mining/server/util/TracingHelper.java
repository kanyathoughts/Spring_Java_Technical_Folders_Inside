package innowake.mining.server.util;

import java.util.concurrent.Callable;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import brave.Span;
import brave.Tracer;
import brave.Tracer.SpanInScope;
import innowake.lib.job.api.Job;

/**
 * Service for dealing with log tracing when not in the context of an active REST call.
 * <p>
 * This can be used for example when starting a background {@link Job job}.
 */
@Service
public class TracingHelper {

	@Autowired
	private Tracer tracer;

	/**
	 * Wraps a given runnable with the parent tracing scope.
	 * <p>
	 * If there is no active tracing a new one will be created. 
	 *
	 * @param runnable the runnable to wrap
	 * @return a wrapped runnable, which when run, will run in the parent tracing scope
	 */
	public Runnable runInParentScope(final Runnable runnable) {
		final Span currentSpan = getSpan();
		return () -> {
			try (final SpanInScope scope = tracer.withSpanInScope(currentSpan)) {
				runnable.run();
			}
		};
	}

	/**
	 * Wraps a given callable with the parent tracing scope.
	 * <p>
	 * If there is no active tracing a new one will be created. 
	 *
	 * @param <T> the type of the return value of the callable
	 * @param callable the callable to wrap
	 * @return a wrapped callable, which when called, will run in the parent tracing scope
	 */
	public <T> Callable<T> runInParentScope(final Callable<T> callable) {
		final Span currentSpan = getSpan();
		return () -> {
			try (final SpanInScope scope = tracer.withSpanInScope(currentSpan)) {
				return callable.call();
			}
		};
	}

	/**
	 * Wraps a given runnable with a new tracing scope.
	 *
	 * @param runnable the runnable to wrap
	 * @return a wrapped runnable, which when run, will run in a new tracing scope
	 */
	public Runnable runInNewScope(final Runnable runnable) {
		final Span currentSpan = tracer.currentSpan();
		return () -> {
			try (final SpanInScope scope = tracer.withSpanInScope(currentSpan)) {
				final Span newSpan = tracer.nextSpan();
				try (final SpanInScope newScope = tracer.withSpanInScope(newSpan)) {
					runnable.run();
				} finally {
					newSpan.finish();
				}
			}
		};
	}
	
	/**
	 * Wraps a given callable with a new tracing scope.
	 *
	 * @param <T> the type of the return value of the callable
	 * @param callable the callable to wrap
	 * @return a wrapped callable, which when called, will run in a new tracing scope
	 */
	public <T> Callable<T> runInNewScope(final Callable<T> callable) {
		final Span currentSpan = tracer.currentSpan();
		return () -> {
			try (final SpanInScope scope = tracer.withSpanInScope(currentSpan)) {
				final Span newSpan = tracer.nextSpan();
				try (final SpanInScope newScope = tracer.withSpanInScope(newSpan)) {
					return callable.call();
				} finally {
					newSpan.finish();
				}
			}
		};
	}

	private Span getSpan() {
		Span currentSpan = tracer.currentSpan();
		if (currentSpan == null) {
			currentSpan = tracer.newTrace();
		} 
		return currentSpan;
	}

}
