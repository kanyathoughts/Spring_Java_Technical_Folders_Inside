/*
 * Copyright (c) 2022 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.lang;

import innowake.lib.core.api.lang.Nullable;

/**
 * Collect Exceptions (like during a series of Assertions in a test) to throw at a later point. 
 */
public class FailLater {
	
	private final AssertionError error;
	
	/**
	 * Start a new Exceptions collecting session, with context information 
	 * to be referenced in the {@link AssertionError} thrown upon calling {@link #now()}.
	 * @param context value to be used in constructing detail message
	 */
	public FailLater(@Nullable final Object context) {
		this.error = new AssertionError(context);
	}

	/**
	 * Start a new Exceptions collecting session with no context information.
	 */
	public FailLater() {
		this.error = new AssertionError();
	}
	
	/**
	 * Run a procedure and record but don't yet throw any Exception that may occur.
	 * @param action Procedure to try.
	 */
	public void later(final Runnable action) {
		try {
			action.run();
		} catch (final Throwable e) {
			error.addSuppressed(e);
		}
	}
	
	/**
	 * Assert that no Exceptions have been recorded so far.
	 * If any Exceptions occurred during previous calls to {@link #later(Runnable)}
	 * an {@link AssertionError} will be thrown bearing all recorded Exceptions as suppressed.
	 */
	public void now() {
		if (error.getSuppressed().length > 0) {
			throw error;
		}
	}

}
