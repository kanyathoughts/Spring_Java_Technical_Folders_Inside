/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.plugin.ui;

import java.util.concurrent.Callable;

import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.browser.BrowserFunction;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.plugin.Logging;

/**
 * Callback functions for interacting with the Mining UI web components.
 * 
 * @param <V> the type of the return value of the function
 */
public class CallbackFunction<V> extends BrowserFunction {

	private Callable<V> callable;

	/**
	 * Creates a new callback function.
	 * 
	 * @param browser the browser into which this function should be registered
	 * @param name the name of the callback function
	 * @param callable the callable which gets called when the function is triggered
	 */
	public CallbackFunction(final Browser browser, final String name, final Callable<V> callable) {
		super(browser, name);
		this.callable = callable;
	}

	@Nullable
	@Override
	public Object function(@Nullable final Object[] arguments) {
		try {
			return callable.call();
		} catch (final Exception e) {
			Logging.error("An error occurred while trying to call the callable for the browser callback function '" + getName() + "'", e);
			throw new IllegalStateException(e);
		}
	}
}
