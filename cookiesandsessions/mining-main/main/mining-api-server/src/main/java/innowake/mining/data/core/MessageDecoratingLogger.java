/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core;

import java.util.function.Function;
import java.util.function.Supplier;

import innowake.lib.core.api.lang.NonNullByDefault;
import innowake.lib.core.log.Logger;


/**
 * A logger which decorates the log message (pattern) by a decorating function.
 */
@NonNullByDefault(false)
public class MessageDecoratingLogger implements Logger {
	
	private final Function<String, String> decoratingFunction;
	private final Logger logger;
	
	/**
	 * Creates a new decorating logger, with the decorating function and the logger.
	 * 
	 * @param decoratingFunction the function to apply to the log message
	 * @param logger the logger to use for logging
	 */
	public MessageDecoratingLogger(final Function<String, String> decoratingFunction, final Logger logger) {
		this.decoratingFunction = decoratingFunction;
		this.logger = logger;
	}

	@Override
	public String getName() {
		return logger.getName();
	}

	@Override
	public boolean isTraceEnabled() {
		return logger.isTraceEnabled();
	}

	@Override
	public void trace(final String msg) {
		logger.trace(() -> decorate(msg));
	}

	@Override
	public void trace(final Supplier<String> msgSupplier) {
		logger.trace(() -> decorate(msgSupplier.get()));
	}

	@Override
	public void trace(final String format, final Object arg) {
		if (isTraceEnabled()) {
			logger.trace(decorate(format), arg);
		}
	}

	@Override
	public void trace(final String format, final Object arg1, final Object arg2) {
		if (isTraceEnabled()) {
			logger.trace(decorate(format), arg1, arg2);
		}
	}

	@Override
	public void trace(final String format, final Object... argArray) {
		if (isTraceEnabled()) {
			logger.trace(decorate(format), argArray);
		}
	}

	@Override
	public void trace(final String msg, final Throwable t) {
		logger.trace(() -> decorate(msg), t);
	}

	@Override
	public void trace(final Supplier<String> msgSupplier, final Throwable t) {
		logger.trace(() -> decorate(msgSupplier.get()), t);
	}

	@Override
	public boolean isDebugEnabled() {
		return logger.isDebugEnabled();
	}

	@Override
	public void debug(final String msg) {
		logger.debug(() -> decorate(msg));
	}

	@Override
	public void debug(final Supplier<String> msgSupplier) {
		logger.debug(() -> decorate(msgSupplier.get()));
	}

	@Override
	public void debug(final String format, final Object arg) {
		if (isDebugEnabled()) {
			logger.debug(decorate(format), arg);
		}
	}

	@Override
	public void debug(final String format, final Object arg1, final Object arg2) {
		if (isDebugEnabled()) {
			logger.debug(decorate(format), arg1, arg2);
		}
	}

	@Override
	public void debug(final String format, final Object... argArray) {
		if (isDebugEnabled()) {
			logger.debug(decorate(format), argArray);
		}
	}

	@Override
	public void debug(final String msg, final Throwable t) {
		logger.debug(() -> decorate(msg), t);
	}

	@Override
	public void debug(final Supplier<String> msgSupplier, final Throwable t) {
		logger.debug(() -> decorate(msgSupplier.get()), t);
	}

	@Override
	public boolean isInfoEnabled() {
		return logger.isInfoEnabled();
	}

	@Override
	public void info(final String msg) {
		logger.info(() -> decorate(msg));
	}

	@Override
	public void info(final Supplier<String> msgSupplier) {
		logger.info(() -> decorate(msgSupplier.get()));
	}

	@Override
	public void info(final String format, final Object arg) {
		if (isInfoEnabled()) {
			logger.info(decorate(format), arg);
		}
	}

	@Override
	public void info(final String format, final Object arg1, final Object arg2) {
		if (isInfoEnabled()) {
			logger.info(decorate(format), arg1, arg2);
		}
	}

	@Override
	public void info(final String format, final Object... argArray) {
		if (isInfoEnabled()) {
			logger.info(decorate(format), argArray);
		}
	}

	@Override
	public void info(final String msg, final Throwable t) {
		logger.info(() -> decorate(msg), t);
	}

	@Override
	public void info(final Supplier<String> msgSupplier, final Throwable t) {
		logger.info(() -> decorate(msgSupplier.get()), t);
	}

	@Override
	public boolean isWarnEnabled() {
		return logger.isWarnEnabled();
	}

	@Override
	public void warn(final String msg) {
		logger.warn(() -> decorate(msg));
	}

	@Override
	public void warn(final Supplier<String> msgSupplier) {
		logger.warn(() -> decorate(msgSupplier.get()));
	}

	@Override
	public void warn(final String format, final Object arg) {
		if (isWarnEnabled()) {
			logger.warn(decorate(format), arg);
		}
	}

	@Override
	public void warn(final String format, final Object... argArray) {
		if (isWarnEnabled()) {
			logger.warn(decorate(format), argArray);
		}
	}

	@Override
	public void warn(final String format, final Object arg1, final Object arg2) {
		if (isWarnEnabled()) {
			logger.warn(decorate(format), arg1, arg2);
		}
	}

	@Override
	public void warn(final String msg, final Throwable t) {
		logger.warn(() -> decorate(msg), t);
	}

	@Override
	public void warn(final Supplier<String> msgSupplier, final Throwable t) {
		logger.warn(() -> decorate(msgSupplier.get()), t);
	}

	@Override
	public boolean isErrorEnabled() {
		return logger.isErrorEnabled();
	}

	@Override
	public void error(final String msg) {
		logger.error(() -> decorate(msg));
	}

	@Override
	public void error(final Supplier<String> msgSupplier) {
		logger.error(() -> decorate(msgSupplier.get()));
	}

	@Override
	public void error(final String format, final Object arg) {
		if (isErrorEnabled()) {
			logger.error(decorate(format), arg);
		}
	}

	@Override
	public void error(final String format, final Object arg1, final Object arg2) {
		if (isErrorEnabled()) {
			logger.error(decorate(format), arg1, arg2);
		}
	}

	@Override
	public void error(final String format, final Object... argArray) {
		if (isErrorEnabled()) {
			logger.error(decorate(format), argArray);
		}
	}

	@Override
	public void error(final String msg, final Throwable t) {
		logger.error(() -> decorate(msg), t);
	}

	@Override
	public void error(final Supplier<String> msgSupplier, final Throwable t) {
		logger.error(() -> decorate(msgSupplier.get()), t);
	}

	private String decorate(final String message) {
		return decoratingFunction.apply(message);
	}
}
