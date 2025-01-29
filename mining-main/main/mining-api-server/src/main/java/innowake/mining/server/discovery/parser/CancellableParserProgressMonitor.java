/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.parser;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.ndt.core.ParserProgressMonitor;

/**
 * Default {@link ParserProgressMonitor} to stop parsers.
 */
public class CancellableParserProgressMonitor implements ParserProgressMonitor {

	private static final Logger LOG = LoggerFactory.getLogger(CancellableParserProgressMonitor.class);

	private boolean cancelled;

	/**
	 * Sets this monitor to cancelled to interrupt parsing.
	 */
	public void cancel() {
		LOG.debug(() -> "Parser progress monitor: " + getClass().getSimpleName() + " was cancelled.");
		this.cancelled = true;
	}

	@Override
	public boolean isCancelled() {
		return cancelled;
	}
}
