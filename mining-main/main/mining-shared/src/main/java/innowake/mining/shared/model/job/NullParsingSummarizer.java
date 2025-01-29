/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.job;

import innowake.mining.shared.access.EntityId;

/**
 * {@link ParsingSummarizer} implementation which does nothing.
 */
public final class NullParsingSummarizer implements ParsingSummarizer {

	public final static NullParsingSummarizer INSTANCE = new NullParsingSummarizer();
	
	private NullParsingSummarizer() {
	}
	
	@Override
	public void success(final EntityId moduleId) {
	}

	@Override
	public void error(final EntityId moduleId) {
	}

	@Override
	public void unsupported(final EntityId moduleId) {
	}
}