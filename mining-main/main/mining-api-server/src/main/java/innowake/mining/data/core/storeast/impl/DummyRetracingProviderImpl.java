/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.data.core.storeast.api.RetracingProvider;
import innowake.ndt.core.assembling.IAssembling;
import innowake.ndt.core.assembling.retrace.Inclusion;
import innowake.ndt.core.assembling.retrace.Retracing;
import innowake.ndt.core.assembling.retrace.RetracingSourceOffset;
import innowake.ndt.core.parsing.ast.AstNode;

/**
 * Dummy implementation of {@link RetracingProvider}. This will be used for those language which don't have assemblers.
 * @param <T> the concrete type of {@link Retracing}
 */
public final class DummyRetracingProviderImpl<T> implements RetracingProvider<T> {

	private final IAssembling<T> assembling;

	/**
	 * Constructor.
	 * 
	 * @param assembling the assembling
	 */
	public DummyRetracingProviderImpl(final IAssembling<T> assembling) {
		this.assembling = assembling;
	}

	@Override
	public T getRoot() {
		return Assert.assertNotNull(assembling.getSourceObject(), "Source should not be null");
	}

	@Override
	@Nullable
	public Inclusion<T> getInclusion(final AstNode node) {
		return null;
	}

	@Override
	@Nullable
	public RetracingSourceOffset<T> getOffset(final AstNode node) {
		return null;
	}
}
