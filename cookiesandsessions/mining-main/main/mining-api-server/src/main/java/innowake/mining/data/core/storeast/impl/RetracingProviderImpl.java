/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.storeast.impl;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.storeast.api.RetracingProvider;
import innowake.ndt.core.assembling.IAssembling;
import innowake.ndt.core.assembling.retrace.Inclusion;
import innowake.ndt.core.assembling.retrace.Retracer;
import innowake.ndt.core.assembling.retrace.Retracing;
import innowake.ndt.core.assembling.retrace.RetracingSourceOffset;
import innowake.ndt.core.parsing.ast.AstNode;

/**
 * Core implementation of {@link RetracingProvider}.
 * 
 * @param <T> the concrete type of {@link Retracing}
 */
public final class RetracingProviderImpl<T> implements RetracingProvider<T> {

	private final Retracing<T> retracing;

	/**
	 * Constructor.
	 * 
	 * @param assembling the assembling
	 */
	public RetracingProviderImpl(final IAssembling<T> assembling) {
		retracing = new Retracer<T>().retrace(assembling);
	}
	
	@Override
	public T getRoot() {
		return retracing.getOriginal();
	}
	
	@Override
	@Nullable
	public Inclusion<T> getInclusion(final AstNode node) {
		try {
			return retracing.findInclusion(node.getStartOffset());
		} catch (final IllegalStateException e) {
			/* is actually an intended ArtificialTokenException from ndt-cobolparser
			 * we cannot find out if this exception occurs and avoid it
			 * do not log */
			return null;
		}
	}
	
	@Override
	@Nullable
	public RetracingSourceOffset<T> getOffset(final AstNode node) {
		try {
			return retracing.retrace(node.getStartOffset());
		} catch (final IllegalStateException e) {
			/* is actually an intended ArtificialTokenException from ndt-cobolparser
			 * we cannot find out if this exception occurs and avoid it
			 * do not log */
			return null;
		}
	}
}
