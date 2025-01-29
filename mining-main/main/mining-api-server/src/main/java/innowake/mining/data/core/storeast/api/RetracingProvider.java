/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core.storeast.api;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.DummyIAssembling;
import innowake.mining.data.core.storeast.impl.DummyRetracingProviderImpl;
import innowake.mining.data.core.storeast.impl.RetracingProviderImpl;
import innowake.ndt.core.assembling.IAssembling;
import innowake.ndt.core.assembling.retrace.Inclusion;
import innowake.ndt.core.assembling.retrace.Retracing;
import innowake.ndt.core.assembling.retrace.RetracingSourceOffset;
import innowake.ndt.core.parsing.ast.AstNode;

/**
 * Provides information about retraced objects.
 * 
 * @param <T> the concrete type of {@link Retracing}
 */
public interface RetracingProvider<T> {

	/**
	 * Creates a new instance.
	 * 
	 * @param assembling the assembling
	 * @return a new instance
	 */
	public static <T> RetracingProvider<T> createInstance(final IAssembling<T> assembling) {
		if (assembling.getClass().equals(DummyIAssembling.class)) {
			return new DummyRetracingProviderImpl<>(assembling);
		} else {
			return new RetracingProviderImpl<>(assembling);
		}
	}
	
	/**
	 * Returns the root object being retraced.
	 *
	 * @return the root object
	 */
	public T getRoot();
	
	/**
	 * Returns the inclusion that encloses the given node, or {@code null} if no inclusion exists.
	 *
	 * @param node the node
	 * @return the inclusion, or {@code null}
	 */
	@Nullable
	public Inclusion<T> getInclusion(final AstNode node);
	
	/**
	 * Returns the offset in the original object for the given node, or {@code null} if the offset cannot be retrieved.
	 *
	 * @param node the node
	 * @return the offset, or {@code null}
	 */
	@Nullable
	public RetracingSourceOffset<T> getOffset(final AstNode node);
}
