/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.plugin.fieldtracer.model;

import java.nio.file.Path;
import java.nio.file.Paths;

import org.eclipse.core.runtime.CoreException;

import innowake.lib.core.api.lang.Nullable;
import innowake.ndt.fieldtracing.model.TracedModule;
import innowake.ndt.ide.core.object.IIdeObject;

/**
 * Abstract class providing implementation to Legacy Objects based on {@link IIdeObject}
 * 
 * @param <O> type of legacy object of type {@link IIdeObject}
 * @param <P> type of source object
 */
public abstract class LegacyTracedModule<O extends IIdeObject<O>, P> implements TracedModule<P> {

	protected final O object;

	/**
	 * Returns source object
	 *
	 * @return source object of type O
	 */
	public O getObject() {
		return object;
	}

	/**
	 * Constructor to instantiate legacy object
	 * 
	 * @param object of type O
	 */
	protected LegacyTracedModule(final O object) {
		super();
		this.object = object;
	}

	@Override
	public String getName() {
		return object.getFileName();
	}

	@Override
	public String getContent() {
		try {
			return object.getContentAsString();
		} catch (final CoreException e) {
			throw new IllegalStateException("cannot retrieve contents from module", e);
		}
	}

	@Override
	public Path getPath() {
		return Paths.get(object.getObjectPath());
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + object.hashCode();
		return result;
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		final LegacyTracedModule<?,?> other = (LegacyTracedModule<?,?>) obj;
		if ( ! object.equals(other.object))
			return false;
		return true;
	}
	
}
