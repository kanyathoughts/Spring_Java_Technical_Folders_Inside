/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.internal.executor;

import java.io.Serializable;
import java.util.concurrent.Callable;

/**
 * Base interface for all {@linkplain Callable Callables} to be submitted for execution.
 * 
 * @param <R> the concrete type of the result
 */
public interface SerializableCallable<R extends Serializable> extends Serializable, Callable<R> {
	
}
