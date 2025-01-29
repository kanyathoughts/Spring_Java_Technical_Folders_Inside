/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.internal.executor;

import java.io.Serializable;

/**
 * Base interface for all {@linkplain Runnable Runnables} to be submitted for execution.
 */
public interface SerializableRunnable extends Serializable, Runnable {

}
