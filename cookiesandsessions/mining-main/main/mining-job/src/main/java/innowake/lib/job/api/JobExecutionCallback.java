/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api;

import innowake.lib.core.api.lang.Nullable;

/**
 * The callback can be used to track the execution of a submitted {@link Job}.
 */
public interface JobExecutionCallback {

	/**
	 * Called upon successful execution of a {@link Job}.
	 */
	void onCompletion();
	
	/**
	 * Called upon execution of a {@link Job} with errors.
	 * 
	 * @param throwable the {@link Throwable} of the error
	 */
    void onFailure(@Nullable Throwable throwable);
}
