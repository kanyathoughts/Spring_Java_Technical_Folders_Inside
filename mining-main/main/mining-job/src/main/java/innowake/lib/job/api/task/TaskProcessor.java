/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api.task;

import java.io.Serializable;

/**
 * Handles processing of tasks provided by a {@link TaskSource} and delegates the task results to a {@link ResultConsumer}.
 * <p>
 * <b>Note: </b> Job implementors may provide their own implementation, but it is advised to always use the provided
 * {@link DefaultTaskProcessor}. Otherwise it may break the cluster if you don't know what you're doing!
 * 
 * @param <S> the concrete return type of the task
 */
public interface TaskProcessor<S extends Serializable> {

	/**
	 * Processes the tasks provided by the {@link TaskSource} and delegates the results to the {@link ResultConsumer}.
	 * This method blocks until all tasks have been submitted and all of their results are resolved.
	 * It is implementation detail how errors during task submission and result consumption are being handled.
	 * 
	 * @param taskSource the {@link TaskSource} providing the tasks to submit
	 * @param resultConsumer the {@link ResultConsumer} consuming the task results
	 */
	void process(TaskSource<S> taskSource, ResultConsumer<S> resultConsumer);
	
}
