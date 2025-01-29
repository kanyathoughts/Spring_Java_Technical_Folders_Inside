/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.genai;

import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.server.properties.GenericConfigProperties;
import org.springframework.stereotype.Service;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;


/**
 *  This is a singleton service which creates a new instance of MonitoredTaskManager
 */
@Service
public class MonitoredTaskManagerService {

	private int maxParallelTasks = 5;
	private final GenericConfigProperties configProperties;
	private final ExecutorService executors;
	private final Map<UUID, MonitoredTaskManager> taskManagers = new ConcurrentHashMap<>();

	public MonitoredTaskManagerService(final GenericConfigProperties configProperties) {
		this.configProperties = configProperties;
		final int maxParallelTask = getConfigProperties().getMaxParallelTasks().orElse(maxParallelTasks);
		this.executors =  Executors.newFixedThreadPool(maxParallelTask);
	}

	public GenericConfigProperties getConfigProperties() {
		return configProperties;
	}

	/**
	 * The method provides a new instance of MonitoredTaskManager
	 * @param progressMonitor The parameter is coming from the services which autowired this service
	 * @param numberOfProgressSteps The total number of progress steps for the task. This is used to initialize the progress monitor and
	 *                              to generate the step description.
	 * @return MonitoredTaskManager
	 */
	public MonitoredTaskManager newTaskManager(final ProgressMonitor progressMonitor, final int numberOfProgressSteps) {
		final MonitoredTaskManager taskManager = new MonitoredTaskManager(progressMonitor, executors, numberOfProgressSteps);
		taskManagers.put(taskManager.getManagerId(), taskManager);
		return taskManager;
	}

	/**
	 * The method provides a subtask manager
	 * @param parentTaskManagerId The id of the main monitored task manager
	 * @param numberOfProgressSteps The number of steps in progress
	 * @return a Sub monitored task manager
	 */
	public MonitoredTaskManager newSubTaskManager(final UUID parentTaskManagerId, final int numberOfProgressSteps) {
		final MonitoredTaskManager parent = taskManagers.get(parentTaskManagerId);
		final MonitoredTaskManager taskManager = parent.createSubTaskManager(numberOfProgressSteps);
		taskManagers.put(taskManager.getManagerId(), taskManager);
		return taskManager;
	}
}
