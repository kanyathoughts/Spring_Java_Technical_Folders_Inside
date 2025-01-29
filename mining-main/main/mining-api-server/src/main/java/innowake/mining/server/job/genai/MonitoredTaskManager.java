/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.genai;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * The class is responsible for running some job tasks in parallel
 */
public class MonitoredTaskManager {

	private static final Logger LOG = LoggerFactory.getLogger(MonitoredTaskManager.class);
	private final ProgressMonitor progressMonitor;
	private final CurrentThreadCounts threadNum = new CurrentThreadCounts();
	private final List<CompletableFuture<Void>> futures = new ArrayList<>();
	private final ExecutorService executorService;
	private final AtomicInteger workedCount  = new AtomicInteger(0);
	private final int numberOfProgressSteps;
	private final List<MonitoredTaskManager> subManagers = new ArrayList<>();
	private final UUID managerId = UUID.randomUUID();

	/**
	 *
	 * @param progressMonitor the progress monitor of the job
	 * @param executorService the {@link ExecutorService}
	 * @param numberOfProgressSteps The total number of progress steps for the task.
	 *                              This is used to initialize the progress monitor and
	 *                              to generate the step description.
	 */
	public MonitoredTaskManager(final ProgressMonitor progressMonitor, final ExecutorService executorService,
			final int numberOfProgressSteps) {
		this.progressMonitor = progressMonitor;
		this.executorService = executorService;
		this.numberOfProgressSteps = numberOfProgressSteps;
	}

	/**
	 * Executes a given task in parallel with a specified number of progress steps.
	 * This method is intended for tasks that do not require exception handling.
	 *
	 * @param <T> The type of the parameter that the task function accepts.
	 * @param taskFunction The function representing the task to be executed.
	 *                     It accepts a MonitoredTaskParameter and a parameter of type T.
	 * @param parameter The parameter to be passed to the task function.
	 */
	public <T> void run(final BiConsumer<MonitoredTaskParameter, T> taskFunction,
			final T parameter) {
		LOG.debug(() -> String.format("The number of Progress steps %s", numberOfProgressSteps));
		progressMonitor.begin(numberOfProgressSteps);
		final Function<MonitoredTaskParameter, String> stepDescriptionSupplier =
				monitoredTaskParameter ->  " / " + numberOfProgressSteps;
		run(taskFunction, parameter, stepDescriptionSupplier);
	}

	/**
	 * Executes a given task in parallel with step descriptions.
	 * This method is intended for tasks that do not require exception handling.
	 *
	 * @param <T> The type of the parameter that the task function accepts.
	 * @param taskFunction The function representing the task to be executed.
	 *                     It accepts a MonitoredTaskParameter and a parameter of type T.
	 * @param parameter The parameter to be passed to the task function.
	 * @param stepDescriptionSupplier The function provide what should be done for step descriptions
	 */
	public <T> void run(final BiConsumer<MonitoredTaskParameter, T> taskFunction,
			final T parameter,
			final Function<MonitoredTaskParameter, String> stepDescriptionSupplier) {
		run(taskFunction, parameter, stepDescriptionSupplier, null);
	}

	/**
	 * Executes a given task in parallel with a specified number of progress steps.
	 * This method is intended for tasks that require exception handling.
	 * @param <T> The type of the parameter that the task function accepts.
	 * @param taskFunction The function representing the task to be executed.
	 *                     It accepts a MonitoredTaskParameter and a parameter of type T.
	 * @param parameter The parameter to be passed to the task function.
	 * @param stepDescriptionSupplier The parameter provide what should be done for step descriptions
	 * @param exceptionHandler The consumer handle the exceptions is happening in a job
	 */
	public <T> void run(final BiConsumer<MonitoredTaskParameter, T> taskFunction,
			final T parameter,
			final Function<MonitoredTaskParameter, String> stepDescriptionSupplier,
			final @Nullable Consumer<Throwable> exceptionHandler
			) {
		progressMonitor.checkCanceled();
		final CompletableFuture<Void> voidCompletableFuture = CompletableFuture.runAsync(() ->{
			progressMonitor.checkCanceled();
			final int threadNumber = threadNum.getAndIncrement();
			LOG.debug(() -> String.format("The task number %s", threadNumber));
			final var monitoredTaskParameter = new MonitoredTaskParameter(managerId, progressMonitor, threadNumber);
			updateStepDescription(monitoredTaskParameter, stepDescriptionSupplier);
			taskFunction.accept(monitoredTaskParameter, parameter);
			workedCount.incrementAndGet();
			progressMonitor.worked(1);
			updateStepDescription(monitoredTaskParameter, stepDescriptionSupplier);
		}, executorService).exceptionally(exception -> {
			if (exceptionHandler != null) {
				exceptionHandler.accept(exception);
			}
			return null;
		});
		futures.add(voidCompletableFuture);
	}

	private void updateStepDescription(
			final MonitoredTaskParameter monitoredTaskParameter,
			final Function<MonitoredTaskParameter, String> stepDescriptionSupplier) {
		progressMonitor.setStepDescription(stepDescriptionSupplier.apply(monitoredTaskParameter));
	}

	/**
	 * the method is joining all futures
	 */
	public void join() {
		progressMonitor.checkCanceled();
		CompletableFuture.allOf(futures.toArray(CompletableFuture[]::new)).join();
		LOG.debug(() -> String.format("The number of completed tasks %s", futures.size()));
	}

	/**
	 *  The method is responsible for creating subtask manager
	 * @param numberOfSubTaskProgressSteps the number of all progress steps of subtasks
	 * @return a new MonitoredTaskManager
	 */
	public MonitoredTaskManager createSubTaskManager(final int numberOfSubTaskProgressSteps) {
		final MonitoredTaskManager subManager = new MonitoredTaskManager(progressMonitor, executorService, numberOfSubTaskProgressSteps);
		subManagers.add(subManager);
		return subManager;
	}

	/**
	 * The method returns all finished steps
	 * @return The number of finished steps
	 */
	public int getNumberOfWorkedSteps() {
		return workedCount.get();
	}
	/**
	 * The method returns all progress steps
	 * @return The number of progress steps
	 */
	public int getNumberOfProgressSteps() {
		return numberOfProgressSteps;
	}

	/**
	 * The method returns the list of subtask managers
	 * @return list of MonitoredTaskManager
	 */
	public List<MonitoredTaskManager> getSubTaskManagers() {
		return subManagers;
	}

	/**
	 * The method returns the progressMonitor
	 * @return ProgressMonitor
	 */
	public ProgressMonitor getProgressMonitor() {
		return progressMonitor;
	}

	/**
	 * The method returns all progress steps for subtasks
	 * @return the number of all progress steps of subtasks
	 */
	public int getNumberOfProgressStepsFromSubTasks() {
		int count = 0;
		for (MonitoredTaskManager subManager : subManagers) {
			count += subManager.getNumberOfProgressSteps();
		}
		return count;
	}
	/**
	 * The method returns all finished steps for subtasks
	 * @return the number of finished steps of subtasks
	 */
	public int getNumberOfWorkedStepsFromSubTasks() {
		int count = 0;
		for (MonitoredTaskManager subManager : subManagers) {
			count += subManager.getNumberOfWorkedSteps();
		}
		return count;
	}
	/**
	 * The method returns the id of monitor task manager
	 * @return monitor task manager Id
	 */
	public UUID getManagerId() {
		return managerId;
	}
}
