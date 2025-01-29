/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.genai;

/**
 * the class is responsible for building step description for reachability block job
 */
public class GenerateAnnotationsStepDescriptionHolder {
	private static final String STEP_DESCRIPTION = "Generating annotation descriptions (%s/%s modules)";
	private static final String STEP_ANNOTATION_DESCRIPTION_WITH_MODULES = "Generating annotation descriptions (%s/%s annotations in %s/%s modules)";

	private final MonitoredTaskManager monitoredTaskManager;

	public GenerateAnnotationsStepDescriptionHolder(
			final MonitoredTaskManager monitoredTaskManager) {
		this.monitoredTaskManager = monitoredTaskManager;
	}

	/**
	 * The method create the step description
	 * @param monitoredTaskParameter is related to the Function<MonitoredTaskParameter, String> stepDescriptionSupplier
	 * @return the step description
	 */
	public String createStepDescription(final MonitoredTaskParameter monitoredTaskParameter) {
		if (monitoredTaskManager.getSubTaskManagers().isEmpty()) {
			return String.format(STEP_DESCRIPTION, monitoredTaskManager.getNumberOfWorkedSteps(), monitoredTaskManager.getNumberOfProgressSteps());
		} else {
			return String.format(STEP_ANNOTATION_DESCRIPTION_WITH_MODULES, monitoredTaskManager.getNumberOfWorkedStepsFromSubTasks(),
					monitoredTaskManager.getNumberOfProgressStepsFromSubTasks(),
					monitoredTaskManager.getNumberOfWorkedSteps(),
					monitoredTaskManager.getNumberOfProgressSteps());
		}
	}
}
