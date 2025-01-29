/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.job.genai;

/**
 * the class is responsible for building step description for functional block job
 */
public class GenerateFunctionalBlockStepDescriptionHolder {
	private static final String STEP_DESCRIPTION = "Generating functional block descriptions (%s/%s blocks)";
	private static final String STEP_DESCRIPTION_WITH_ANNOTATION = "Generating functional block descriptions (%s/%s annotations in %s/%s blocks)";

	private final MonitoredTaskManager monitoredTaskManager;

	public GenerateFunctionalBlockStepDescriptionHolder(final MonitoredTaskManager monitoredTaskManager) {
		this.monitoredTaskManager = monitoredTaskManager;
	}

	/**
	 * The method creates the step description
	 * @param monitoredTaskParameter is related to the Function<MonitoredTaskParameter, String> stepDescriptionSupplier
	 * @return the step description
	 */
	public String createStepDescription(final MonitoredTaskParameter monitoredTaskParameter) {
		if (monitoredTaskManager.getSubTaskManagers().isEmpty()) {
			return String.format(STEP_DESCRIPTION, monitoredTaskManager.getNumberOfWorkedSteps(), monitoredTaskManager.getNumberOfProgressSteps());
		} else {
			return String.format(STEP_DESCRIPTION_WITH_ANNOTATION, monitoredTaskManager.getNumberOfWorkedStepsFromSubTasks(),
					monitoredTaskManager.getNumberOfProgressStepsFromSubTasks(),
					monitoredTaskManager.getNumberOfWorkedSteps(),
					monitoredTaskManager.getNumberOfProgressSteps());
		}
	}
}
