/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

/**
 * Enumerate the Field Name available for job infos
 */
public enum JobInfoFieldName implements FieldName {

	/** Field for the job id */
	ID,
	/** Field for the job name */
	NAME,
	/** Field for the job description */
	DESCRIPTION,
	/** Field for the step description */
	STEP_DESCRIPTION,
	/** Field for the job status */
	STATUS,
	/** Field for the number of pending tasks */
	PENDING_TASKS,
	/** Field for the number of total work units (total number of tasks) */
	TOTAL_WORK_UNITS,
	/** Field for the number of processed work units (processed number of tasks) */
	PROCESSED_WORK_UNITS,
	/** Field for the submit time */
	SUBMIT_TIME,
	/** Field for the scheduled start time */
	SCHEDULED_START_TIME,
	/** Field for the start time */
	START_TIME,
	/** Field for the finish time */
	FINISH_TIME,
	/** Field for the ID of the user that created the job */
	CREATED_BY_USER_ID;
}
