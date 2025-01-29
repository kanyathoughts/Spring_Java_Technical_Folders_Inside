/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.internal;

/**
 * Logging categories for lib-job.
 */
public class Logging {

	private static final String BASE = "innowake.lib.job.";

	private static final String API = BASE + "api.";
	public static final String JOB_API = API + "job";
	public static final String TASK_API = API + "task";
	public static final String TASK_PROCESSOR = API + "task-processor";
	
	public static final String EXECUTOR_SERVICE = BASE + "executor-service";
	public static final String JOB_MANAGER = BASE + "job-manager";
	public static final String JOB_MONITOR = BASE + "job-monitor";
	public static final String CLUSTER_INFO = BASE + "cluster-info";
	public static final String JOB_HEARTBEAT = BASE + "job-heartbeat";
	public static final String JOB_LOG = BASE + "job-log";
	public static final String AWS_LIFECYCLE_HANDLER = BASE + "aws-lifecycle-handler";
	public static final String PERSISTENCE = BASE + "persistence";
	
	private Logging() {}
	
}
