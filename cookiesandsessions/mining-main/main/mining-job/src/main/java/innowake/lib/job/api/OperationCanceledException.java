/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.lib.job.api;

/**
 * Thrown by {@link ProgressMonitor#checkCanceled()} if a job was canceled.
 */
public class OperationCanceledException extends RuntimeException {
	private static final long serialVersionUID = 1L;
}
