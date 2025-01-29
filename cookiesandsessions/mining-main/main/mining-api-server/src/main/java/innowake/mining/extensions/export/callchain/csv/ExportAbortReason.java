/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.export.callchain.csv;

/**
 * The reason for the export being aborted.
 */
public enum ExportAbortReason {
	JOB_CANCELLED,
	EXPORT_LIMIT_REACHED
}
