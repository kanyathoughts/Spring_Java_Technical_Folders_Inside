/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.export.callchain.csv;

/**
 * This exception gets thrown when the CSV export gets aborted.
 *
 * See {@link ExportAbortReason} for the reason.
 */
public class CsvExportAbortException extends RuntimeException {

	private final ExportAbortReason reason;

	public CsvExportAbortException(final ExportAbortReason reason) {
		this.reason = reason;
	}

	public ExportAbortReason getReason() {
		return reason;
	}
}
