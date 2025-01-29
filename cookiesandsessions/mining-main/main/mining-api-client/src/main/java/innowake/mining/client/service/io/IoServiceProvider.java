/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.io;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ServiceProvider;

/**
 * Provides access to IO services.
 */
public class IoServiceProvider extends ServiceProvider<IoServiceProvider> {

	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 */
	public IoServiceProvider(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Access to {@link ImportCSV}.
	 *
	 * @return the service instance
	 */
	public ImportCSV importCSV() {
		return new ImportCSV(connectionInfo);
	}

	/**
	 * Access to {@link ImportExcel}.
	 *
	 * @return the service instance
	 */
	public ImportExcel importExcel() {
		return new ImportExcel(connectionInfo);
	}

	/**
	 * Access to {@link ExportExcel}.
	 *
	 * @return the service instance
	 */
	public ExportExcel exportExcel() {
		return new ExportExcel(connectionInfo);
	}


	/**
	 * Access to {@link ExportCsv}.
	 *
	 * @return the service instance
	 */
	public ExportCsv exportCsv() {
		return new ExportCsv(connectionInfo);
	}

	/**
	 * Access to {@link ImportSourceObjects}.
	 *
	 * @return the service instance
	 */
	public ImportSourceObjects importSourceObjects() {
		return new ImportSourceObjects(connectionInfo);
	}

	/**
	 * Access to {@link ExportSourceObjects}.
	 *
	 * @return the service instance
	 */
	public ExportSourceObjects exportSourceObjects() {
		return new ExportSourceObjects(connectionInfo);
	}

	/**
	 * Access to {@link ExportEffortSummaryExcel}.
	 *
	 * @return the service instance
	 */
	public ExportEffortSummaryExcel exportEffortSummaryExcel() {
		return new ExportEffortSummaryExcel(connectionInfo);
	}
	
	/**
	 * Access to {@link ExportToFormat}.
	 *
	 * @return the service instance
	 */
	public ExportToFormat exportToFormat() {
		return new ExportToFormat(connectionInfo);
	}
	
	/**
	 * Access to {@link GetExportFormats}.
	 *
	 * @return the service instance
	 */
	public GetExportFormats getExportFormats() {
		return new GetExportFormats(connectionInfo);
	}
}
