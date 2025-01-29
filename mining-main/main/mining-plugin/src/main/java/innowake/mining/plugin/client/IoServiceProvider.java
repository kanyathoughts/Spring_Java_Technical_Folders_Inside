/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.client;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.io.ExportCsv;
import innowake.mining.client.service.io.ExportEffortSummaryExcel;
import innowake.mining.client.service.io.ExportExcel;
import innowake.mining.client.service.io.ExportSourceObjects;
import innowake.mining.client.service.io.ImportCSV;
import innowake.mining.client.service.io.ImportExcel;
import innowake.mining.client.service.io.ImportSourceObjects;
import innowake.mining.plugin.preferences.ProjectData;

/**
 * Provides access to {@link innowake.mining.client.service.io.IoServiceProvider} with project id already set.
 */
public class IoServiceProvider extends innowake.mining.client.service.io.IoServiceProvider {

	@Nullable private final ProjectData projectData;

	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 * @param projectData the {@link ProjectData} to set the project ID into the provides services
	 */
	IoServiceProvider(final ConnectionInfo connectionInfo, final ProjectData projectData) {
		super(connectionInfo);
		this.projectData = projectData;
	}

	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 * @param projectData the {@link ProjectData} to set the project ID into the provides services
	 */
	IoServiceProvider(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
		projectData = null;
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set.
	 */
	@Override
	public ImportCSV importCSV() {
		return init(super.importCSV());
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set.
	 */
	@Override
	public ImportExcel importExcel() {
		return init(super.importExcel());
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set.
	 */
	@Override
	public ExportExcel exportExcel() {
		return init(super.exportExcel());
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set.
	 */
	@Override
	public ExportCsv exportCsv() {
		return init(super.exportCsv());
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set.
	 */
	@Override
	public ImportSourceObjects importSourceObjects() {
		return init(super.importSourceObjects());
	}

	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set.
	 */
	@Override
	public ExportSourceObjects exportSourceObjects() {
		return init(super.exportSourceObjects());
	}

	/**
	 * Access to {@link ExportEffortSummaryExcel}.
	 * <br>
	 * Project id is already set.
	 */
	@Override
	public ExportEffortSummaryExcel exportEffortSummaryExcel() {
		return init(super.exportEffortSummaryExcel());
	}

	@SuppressWarnings("unchecked")
	private <T extends ProjectIdService<?, ?>> T init(final ProjectIdService<?, ?> service) {
		if (projectData != null) {
			return (T) service.setProjectId(projectData.getProjectId());
		}
		return (T) service;
	}
}
