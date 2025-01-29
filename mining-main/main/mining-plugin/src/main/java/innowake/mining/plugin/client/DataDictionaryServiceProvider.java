/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.client;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.datadictionary.CreateDataDictionaryEntry;
import innowake.mining.client.service.datadictionary.DeleteDataDictionaryEntry;
import innowake.mining.client.service.datadictionary.FindAllDataDictionaryEntries;
import innowake.mining.client.service.datadictionary.FindAllDataDictionaryOtherScopes;
import innowake.mining.client.service.datadictionary.SearchDataDictionaryEntry;
import innowake.mining.client.service.datadictionary.UpdateDataDictionaryEntry;
import innowake.mining.plugin.preferences.ProjectData;

/**
 * Provides data dictionary services with project id already set. 
 */
public class DataDictionaryServiceProvider extends innowake.mining.client.service.datadictionary.DataDictionaryServiceProvider {

	private final ProjectData projectData;

	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 * @param projectData the {@link ProjectData} to set the project ID into the provides services
	 */
	public DataDictionaryServiceProvider(final ConnectionInfo connectionInfo, final ProjectData projectData) {
		super(connectionInfo);
		this.projectData = projectData;
	}
	
	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public CreateDataDictionaryEntry createDataDictionaryEntry() {
		return init(super.createDataDictionaryEntry());
	}
	
	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public FindAllDataDictionaryEntries findAllDataDictionaryEntries() {
		return init(super.findAllDataDictionaryEntries());
	}
	
	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public DeleteDataDictionaryEntry deleteDataDictionaryEntry() {
		return init(super.deleteDataDictionaryEntry());
	}
	
	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public SearchDataDictionaryEntry searchDataDictionaryEntry() {
		return init(super.searchDataDictionaryEntry());
	}
	
	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public UpdateDataDictionaryEntry updateDataDictionaryEntry() {
		return init(super.updateDataDictionaryEntry());
	}
	
	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public FindAllDataDictionaryOtherScopes findAllDataDictionaryOtherScopes() {
		return init(super.findAllDataDictionaryOtherScopes());
	}

	@SuppressWarnings("unchecked")
	private <T extends ProjectIdService<?, ?>> T init(final ProjectIdService<?, ?> service) {
		return (T) service.setProjectId(projectData.getProjectId());
	}

}
