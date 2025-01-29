/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.client;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.taxonomytype.FindAllTaxonomyTypes;
import innowake.mining.plugin.preferences.ProjectData;

/**
 * Provides taxonomy type services with project id already set. 
 */
public class TaxonomyTypeServiceProvider extends innowake.mining.client.service.taxonomytype.TaxonomyTypeServiceProvider{
	
	private final ProjectData projectData;
	
	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 * @param projectData the {@link ProjectData} to set the project ID into the provides services
	 */
	public TaxonomyTypeServiceProvider(final ConnectionInfo connectionInfo, final ProjectData projectData) {
		super(connectionInfo);
		this.projectData = projectData;
	}
	
	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public FindAllTaxonomyTypes findAllTaxonomyTypes() {
		return init(super.findAllTaxonomyTypes());
	}

	@SuppressWarnings("unchecked")
	private <T extends ProjectIdService<?, ?>> T init(final ProjectIdService<?, ?> service) {
		return (T) service.setProjectId(projectData.getProjectId());
	}
}
