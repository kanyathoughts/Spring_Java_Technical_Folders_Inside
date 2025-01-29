/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.client;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.client.service.taxonomy.CreateTaxonomy;
import innowake.mining.client.service.taxonomy.DeleteTaxonomy;
import innowake.mining.client.service.taxonomy.FindAllTaxonomies;
import innowake.mining.client.service.taxonomy.FindTaxonomyById;
import innowake.mining.client.service.taxonomy.GetTaxonomyAssignments;
import innowake.mining.client.service.taxonomy.GetTaxonomyReport;
import innowake.mining.client.service.taxonomy.IdentifyTechnicalTaxonomies;
import innowake.mining.client.service.taxonomy.TaxonomyAggregations;
import innowake.mining.client.service.taxonomy.UpdateTaxonomy;
import innowake.mining.client.service.taxonomy.UpdateTaxonomyAssignments;
import innowake.mining.plugin.preferences.ProjectData;

/**
 * Provides taxonomy services with project id already set. 
 */
public class TaxonomyServiceProvider extends innowake.mining.client.service.taxonomy.TaxonomyServiceProvider {

	private final ProjectData projectData;
	
	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 * @param projectData the {@link ProjectData} to set the project ID into the provides services
	 */
	public TaxonomyServiceProvider(final ConnectionInfo connectionInfo, final ProjectData projectData) {
		super(connectionInfo);
		this.projectData = projectData;
	}
	
	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public CreateTaxonomy createTaxonomy() {
		return init(super.createTaxonomy());
	}
	
	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public DeleteTaxonomy deleteTaxonomy() {
		return init(super.deleteTaxonomy());
	}
	
	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public FindAllTaxonomies findAllTaxonomies() {
		return init(super.findAllTaxonomies());
	}
	
	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public UpdateTaxonomy updateTaxonomy() {
		return init(super.updateTaxonomy());
	}
	
	@Override
	public GetTaxonomyAssignments getTaxonomyAssignments() {
		return init(super.getTaxonomyAssignments());
	}
	
	@Override
	public TaxonomyAggregations getTaxonomyAggregations() {
		return init(super.getTaxonomyAggregations());
	}
	
	@Override
	public FindTaxonomyById findTaxonomyById() {
		return init(super.findTaxonomyById());
	}
	
	@Override
	public GetTaxonomyReport getTaxonomyReport() {
		return init(super.getTaxonomyReport());
	}
	
	@Override
	public UpdateTaxonomyAssignments updateTaxonomyAssignments() {
		return init(super.updateTaxonomyAssignments());
	}
	
	/**
	 * {@inheritDoc}
	 * <br>
	 * Project id is already set. 
	 */
	@Override
	public IdentifyTechnicalTaxonomies identifyTechnicalTaxonomies() {
		return init(super.identifyTechnicalTaxonomies());
	}

	@SuppressWarnings("unchecked")
	private <T extends ProjectIdService<?, ?>> T init(final ProjectIdService<?, ?> service) {
		return (T) service.setProjectId(projectData.getProjectId());
	}
}
