/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.taxonomy;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ServiceProvider;

/**
 * Provides access to taxonomy services.
 */
public class TaxonomyServiceProvider extends ServiceProvider<TaxonomyServiceProvider> {

	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 */
	public TaxonomyServiceProvider(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Access to {@link CreateTaxonomy}.
	 *
	 * @return the service instance
	 */
	public CreateTaxonomy createTaxonomy() {
		return new CreateTaxonomy(connectionInfo);
	}
	
	/**
	 * Access to {@link DeleteTaxonomy}.
	 *
	 * @return the service instance
	 */
	public DeleteTaxonomy deleteTaxonomy() {
		return new DeleteTaxonomy(connectionInfo);
	}
	
	/**
	 * Access to {@link UpdateTaxonomy}.
	 *
	 * @return the service instance
	 */
	public UpdateTaxonomy updateTaxonomy() {
		return new UpdateTaxonomy(connectionInfo);
	}
	
	/**
	 * Access to {@link FindAllTaxonomies}.
	 *
	 * @return the service instance
	 */
	public FindAllTaxonomies findAllTaxonomies() {
		return new FindAllTaxonomies(connectionInfo);
	}
	
	/**
	 * Access to {@link FindAllTaxonomies}.
	 *
	 * @return the service instance
	 */
	public FindTaxonomyById findTaxonomyById() {
		return new FindTaxonomyById(connectionInfo);
	}
	
	/**
	 * Access to {@link GetTaxonomyReport}.
	 *
	 * @return the service instance
	 */
	public GetTaxonomyReport getTaxonomyReport() {
		return new GetTaxonomyReport(connectionInfo);
	}
	
	/**
	 * Access to {@link IdentifyTechnicalTaxonomies}.
	 *
	 * @return the service instance
	 */
	public IdentifyTechnicalTaxonomies identifyTechnicalTaxonomies() {
		return new IdentifyTechnicalTaxonomies(connectionInfo);
	}
	
	/**
	 * Access to {@link GetTaxonomyAssignments}.
	 *
	 * @return the service instance
	 */
	public GetTaxonomyAssignments getTaxonomyAssignments() {
		return new GetTaxonomyAssignments(connectionInfo);
	}

	/**
	 * Access to {@link TaxonomyAggregations}.
	 *
	 * @return the service instance
	 */
	public TaxonomyAggregations getTaxonomyAggregations() {
		return new TaxonomyAggregations(connectionInfo);
	}
	
	/**
	 * Access to {@link UpdateTaxonomyAssignments}.
	 *
	 * @return the service instance
	 */
	public UpdateTaxonomyAssignments updateTaxonomyAssignments() {
		return new UpdateTaxonomyAssignments(connectionInfo);
	}
}
