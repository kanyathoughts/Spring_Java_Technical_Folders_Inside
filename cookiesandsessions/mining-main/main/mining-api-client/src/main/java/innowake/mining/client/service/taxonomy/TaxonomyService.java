/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.taxonomy;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ProjectIdService;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.entities.TaxonomyPojoPrototype;

/**
 * Base service for taxonomy endpoints holding a taxonomy entity.
 * @param <T> the specific implementation of the {@link TaxonomyService}
 */
public abstract class TaxonomyService<T extends TaxonomyService<T>> extends ProjectIdService<T, TaxonomyPojo> {

	@Nullable
	protected TaxonomyPojoPrototype taxonomy;


	/**
	 * Creates a new instance.
	 *
	 * @param connectionInfo the connection info to use
	 */
	TaxonomyService(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}
	
	/**
	 * Sets the taxonomy.
	 *
	 * @param taxonomy the taxonomy
	 * @return {@code this}
	 */
	public T setTaxonomy(final TaxonomyPojoPrototype taxonomy) {
		this.taxonomy = taxonomy;
		return getThis();
	}
	
	@Override
	protected void validate() {
		super.validate();
		if (taxonomy == null) {
			throw new IllegalStateException("Taxonomy must be set.");
		}
	}

}
