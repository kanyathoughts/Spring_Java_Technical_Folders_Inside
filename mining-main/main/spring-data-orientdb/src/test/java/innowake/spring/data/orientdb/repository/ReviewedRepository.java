/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.repository;

import innowake.spring.data.orientdb.integration.repository.domain.Customer;
import innowake.spring.data.orientdb.integration.repository.domain.Product;
import innowake.spring.data.orientdb.integration.repository.domain.Reviewed;

/**
 * Repository for edge entity reviewed.
 */
public interface ReviewedRepository extends OrientRepository<Reviewed> {
	

	/**
	 * Returns the edge entity linking respective vertices from {@link Customer} to {@link Product} by their id.
	 *
	 * @param outId the customer id
	 * @param inId the product id
	 * @return the edge linking the respective vertices
	 */
	Reviewed findByOutCustomerIdAndInProductId(final Long outId, final Long inId);
}
