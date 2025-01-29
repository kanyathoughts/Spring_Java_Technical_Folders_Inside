/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.metamodel;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.RestService;


/**
 * Base class for all metamodel REST services.
 * 
 *  @param <S> the type of the actual service
 *  @param <T> the type of the service result
 */
abstract class MetamodelService<S extends MetamodelService<S, T>, T> extends RestService<T> {
	
	@Nullable
	protected String className;

	protected MetamodelService(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Sets the name of the class for which the metadata should be retrieved.
	 *
	 * @param className the name of the class
	 * @return the service instance
	 */
	public S setClassName(final String className) {
		this.className = className;
		return getThis();
	}

	@Override
	protected void validate() {
		super.validate();
		if (className == null || (className != null && className.isEmpty())) {
			throw new IllegalStateException("Class name must be set");
		}
	}

	/**
	 * Returns this object for method chaining.
	 *
	 * @return this object
	 */
	@SuppressWarnings("unchecked")
	protected S getThis() {
		return (S) this;
	}

}
