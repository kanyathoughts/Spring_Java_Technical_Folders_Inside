/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.reference;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ServiceProvider;

/**
 * Provides access to the reference services.
 */
public class ReferenceServiceProvider extends ServiceProvider<ReferenceServiceProvider> {

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	public ReferenceServiceProvider(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Access to {@link FindAllReferences}.
	 *
	 * @return the service instance
	 */
	public FindAllReferences findAllReferences() {
		return new FindAllReferences(connectionInfo);
	}
	
	/**
	 * Access to {@link FindAllReferencesForModule}.
	 *
	 * @return the service instance
	 */
	public FindAllReferencesForModule findAllReferencesForModule() {
		return new FindAllReferencesForModule(connectionInfo);
	}
	
	/**
	 * Access to {@link FindReferenceById}.
	 *
	 * @return the service instance
	 */
	public FindReferenceById findReferenceById() {
		return new FindReferenceById(connectionInfo);
	}
	
	/**
	 * Access to {@link CreateReference}.
	 *
	 * @return the service instance
	 */
	public CreateReference createReference() {
		return new CreateReference(connectionInfo);
	}
	
	/**
	 * Access to {@link DeleteReference}.
	 *
	 * @return the service instance
	 */
	public DeleteReference deleteReference() {
		return new DeleteReference(connectionInfo);
	}
}
