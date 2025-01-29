/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.datadictionary;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ServiceProvider;

/**
 * Provides access to the data dictionary services.
 */
public class DataDictionaryServiceProvider extends ServiceProvider<DataDictionaryServiceProvider> {

	/**
	 * Creates a new instance.
	 * 
	 * @param connectionInfo the connection info to use
	 */
	public DataDictionaryServiceProvider(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Access to {@link FindAllDataDictionaryEntries}.
	 *
	 * @return the service instance
	 */
	public FindAllDataDictionaryEntries findAllDataDictionaryEntries() {
		return new FindAllDataDictionaryEntries(connectionInfo);
	}

	/**
	 * Access to {@link FindDataDictionaryById}.
	 *
	 * @return the service instance
	 */
	public FindDataDictionaryById findDataDictionaryEntryById() {
		return new FindDataDictionaryById(connectionInfo);
	}

	/**
	 * Access to {@link CreateDataDictionaryEntry}.
	 *
	 * @return the service instance
	 */
	public CreateDataDictionaryEntry createDataDictionaryEntry() {
		return new CreateDataDictionaryEntry(connectionInfo);
	}

	/**
	 * Access to {@link UpdateDataDictionaryEntry}.
	 *
	 * @return the service instance
	 */
	public UpdateDataDictionaryEntry updateDataDictionaryEntry() {
		return new UpdateDataDictionaryEntry(connectionInfo);
	}

	/**
	 * Access to {@link DeleteDataDictionaryEntry}.
	 *
	 * @return the service instance
	 */
	public DeleteDataDictionaryEntry deleteDataDictionaryEntry() {
		return new DeleteDataDictionaryEntry(connectionInfo);
	}

	/**
	 * Access to {@link SearchDataDictionaryEntry}.
	 *
	 * @return the service instance
	 */
	public SearchDataDictionaryEntry searchDataDictionaryEntry() {
		return new SearchDataDictionaryEntry(connectionInfo);
	}
	
	/**
	 * Access to {@link FindAllDataDictionaryOtherScopes}.
	 *
	 * @return the service instance
	 */
	public FindAllDataDictionaryOtherScopes findAllDataDictionaryOtherScopes() {
		return new FindAllDataDictionaryOtherScopes(connectionInfo);
	}

	/**
	 * Access to {@link GetFormatIfSelectionIsValidBasedOnOffset}
	 *
	 * @return the service instance
	 */
	public GetFormatIfSelectionIsValidBasedOnOffset getFormatIfSelectionIsValidBasedOnOffset() {
		return new GetFormatIfSelectionIsValidBasedOnOffset(connectionInfo);
	}
}
