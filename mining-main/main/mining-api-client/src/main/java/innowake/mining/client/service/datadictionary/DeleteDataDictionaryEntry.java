/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.datadictionary;

import java.io.IOException;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ModuleIdService;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.shared.access.EntityId;


/**
 * HTTP REST service for deleting a data dictionary entry.
 */
public class DeleteDataDictionaryEntry extends ModuleIdService<DeleteDataDictionaryEntry, Void> {

	public static final String ENDPOINT = RouteConfiguration.API_BASE + "/v1/projects/%s/modules/%s/data-dictionary/%s";
	
	@Nullable
	private EntityId dataDictionaryEntryId;

	DeleteDataDictionaryEntry(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Sets the dataDictionaryEntryId.
	 *
	 * @param dataDictionaryEntryId the dataDictionaryEntryId to set
	 * @return {@code this}
	 */
	public DeleteDataDictionaryEntry setDataDictionaryEntryId(final EntityId dataDictionaryEntryId) {
		this.dataDictionaryEntryId = dataDictionaryEntryId;
		return this;
	}

	/**
	 * Deletes a data dictionary entry by sending a HTTP DELETE request to the specified {@value #ENDPOINT}.
	 * <p>
	 * Returns the following status code:
	 * <li><strong>204</strong>: regardless if the data dictionary entry exists or not 
	 * 
	 * @return a result holding only the status code of the response
	 */
	@Override
	public Result<Void> execute() throws IOException {
		validate();
		setServiceUrl(String.format(ENDPOINT, encode(projectId), encode(moduleId), encode(dataDictionaryEntryId)));
		return execute(httpDelete());
	}
	
	@Override
	protected void validate() {
		super.validate();
		if (dataDictionaryEntryId == null) {
			throw new IllegalStateException("Data dictionary entry ID must be set.");
		}
	}

}
