/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service.datadictionary;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.service.ModuleIdService;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.DataDictionaryPojoPrototype;


/**
 * Base class for data dictionary entry services, which directly make use of
 * the data dictionary entry.
 * 
 * @param <S> the concrete entry type
 */
public abstract class DataDictionaryEntryService<S extends DataDictionaryEntryService<S>> extends ModuleIdService<S, DataDictionaryPojo> {

	@Nullable 
	protected DataDictionaryPojoPrototype dataDictionaryEntry;

	DataDictionaryEntryService(final ConnectionInfo connectionInfo) {
		super(connectionInfo);
	}

	/**
	 * Sets the data dictionary entry to update.
	 *
	 * @param dataDictionaryEntry the data dictionary entry
	 * @return {@code this}
	 */
	public S setDataDictionaryEntry(final DataDictionaryPojoPrototype dataDictionaryEntry) {
		this.dataDictionaryEntry = dataDictionaryEntry;
		return getThis();
	}
	
	@Override
	protected void validate() {
		super.validate();
		if (dataDictionaryEntry == null) {
			throw new IllegalStateException("Data dictionary entry must be set.");
		}
	}
}
