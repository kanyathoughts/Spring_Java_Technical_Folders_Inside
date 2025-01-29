/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core;

import java.io.Externalizable;

import com.orientechnologies.orient.core.record.impl.ODocument;

/**
 * Result object for the candidate identification functions.
 */
public class CandidateIdentificationResult extends ODocument {
	
	/**
	 * Only provided because {@link ODocument} is {@link Externalizable}.
	 */
	public CandidateIdentificationResult() {}
	
	/**
	 * Constructs a new candidate identification result.
	 * 
	 * @param candidatesIdentified the number of identified candidates
	 * @param candidatesStored the number of stored candidates, this can differ due to already existing candidates, which not be restored for example
	 */
	public CandidateIdentificationResult(final Long candidatesIdentified, final Long candidatesStored) {
		setProperty("candidatesIdentified", candidatesIdentified);
		setProperty("candidatesStored", candidatesStored);
	}
}