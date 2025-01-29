/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.entities.functionalblocks;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.UUID;

/**
 * A condition that may be attached to one or more {@link FunctionalBlockLink}.
 */
public class FunctionalBlockLinkCondition {

	private final UUID uid;

	private final String label;

	@JsonCreator
	public FunctionalBlockLinkCondition(@JsonProperty("uid") final UUID uid, @JsonProperty("label") final String label) {
		this.uid = uid;
		this.label = label;
	}

	/**
	 * Returns the label of the condition.
	 * @return the condition label
	 */
	public String getLabel() {
		return label;
	}

	/**
	 * @return the id of the condition.
	 */
	public UUID getUid() {
		return uid;
	}
}
