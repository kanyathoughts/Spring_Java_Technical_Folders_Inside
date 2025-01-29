/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.entities.functionalblocks;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition.ScalarType;

import java.util.Map;
import java.util.Objects;
import java.util.UUID;

/**
 * A link between to child-blocks of a functional block.
 */
@MiningDataType(name = "FunctionalBlockLink")
public class FunctionalBlockLink {

	private final UUID uid;
	private final UUID parent;
	private final UUID childA;
	private final UUID childB;
	@Nullable
	private final String conditionLabel;
	@MiningDataPoint(scalarType = ScalarType.JSON)
	private final Map<String, Object> flags;
	@Nullable
	private final FunctionalBlockLinkCondition condition;

	@JsonCreator
	public FunctionalBlockLink(@JsonProperty("uid") final UUID uid,
							   @JsonProperty("parent") final UUID parent,
							   @JsonProperty("childA") final UUID childA,
							   @JsonProperty("childB") final UUID childB,
							   @JsonProperty("conditionLabel") @Nullable final String conditionLabel,
							   @JsonProperty("flags") final Map<String, Object> flags,
							   @JsonProperty("condition") @Nullable final FunctionalBlockLinkCondition condition) {
		this.uid = uid;
		this.parent = parent;
		this.childA = childA;
		this.childB = childB;
		this.conditionLabel = conditionLabel;
		this.flags = flags;
		this.condition = condition;
	}

	/**
	 * Returns the unique id of the link.
	 * @return unique id of the link
	 */
	public UUID getUid() {
		return uid;
	}
	
	/**
	 * Returns the unique id of the link.
	 * @return unique id of the link
	 */
	public UUID getParent() {
		return parent;
	}
	
	/**
	 * Returns the id of the first child block (the link connects {@linkplain #getChildA() childA} and {@linkplain #getChildB() childB}).
	 * <p>
	 * For directional links, this is the outgoing side, i.e. the block from which the link is outgoing.
	 * @return the id of the first child block
	 */
	public UUID getChildA() {
		return childA;
	}

	/**
	 * Returns the id of the second child block (the link connects {@linkplain #getChildA() childA} and {@linkplain #getChildB() childB}).
	 * <p>
	 * 	For directional links, this is the incoming side, i.e. the block into which the link is incoming.
	 * @return the id of the second child block
	 */
	public UUID getChildB() {
		return childB;
	}

	/**
	 * Returns the label that the edge should have with respect to its condition (aka "branch label").
	 * @return the condition label or {@code null}
	 */
	@Nullable
	public String getConditionLabel() {
		return conditionLabel;
	}

	/**
	 * Returns the flags set for this link. Flags are key-value pairs meant for internal use (i.e. not user-definable) to record
	 * additional data about a link.
	 * @return the flags of the link
	 */
	public Map<String, Object> getFlags() {
		return flags;
	}

	/**
	 * Returns the condition that is associated with the link, or {@code null} if this link has no condition.
	 * @return the condition (unique id and label) associated with the link or {@code null}
	 */
	@Nullable
	public FunctionalBlockLinkCondition getCondition() {
		return condition;
	}

	@Override
	public String toString() {
		return "FunctionalBlockLink{" +
				"uid=" + uid +
				", parent=" + parent +
				", childA=" + childA +
				", childB=" + childB +
				", conditionLabel='" + conditionLabel + '\'' +
				", flags=" + flags +
				'}';
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) return true;
		if (!(o instanceof FunctionalBlockLink)) return false;
		final FunctionalBlockLink that = (FunctionalBlockLink) o;
		return Objects.equals(getUid(), that.getUid()) && Objects.equals(getParent(),
				that.getParent()) && Objects.equals(getChildA(),
				that.getChildA()) && Objects.equals(getChildB(),
				that.getChildB()) && Objects.equals(getCondition(),
				that.getCondition()) && Objects.equals(getConditionLabel(),
				that.getConditionLabel()) && Objects.equals(getFlags(),
				that.getFlags());
	}

	@Override
	public int hashCode() {
		return Objects.hash(getUid(), getParent(), getChildA(), getChildB(), getCondition(), getConditionLabel(), getFlags());
	}
}
