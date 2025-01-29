/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.codeviewer;

import java.util.Objects;

import innowake.mining.shared.access.EntityId;
import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.model.ModuleLocation;

/**
 * A link for the code viewer which links a location in the source code to another location (possibly in a different Module).
 */
public class CodeViewerLink {

    private final LinkType linkType;
    private final LinkTargetType linkTargetType;
    private final String relationshipLabel;
    private final String targetLabel;

    private final EntityId fromModuleId;
    private final ModuleLocation fromModuleLocation;
    private final CodeViewerRange fromRange;
    @Nullable
    private final EntityId toModuleId;
    @Nullable
    private final ModuleLocation toModuleLocation;
    @Nullable
    private final CodeViewerRange toRange;

    @JsonCreator
    public CodeViewerLink(@JsonProperty("linkType") final LinkType linkType,
                          @JsonProperty("linkTargetType") final LinkTargetType linkTargetType,
                          @JsonProperty("relationshipLabel") final String relationshipLabel,
                          @JsonProperty("targetLabel") final String targetLabel,
                          @JsonProperty("fromModuleId") final EntityId fromModuleId,
                          @JsonProperty("fromModuleLocation") final ModuleLocation fromModuleLocation,
                          @JsonProperty("fromRange") final CodeViewerRange fromRange,
                          @JsonProperty("toModuleId") @Nullable final EntityId toModuleId,
                          @JsonProperty("toModuleLocation") @Nullable final ModuleLocation toModuleLocation,
                          @JsonProperty("toRange") @Nullable final CodeViewerRange toRange) {
        this.linkType = linkType;
        this.linkTargetType = linkTargetType;
        this.relationshipLabel = relationshipLabel;
        this.targetLabel = targetLabel;
        this.fromModuleId = fromModuleId;
        this.fromModuleLocation = fromModuleLocation;
        this.fromRange = fromRange;
        this.toModuleId = toModuleId;
        this.toModuleLocation = toModuleLocation;
        this.toRange = toRange;
    }

    /**
     * Returns the type of the link. This can be used to give different link types a different visual representation.
     * @return the link type
     */
    public LinkType getLinkType() {
        return linkType;
    }

    /**
     * Returns whether the target of the link is in the same file or a different file.
     * @return the link target
     */
    public LinkTargetType getLinkTargetType() {
        return linkTargetType;
    }

    /**
     * Returns a short label that describes the relationship of the current code location with the target (e.g. "definedBy", "calls", "includes").
     * @return the relationship label
     */
    public String getRelationshipLabel() {
        return relationshipLabel;
    }

    /**
     * Returns a short label for the target of the link, e.g. the name of the referenced variable or the name of the called Module.
     * @return the target label
     */
    public String getTargetLabel() {
        return targetLabel;
    }

    /**
     * Returns the id of the module that contains the link.
     * @return the id of the module that contains the link
     */
    public EntityId getFromModuleId() {
        return fromModuleId;
    }

    /**
     * Returns the location in the source code where the link is placed.
     * @return the location of the link in the source code
     */
    public ModuleLocation getFromModuleLocation() {
        return fromModuleLocation;
    }

    /**
     * Returns the location in the source code where the link is placed expressed in line numbers and columns.
     * @return the code viewer range occupied by the link
     */
    public CodeViewerRange getFromRange() {
        return fromRange;
    }

    /**
     * Returns the id of the module where the link target is located, or {@code null} if the target is within the same module as the link.
     * @return the id of the module containing the link target
     */
    @Nullable
    public EntityId getToModuleId() {
        return toModuleId;
    }

    /**
     * Returns the location of the link target in the source code, or {@code null} if the target location is unknown.
     * @return the location of the link target
     */
    @Nullable
    public ModuleLocation getToModuleLocation() {
        return toModuleLocation;
    }

    /**
     * Returns the location of the link target in the source code expressed in line numbers and columns, or {@code null} if the target location is unknown.
     * @return the code viewer range of the link target
     */
    @Nullable
    public CodeViewerRange getToRange() {
        return toRange;
    }

	@Override
	public int hashCode() {
		return Objects.hash(fromModuleId, fromModuleLocation, fromRange, linkTargetType, linkType, relationshipLabel, targetLabel, toModuleId, toModuleLocation,
				toRange);
	}

	/* We omit toModuleLocation and toRange intentionally, since those values differ for assembled vs unassembled */
	@Override
	public boolean equals(final Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		final CodeViewerLink other = (CodeViewerLink) obj;
		return Objects.equals(fromModuleId, other.fromModuleId) && Objects.equals(fromModuleLocation, other.fromModuleLocation)
				&& Objects.equals(fromRange, other.fromRange) && linkTargetType == other.linkTargetType && linkType == other.linkType
				&& Objects.equals(relationshipLabel, other.relationshipLabel) && Objects.equals(targetLabel, other.targetLabel)
				&& Objects.equals(toModuleId, other.toModuleId);
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.SHORT_PREFIX_STYLE)
				.append("linkType", linkType)
				.append("linkTargetType", linkTargetType)
				.append("relationshipLabel", relationshipLabel)
				.append("targetLabel", targetLabel)
				.append("fromModuleId", fromModuleId)
				.append("fromModuleLocation", fromModuleLocation)
				.append("fromRange", fromRange)
				.append("toModuleId", toModuleId)
				.append("toModuleLocation", toModuleLocation) 
				.append("toRange", toRange)
			.toString();
	}
}
