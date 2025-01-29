/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import java.io.Serializable;
import java.util.Objects;
import java.util.Optional;

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.custom.annotation.JsonIgnoreUnknownProperties;
import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.annotations.Usage;
import innowake.mining.shared.datapoints.annotations.UsageAttribute;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.TableAttributes;

/**
 * Model class for the locations of {@code ast_node} entities.
 */
@JsonIgnoreUnknownProperties
public class AstNodeLocation implements Serializable {

	@MiningDataPoint(displayName = "Retraced Length", description = "The length of the Retraced location")
	@Usage(value = Usages.MINING_UI_MODULE_ERROR_MARKER_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Location"),
			@UsageAttribute(key = TableAttributes.HIDDEN_BY_DEFAULT, value = "true"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "11")
	})
	@Nullable
	private final Integer retracedLength;
	@MiningDataPoint(displayName = "Retraced Offset", description = "The offset of the Retraced location")
	@Usage(value = Usages.MINING_UI_MODULE_ERROR_MARKER_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Location"),
			@UsageAttribute(key = TableAttributes.HIDDEN_BY_DEFAULT, value = "true"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "10")
	})
	@Nullable
	private final Integer retracedOffset;

	@MiningDataPoint(displayName = "Assembled Offset", description = "The offset of the Assembled location")
	@Usage(value = Usages.MINING_UI_MODULE_ERROR_MARKER_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Location"),
			@UsageAttribute(key = TableAttributes.HIDDEN_BY_DEFAULT, value = "true"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "4")
	})
	@Nullable
	private final Integer assembledOffset;

	@MiningDataPoint(displayName = "Assembled Length", description = "The length of the Assembled location")
	@Usage(value = Usages.MINING_UI_MODULE_ERROR_MARKER_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Location"),
			@UsageAttribute(key = TableAttributes.HIDDEN_BY_DEFAULT, value = "true"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "5")
	})
	@Nullable
	private final Integer assembledLength;

	@MiningDataPoint(displayName = "Root Relative Offset", description = "The offset of the Root Relative location")
	@Usage(value = Usages.MINING_UI_MODULE_ERROR_MARKER_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Location"),
			@UsageAttribute(key = TableAttributes.HIDDEN_BY_DEFAULT, value = "true"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "6")
	})
	@Nullable
	private final Integer rootRelativeOffset;

	@MiningDataPoint(displayName = "Root Relative Length", description = "The length of the Root Relative location")
	@Usage(value = Usages.MINING_UI_MODULE_ERROR_MARKER_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Location"),
			@UsageAttribute(key = TableAttributes.HIDDEN_BY_DEFAULT, value = "true"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "7")
	})
	@Nullable
	private final Integer rootRelativeLength;

	@MiningDataPoint(displayName = "Root Relative Start Line Number", description = "The start line number of the Root Relative location")
	@Usage(value = Usages.MINING_UI_MODULE_ERROR_MARKER_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Location"),
			@UsageAttribute(key = TableAttributes.HIDDEN_BY_DEFAULT, value = "true"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "8")
	})
	@Nullable
	private final Integer rootRelativeStartLineNumber;

	@MiningDataPoint(displayName = "Root Relative End Line Number", description = "The end line number of the Root Relative location")
	@Usage(value = Usages.MINING_UI_MODULE_ERROR_MARKER_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Location"),
			@UsageAttribute(key = TableAttributes.HIDDEN_BY_DEFAULT, value = "true"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "9")
	})
	@Nullable
	private final Integer rootRelativeEndLineNumber;
	
	/**
	 * Constructor.
	 * 
	 * @param retracedOffset the retracedOffset
	 * @param retracedLength the retracedLength
	 * @param assembledOffset the assembledOffset
	 * @param assembledLength the assembledLength
	 * @param rootRelativeOffset the rootRelativeOffset
	 * @param rootRelativeLength the rootRelativeLength
	 * @param rootRelativeStartLineNumber the rootRelativeStartLineNumber
	 * @param rootRelativeEndLineNumber the rootRelativeEndLineNumber
	 */
	@JsonCreator
	public AstNodeLocation(
				@JsonProperty("retracedOffset") @JsonAlias("retraced_offset") @Nullable final Integer retracedOffset,
				@JsonProperty("retracedLength") @JsonAlias("retraced_length") @Nullable final Integer retracedLength,
				@JsonProperty("assembledOffset") @JsonAlias("assembled_offset") @Nullable final Integer assembledOffset,
				@JsonProperty("assembledLength") @JsonAlias("assembled_length") @Nullable final Integer assembledLength,
				@JsonProperty("rootRelativeOffset") @JsonAlias("root_relative_offset") @Nullable final Integer rootRelativeOffset,
				@JsonProperty("rootRelativeLength") @JsonAlias("root_relative_length") @Nullable final Integer rootRelativeLength,
				@JsonProperty("rootRelativeStartLineNumber") @JsonAlias("root_relative_start_line") @Nullable final Integer rootRelativeStartLineNumber,
				@JsonProperty("rootRelativeEndLineNumber") @JsonAlias("root_relative_end_line") @Nullable final Integer rootRelativeEndLineNumber) {
		this.retracedOffset = retracedOffset;
		this.retracedLength = retracedLength;
		this.assembledOffset = assembledOffset;
		this.assembledLength = assembledLength;
		this.rootRelativeOffset = rootRelativeOffset;
		this.rootRelativeLength = rootRelativeLength;
		this.rootRelativeStartLineNumber = rootRelativeStartLineNumber;
		this.rootRelativeEndLineNumber = rootRelativeEndLineNumber;
	}
	
	public AstNodeLocation(@Nullable final Integer retracedOffset, @Nullable final Integer retracedLength) {
		this(retracedOffset, retracedLength, null, null, null, null, null, null);
	}

	/**
	 * Constructor with default values
	 */
	public AstNodeLocation() {
		this(null, null, null, null, null, null, null, null);
	}
	
	/**
	 * Returns the retracedOffset.
	 *
	 * @return the retracedOffset
	 */
	public Optional<Integer> getRetracedOffset() {
		return Optional.ofNullable(retracedOffset);
	}

	/**
	 * Returns the retracedLength.
	 *
	 * @return the retracedLength
	 */
	public Optional<Integer> getRetracedLength() {
		return Optional.ofNullable(retracedLength);
	}
	
	/**
	 * Returns the assembledOffset.
	 *
	 * @return the assembledOffset
	 */
	public Optional<Integer> getAssembledOffset() {
		return Optional.ofNullable(assembledOffset);
	}

	/**
	 * Returns the assembledLength.
	 *
	 * @return the assembledLength
	 */
	public Optional<Integer> getAssembledLength() {
		return Optional.ofNullable(assembledLength);
	}
	
	/**
	 * Returns the rootRelativeOffset.
	 *
	 * @return the rootRelativeOffset
	 */
	public Optional<Integer> getRootRelativeOffset() {
		return Optional.ofNullable(rootRelativeOffset);
	}
	
	/**
	 * Returns the rootRelativeLength.
	 *
	 * @return the rootRelativeLength
	 */
	public Optional<Integer> getRootRelativeLength() {
		return Optional.ofNullable(rootRelativeLength);
	}
	
	/**
	 * Returns the rootRelativeStartLineNumber.
	 *
	 * @return the rootRelativeStartLineNumber
	 */
	public Optional<Integer> getRootRelativeStartLineNumber() {
		return Optional.ofNullable(rootRelativeStartLineNumber);
	}

	/**
	 * Returns the rootRelativeEndLineNumber.
	 *
	 * @return the rootRelativeEndLineNumber
	 */
	public Optional<Integer> getRootRelativeEndLineNumber() {
		return Optional.ofNullable(rootRelativeEndLineNumber);
	}

	/**
	 * Retuns a new instance of {@code ModuleLocation} with the retraced offset and length of this {@link AstNodeLocation}.
	 *
	 * @return the converted {@linkplain ModuleLocation}
	 */
	public ModuleLocation convertToSharedModuleLocation() {
		return new ModuleLocation(Objects.requireNonNull(retracedOffset, "Retraced offset must not be null"),
								  Objects.requireNonNull(retracedLength, "Retraced length must not be null"));
	}

	/**
	 * Returns a new instance of {@code AstNodeLocation} with the offset and length of the given {@linkplain ModuleLocation}.
	 * This will create an {@linkplain AstNodeLocation} with the same offset and length for all three offsets because the {@linkplain ModuleLocation} does not
	 * have the information related to other offsets.
	 *
	 * @param moduleLocation the {@linkplain ModuleLocation} to convert
	 * @return the converted {@linkplain AstNodeLocation}
	 */
	public static AstNodeLocation fromModuleLocation(final ModuleLocation moduleLocation) {
		return new AstNodeLocation(moduleLocation.getOffset(), moduleLocation.getLength(), moduleLocation.getOffset(), moduleLocation.getLength(),
				moduleLocation.getOffset(), moduleLocation.getLength(), -1, -1);
	}

	/**
	 * Returns whether the location is inside an include component. On an unlikely event this would return false for an include component when both
	 * root relative location and retraced location are same.
	 *
	 * @return {@code true} if the location is inside an include component, {@code false} otherwise.
	 * If it cannot be determined then {@code Optional.empty()} is returned.
	 */
	public Optional<Boolean> isLocationInsideIncludeComponent() {
		if (rootRelativeOffset == null || rootRelativeLength == null || retracedOffset == null || retracedLength == null) {
			return Optional.empty();
		}

		return Optional.of(!(rootRelativeOffset.equals(retracedOffset) && rootRelativeLength.equals(retracedLength)));
	}

	@Override
	public String toString() {
		return new StringBuilder()
				.append("AstNodeLocation [retracedOffset=").append(retracedOffset)
				.append(", retracedLength=").append(retracedLength)
				.append(", assembledOffset=").append(assembledOffset)
				.append(", assembledLength=").append(assembledLength)
				.append(", rootRelativeOffset=").append(rootRelativeOffset)
				.append(", rootRelativeLength=").append(rootRelativeLength)
				.append(", rootRelativeStartLineNumber=").append(rootRelativeStartLineNumber)
				.append(", rootRelativeEndLineNumber=").append(rootRelativeEndLineNumber)
				.append("]")
				.toString();
	}
}
