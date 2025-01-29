/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.apache.commons.lang.builder.ToStringBuilder;
import org.apache.commons.lang.builder.ToStringStyle;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonAutoDetect.Visibility;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.springdata.annotations.Embedded;

/**
 * Model class for a module location entity.
 */
/* Do not use the getters to avoid iw AssertionExceptions if one of the members is null */
@JsonAutoDetect(getterVisibility=Visibility.NONE, fieldVisibility=Visibility.ANY)
@Embedded(name = "ModuleLocation")
@MiningDataType(name = "ModuleLocation")
public class ModuleLocation implements Serializable {

	@Nullable
	private Integer offset;
	@Nullable
	private Integer length;
	
	/**
	 * Creates a new instance.
	 */
	public ModuleLocation() {}
	
	/**
	 * Creates a new instance.
	 * 
	 * @param offset the location offset 
	 * @param length the location length
	 */
	public ModuleLocation(final int offset, final int length) {
		this(Integer.valueOf(offset), Integer.valueOf(length));
	}
	
	/**
	 * Creates a new instance.
	 * 
	 * @param offset the location offset 
	 * @param length the location length
	 */
	public ModuleLocation(@Nullable final Integer offset, @Nullable final Integer length) {
		this.offset = offset;
		this.length = length;
	}
	
	/**
	 * Gets the offset.
	 *
	 * @return the offset
	 */
	public Integer getOffset() {
		return Assert.assertNotNull(offset, "Offset must not be null.");
	}

	/**
	 * Sets the offset.
	 *
	 * @param offset the offset
	 */
	public void setOffset(final Integer offset) {
		this.offset = offset;
	}
	
	/**
	 * Gets the length.
	 *
	 * @return the length
	 */
	public Integer getLength() {
		return Assert.assertNotNull(length, "Length must not be null.");
	}
	
	/**
	 * Sets the length.
	 *
	 * @param length the length
	 */
	public void setLength(final Integer length) {
		this.length = length;
	}
	
	/**
	 * Checks if the module location overlaps with the given module location.
	 *
	 * @param otherLocation the module location to compare to
	 * @return {@code true} if both module locations overlap
	 */
	public boolean overlapsWith(final ModuleLocation otherLocation) {
		final int firstStart = getOffset();
		final int firstLength = getLength();
		final int firstEnd = firstStart + firstLength;
		final int secondStart = otherLocation.getOffset();
		final int secondLength = otherLocation.getLength();
		final int secondEnd = secondStart + secondLength;
		return firstStart <= secondEnd && secondStart <= firstEnd;
	}
	
	/**
	 * Checks if the module location is at the same position or within the given module location.
	 *
	 * @param otherLocation the module location to compare to
	 * @return {@code true} if the module location is located at the same position or within the given module location
	 */
	public boolean isWithin(final ModuleLocation otherLocation) {
		final int firstStart = getOffset();
		final int firstLength = getLength();
		final int firstEnd = firstStart + firstLength;
		final int secondStart = otherLocation.getOffset();
		final int secondLength = otherLocation.getLength();
		final int secondEnd = secondStart + secondLength;
		return firstStart >= secondStart && firstEnd <= secondEnd;
	}

	/**
	 * Checks whether another location given by {@code offset} and {@code length} is at the same position or within this {@link ModuleLocation}.
	 *
	 * @param offset The offset to compare to
	 * @param length The length to compare to
	 * @return {@code true} if it is located at the same position or within this location. Otherwise {@code false}
	 */
	public boolean contains(final int offset, final int length) {
		final int moduleOffset = getOffset();
		final int moduleLength = getLength();

		return ! (moduleOffset > offset || moduleOffset + moduleLength < offset + length);
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.SHORT_PREFIX_STYLE)
				.append("offset", offset)
				.append("length", length)
				.toString();
	}

	@Override
	public int hashCode() {
		return Objects.hash(length, offset);
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		final ModuleLocation other = (ModuleLocation) obj;
		return Objects.equals(length, other.length) && Objects.equals(offset, other.offset);
	}

	/**
	 * This method will merge all overlapping ModuleLocations while keeping track of the amount of locations that have been merged
	 * @param locations The locations to merge
	 * @return A list of the MergedModuleLocations
	 */
	public static List<MergedModuleLocation> merge(final List<ModuleLocation> locations) {
		final List<MergedModuleLocation> resultList = new ArrayList<>();

		for (int i = 0; i < locations.size();) {
			int skippedLocations = 1; /* Default is 1 since we need to keep iterating even if no merge was performed */
			int mergedLocations = 1;

			final ModuleLocation currentLocation = locations.get(i);
			final int currentOffset = currentLocation.getOffset();
			int currentEndOffset = currentLocation.getOffset() + currentLocation.getLength();

			/* Iterating through the next locations to compare offsets + length*/
			for (int j = i + 1; j < locations.size(); j++) {
				final ModuleLocation currentLocationToLookAt = locations.get(j);

				if ( ! currentLocation.overlapsWith(currentLocationToLookAt) ) {
					/* Location doesn't overlap with next location -> location can be kept as-is*/
					break;
				} else {
					/* Location overlaps with next location -> we need to keep going and save the current highest end offset */
					currentEndOffset = Math.max(currentEndOffset, currentLocationToLookAt.getOffset() + currentLocationToLookAt.getLength());
					skippedLocations++;
					mergedLocations++;
				}
			}
			resultList.add(new MergedModuleLocation(currentOffset, currentEndOffset, mergedLocations));
			i += skippedLocations;
		}
		return resultList;
	}

	public static class MergedModuleLocation {
		private final Integer offset;
		private final Integer endOffset;
		private final Integer mergedLocations;

		/**
		 * Constructor for MergedModuleLocation
		 * @param offset The start offset
		 * @param endOffset The end offset
		 * @param mergedLocations The amount of locations that have been merged into one
		 */
		public MergedModuleLocation(final Integer offset, final Integer endOffset, final Integer mergedLocations) {
			this.offset = offset;
			this.endOffset = endOffset;
			this.mergedLocations = mergedLocations;
		}

		/**
		 * Returns the start offset
		 * @return the start offset
		 */
		public Integer getOffset() {
			return offset;
		}

		/**
		 * Returns the end offset
		 * @return the end offset
		 */
		public Integer getEndOffset() {
			return endOffset;
		}

		/**
		 * Returns the number of locations that have been merged into one
		 * @return the number of locations that have been merged into one
		 */
		public Integer getMergedLocations() {
			return mergedLocations;
		}
	}
}
