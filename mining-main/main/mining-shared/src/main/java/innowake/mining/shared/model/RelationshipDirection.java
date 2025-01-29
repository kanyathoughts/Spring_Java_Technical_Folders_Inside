/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

import java.util.Collection;
import java.util.Optional;
import java.util.stream.Collectors;

import innowake.lib.core.api.lang.Nullable;

/**
 * Used to determine the direction of a Module Relationship.
 */
public enum RelationshipDirection {
		IN, OUT, BOTH;
		
		/**
		 * Resolves a Collection of directions to a distinct value.
		 * @param c Collection of directions.
		 * @return Distinct direction.
		 */
		public static Optional<RelationshipDirection> in(@Nullable final Collection<RelationshipDirection> c) {
			if (c != null) {
				if (c.contains(RelationshipDirection.BOTH) || (c.contains(RelationshipDirection.IN) && c.contains(RelationshipDirection.OUT))) {
					return Optional.of(RelationshipDirection.BOTH);
				} else if (c.contains(RelationshipDirection.IN)) {
					return Optional.of(RelationshipDirection.IN);
				} else if (c.contains(RelationshipDirection.OUT)) {
					return Optional.of(RelationshipDirection.OUT);
				}
			}
			return Optional.empty();
		}
		
		/**
		 * Resolves a Collection of directions to a distinct value.
		 * @param c Collection of directions strings.
		 * @return Distinct direction.
		 */
		public static Optional<RelationshipDirection> inOf(@Nullable final Collection<String> c) {
			return in(c != null ? c.stream().map(RelationshipDirection::valueOf).collect(Collectors.toList()) : null);
		}
}
