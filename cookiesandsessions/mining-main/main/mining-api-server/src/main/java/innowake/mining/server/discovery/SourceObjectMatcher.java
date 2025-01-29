/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery;

import java.util.Arrays;
import java.util.Collections;
import java.util.Set;

import com.google.common.collect.Sets;

import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * A Source object matcher class.
 */
public class SourceObjectMatcher {

	private final Technology technology;
	private final Set<Type> types;

	/**
	 * Constructs a new {@link SourceObjectMatcher} object with the given {@link Technology} and {@link Type}s.
	 *
	 * @param technology the {@link Technology}
	 * @param types the {@link Type}s
	 */
	public SourceObjectMatcher(final Technology technology, final Type... types) {
		this.technology = technology;
		this.types = types.length == 0 || types[0] == null ? Collections.emptySet() : Sets.immutableEnumSet(Arrays.asList(types));
	}

	/**
	 * Returns the {@link Technology } to match.
	 *
	 * @return the Technology to match.
	 */
	public Technology getTechnology() {
		return technology;
	}

	/**
	 * Returns the {@link Type}s to match.
	 *
	 * @return the {@link Type}s to match.
	 */
	public Set<Type> getTypes() {
		return types;
	}

}
