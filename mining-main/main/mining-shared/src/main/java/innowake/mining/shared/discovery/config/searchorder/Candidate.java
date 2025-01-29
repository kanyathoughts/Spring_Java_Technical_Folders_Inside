/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.shared.discovery.config.searchorder;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.filter.AntWildcardFilter;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.datapoints.annotations.MiningDataType;

import javax.xml.bind.annotation.XmlAttribute;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.IOException;
import java.io.Serializable;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/**
 * Base class for "Candidates" that are defined by the {@code discovery-search-order.xml} file.
 * <p>
 * A Candidate is either a {@link Source}, {@link Target} or {@link ContainedIn} definition, represented by
 * a {@code <source>}, {@code <target>} or {@code <contained-in>} tag in the XML, respectively.
 */
@MiningDataType(name = MiningEnitityNames.SEARCH_ORDER_CANDIDATE)
public abstract class Candidate implements Serializable {

	@Nullable
	protected final String name;
	@Nullable
	protected final String type;
	@Nullable
	protected final String path;
	@Nullable
	protected final String pathPattern;
	@Nullable
	@JsonDeserialize(using=ContainedInDeserializer.class) /* must be instanceof ContainedIn for successful XML serialization */
	protected final Candidate containedIn;

	protected Candidate(@Nullable final String name, @Nullable final String type,
			@Nullable final String path, @Nullable final String pathPattern,
			@Nullable final Candidate containedIn) {
		this.name = name;
		this.type = type;
		this.path = path;
		this.pathPattern = pathPattern;
		this.containedIn = containedIn;
	}

	/**
	 * Gets the name that a dependency candidate must match, if present.
	 *
	 * @return the expected name of the dependency candidate
	 */
	public Optional<String> getName() {
		return Optional.ofNullable(name);
	}

	/**
	 * Gets the type that a dependency candidate must match, if present.
	 *
	 * @return the expected type of the dependency candidate
	 */
	public Optional<String> getType() {
		return Optional.ofNullable(type);
	}

	/**
	 * Gets the path that a dependency candidate must match exactly, if present.
	 *
	 * @return the expected path of the dependency candidate
	 */
	public Optional<String> getPath() {
		return Optional.ofNullable(path);
	}

	/**
	 * Gets the pattern that a dependency candidate's path must match, if present.
	 * <p>
	 * The pattern is expressed in <a href="http://ant.apache.org/manual/dirtasks.html#patterns">"Ant Pattern"</a> syntax.
	 *
	 * @return the expected type of the dependency candidate
	 * @see AntWildcardFilter
	 */
	public Optional<String> getPathPattern() {
		return Optional.ofNullable(pathPattern);
	}

	/**
	 * Gets the parent module that a dependency candidate must have, if present.
	 * @return the expected parent, expressed as another {@code Candidate}
	 */
	public Optional<Candidate> getContainedIn() {
		return Optional.ofNullable(containedIn);
	}

	/**
	 * Adapter class used for XML serialization.
	 */
	protected static class CandidateAdapted {
		@XmlAttribute(name = "name")
		@Nullable
		protected String name;

		@XmlAttribute(name = "type")
		@Nullable
		protected String type;

		@XmlAttribute(name = "path")
		@Nullable
		protected String path;

		@XmlAttribute(name = "pattern")
		@Nullable
		protected String pattern;
	}

	/**
	 * Custom JSON deserializer, ensuring that "containedIn" is always instanceof {@link ContainedIn} when read from JSON.
	 */
	protected static class ContainedInDeserializer extends JsonDeserializer<ContainedIn> {

		@Nullable
		@Override
		@SuppressWarnings("unchecked")
		public ContainedIn deserialize(@Nullable final JsonParser p, @Nullable final DeserializationContext ctxt) throws IOException {
			final Map<String, Object> map = assertNotNull(ctxt).readValue(p, Map.class);
			return mapToObject(map);
		}

		@Nullable
		@SuppressWarnings("unchecked")
		private ContainedIn mapToObject(@Nullable final Map<String, Object> map) {
			if (map == null) {
				return null;
			}
			return new ContainedIn(
					(String) map.get("name"),
					(String) map.get("type"),
					(String) map.get("path"),
					(String) map.get("pathPattern"),
					mapToObject((Map<String, Object>) map.get("containedIn")));
		}
	}

	@Override
    public boolean equals(@Nullable final Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        final Candidate candidate = (Candidate) o;
        return Objects.equals(name, candidate.name) &&
                Objects.equals(type, candidate.type) &&
                Objects.equals(path, candidate.path) &&
                Objects.equals(pathPattern, candidate.pathPattern) &&
                Objects.equals(containedIn, candidate.containedIn);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, type, path, pathPattern, containedIn);
    }
}
