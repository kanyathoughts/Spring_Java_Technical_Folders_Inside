/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.discovery.config.searchorder;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.datapoints.annotations.MiningDataPointIgnore;
import innowake.mining.shared.datapoints.annotations.MiningDataType;

/**
 * Model class for the SearchOrder entity.
 * 
 * The SearchOrder is used for the assembling of source objects in order to locate source objects that depends on (e.g. copybooks).
 * It is also used for dependency resolving. In case there are multiple possible candidates to satisfy a dependency, the search order defines
 * which one should take priority.
 * It maps from one source location to several locations that shall be searched for dependencies. 
 * Both the source and the target can be described using several properties such as name, type and path pattern.
 * 
 * Example:
 * <pre>
 * <source pattern="/foo/bar/*.cbl"/>
 * <target pattern="/foo/baz/*.cpy"/>
 * <target pattern="./*.cpy"/>
 * </pre>
 * 
 * Tells the assembler to look for files that match the pattern "/foo/baz/*.cpy" when trying to assemble an object that matches the pattern "/foo/bar/*.cbl". 
 * If not found, tries pattern "./*.cpy"
 * (i.e. it will first look for a copybook in the folder "/foo/baz" and then in the current folder relative to the source object).
 */
@XmlJavaTypeAdapter(SearchOrder.SearchOrderAdapter.class)
@MiningDataType(name = MiningEnitityNames.SEARCH_ORDER)
public class SearchOrder implements Serializable {

	private final Source source;
	private final List<Target> targets;

	/**
	 * Convenience method for creating a search order using only {@linkplain Candidate#pathPattern path patterns} for source and targets.
	 * @param sourcePattern the path pattern of the source
	 * @param targetPatterns the path patterns of the targets
	 * @return the created SearchOrder
	 */
	public static SearchOrder fromPatterns(final String sourcePattern, final String... targetPatterns) {
		return fromPatterns(sourcePattern, Arrays.asList(targetPatterns));
	}

	/**
	 * Convenience method for creating a search order using only {@linkplain Candidate#pathPattern path patterns} for source and targets.
	 * @param sourcePattern the path pattern of the source
	 * @param targetPatterns the path patterns of the targets
	 * @return the created SearchOrder
	 */
	public static SearchOrder fromPatterns(final String sourcePattern, final List<String> targetPatterns) {
		return new SearchOrder(new Source(null, null, null, sourcePattern, null),
				targetPatterns.stream().map(pattern -> new Target(null, null, null, pattern, null)).collect(Collectors.toList()));
	}

	/**
	 * Creates a new instance of SearchOrder with default values.
	 */
	public SearchOrder() {
		source = new Source(null, null, null, "**/*", null);
		targets = Arrays.asList(
				new Target(null, null, null, "./*", null),
				new Target(null, null, null, "**/*", null));
	}

	/**
	 * Creates a new instance of SearchOrder.
	 * 
	 * @param source represents the source candidate
	 * @param targets represents the target candidates
	 */
	@JsonCreator
	public SearchOrder(@JsonProperty("source") final Source source, @JsonProperty("targets") final List<Target> targets) {
		this.source = source;
		this.targets = targets;
	}

	public Source getSource() {
		return source;
	}

	public List<Target> getTargets() {
		return targets;
	}

	/**
	 * Gets the Source pattern. This method is used when assembling source files, where only the {@linkplain Candidate#pathPattern path pattern}
	 * of the source can be taken into account.
	 *
	 * @return the path pattern of the source
	 */
	@JsonIgnore
	@MiningDataPointIgnore
	public Optional<String> getSourcePattern() {
		return source.getPathPattern();
	}

	/**
	 * Gets the list of Target patterns. This method is used when assembling source files, where only the {@linkplain Candidate#pathPattern path pattern}
	 * of the targets can be taken into account.
	 *
	 * @return the path patterns of the targets
	 */
	@JsonIgnore
	@MiningDataPointIgnore
	public List<String> getTargetPatterns() {
		return targets.stream().map(Target::getPathPattern).filter(Optional::isPresent).map(Optional::get).collect(Collectors.toList());
	}
	
	/**
	 * XML adapter class for serializing {@link SearchOrder}.
	 */
	public static class SearchOrderAdapter extends XmlAdapter<SearchOrderAdapted, SearchOrder> {

		@Nullable
		@Override
		public SearchOrderAdapted marshal(@Nullable final SearchOrder searchOrder) throws Exception {
			if (searchOrder == null) {
				return null;
			}

			final SearchOrderAdapted result = new SearchOrderAdapted();
			result.source = searchOrder.source;
			result.targets = searchOrder.targets;
			return result;
		}

		@Nullable
		@Override
		public SearchOrder unmarshal(@Nullable final SearchOrderAdapted searchOrderAdapted) throws Exception {
			if (searchOrderAdapted == null) {
				return null;
			}

			final Source source;
			if (searchOrderAdapted.source == null) {
				source = new Source(null, null, null, null, null);
			} else {
				source = assertNotNull(searchOrderAdapted.source);
			}
			final List<Target> targets;
			if (searchOrderAdapted.targets == null) {
				targets = Collections.emptyList();
			} else {
				targets = assertNotNull(searchOrderAdapted.targets);
			}

			return new SearchOrder(source, targets);
		}

	}
	
	@XmlRootElement(name = "search-order")
	private static class SearchOrderAdapted {

		@XmlElement(name = "source")
		@Nullable
		private Source source;

		@XmlElement(name = "target")
		@Nullable
		private List<Target> targets;
	}

	@Override
    public boolean equals(@Nullable final Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        final SearchOrder that = (SearchOrder) o;
        return Objects.equals(source, that.source) && Objects.equals(targets, that.targets);
    }

    @Override
    public int hashCode() {
        return Objects.hash(source, targets);
    }
}
