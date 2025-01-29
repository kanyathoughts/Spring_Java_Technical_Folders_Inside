/*  * Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.shared.discovery.config.searchorder;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import innowake.lib.core.api.lang.Nullable;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * Represents the parent module of a {@link Candidate}.
 */
@XmlJavaTypeAdapter(ContainedIn.ContainedInAdapter.class)
public class ContainedIn extends Candidate {

	@JsonCreator
	public ContainedIn(@JsonProperty("name") @Nullable final String name,
					   @JsonProperty("type") @Nullable final String type,
					   @JsonProperty("path") @Nullable final String path,
					   @JsonProperty("pathPattern") @Nullable final String pathPattern,
					   @JsonProperty("containedIn") @Nullable final ContainedIn containedIn) {
		super(name, type, path, pathPattern, containedIn);
	}

	/**
	 * Adapter class for {@link ContainedIn} for XML serialization.
	 */
	static class ContainedInAdapter extends XmlAdapter<ContainedInAdapted, ContainedIn> {

		@Nullable
		@Override
		public ContainedIn unmarshal(@Nullable final ContainedInAdapted containedInAdapted) {
			if (containedInAdapted == null) {
				return null;
			}

			return new ContainedIn(containedInAdapted.name, containedInAdapted.type, containedInAdapted.path, containedInAdapted.pattern, containedInAdapted.containedIn);
		}

		@Nullable
		@Override
		public ContainedInAdapted marshal(@Nullable final ContainedIn containedIn) {
			if (containedIn == null) {
				return null;
			}

			final ContainedInAdapted result = new ContainedInAdapted();
			result.name = containedIn.name;
			result.type = containedIn.type;
			result.path = containedIn.path;
			result.pattern = containedIn.pathPattern;
			result.containedIn = (ContainedIn) containedIn.containedIn;
			return result;
		}

	}

	@XmlRootElement(name = "contained-in")
	static class ContainedInAdapted extends CandidateAdapted {

		@Nullable
		@XmlElement(name = "contained-in")
		protected ContainedIn containedIn;
	}
}
