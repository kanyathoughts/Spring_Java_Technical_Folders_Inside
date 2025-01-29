/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.shared.discovery.config.searchorder;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import innowake.lib.core.api.lang.Nullable;

/**
 * Represents the source {@link Candidate}.
 */
@XmlJavaTypeAdapter(Source.SourceAdapter.class)
public class Source extends Candidate {

	@JsonCreator
	public Source(@JsonProperty("name") @Nullable final String name,
				  @JsonProperty("type") @Nullable final String type,
				  @JsonProperty("path") @Nullable final String path,
				  @JsonProperty("pathPattern") @Nullable final String pathPattern,
				  @JsonProperty("containedIn") @Nullable final ContainedIn containedIn) {
		super(name, type, path, pathPattern, containedIn);
	}

	/**
	 * Adapter class for {@link Source} for XML serialization.
	 */
	public static class SourceAdapter extends XmlAdapter<SourceAdapted, Source> {

		@Nullable
		@Override
		public Source unmarshal(@Nullable final SourceAdapted sourceAdapted) {
			if (sourceAdapted == null) {
				return null;
			}

			return new Source(sourceAdapted.name, sourceAdapted.type, sourceAdapted.path, sourceAdapted.pattern, sourceAdapted.containedIn);
		}

		@Nullable
		@Override
		public SourceAdapted marshal(@Nullable final Source src) {
			if (src == null) {
				return null;
			}

			final SourceAdapted result = new SourceAdapted();
			result.name = src.name;
			result.type = src.type;
			result.path = src.path;
			result.pattern = src.pathPattern;
			result.containedIn = (ContainedIn) src.containedIn;
			return result;
		}

	}

	@XmlRootElement(name = "source")
	static class SourceAdapted extends CandidateAdapted {

		@XmlElement(name = "contained-in")
		@Nullable
		private ContainedIn containedIn;
	}
}
