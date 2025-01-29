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
 * Represents the target {@link Candidate}.
 */
@XmlJavaTypeAdapter(Target.TargetAdapter.class)
public class Target extends Candidate {

	@JsonCreator
	public Target(@JsonProperty("name") @Nullable final String name,
				  @JsonProperty("type") @Nullable final String type,
				  @JsonProperty("path") @Nullable final String path,
				  @JsonProperty("pathPattern") @Nullable final String pathPattern,
				  @JsonProperty("parent") @Nullable final ContainedIn containedIn) {
		super(name, type, path, pathPattern, containedIn);
	}

	/**
	 * Adapter class for {@link Target} for XML serialization.
	 */
	public static class TargetAdapter extends XmlAdapter<TargetAdapted, Target> {

		@Nullable
		@Override
		public Target unmarshal(@Nullable final TargetAdapted targetAdapted) {
			if (targetAdapted == null) {
				return null;
			}

			return new Target(targetAdapted.name, targetAdapted.type, targetAdapted.path, targetAdapted.pattern, targetAdapted.containedIn);
		}

		@Nullable
		@Override
		public TargetAdapted marshal(@Nullable final Target target){
			if (target == null) {
				return null;
			}

			final TargetAdapted result = new TargetAdapted();
			result.name = target.name;
			result.type = target.type;
			result.path = target.path;
			result.pattern = target.pathPattern;
			result.containedIn = (ContainedIn) target.containedIn;
			return result;
		}

	}

	@XmlRootElement(name = "target")
	static class TargetAdapted extends CandidateAdapted {

		@XmlElement(name = "contained-in")
		@Nullable
		private ContainedIn containedIn;
	}
}
