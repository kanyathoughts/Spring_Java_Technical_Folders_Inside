/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.taxonomy.assignment;
 
import java.util.List;
import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.TaxonomyPojo;

/**
 * Response model to {@link TaxonomyAssignmentsGetRequest} including the {@link TaxonomyGetAssignment} and the number of {@link ModulePojo ModulePojos} in the selection.
 */
public class TaxonomyAssignmentsGetResponse {
	
	private Long moduleCount;
	
	private List<TaxonomyGetAssignment> taxonomies;
	
	/**
	 * Creates an instance of {@linkplain TaxonomyAssignmentsGetResponse}
	 * 
	 * @param moduleCount number of {@link ModulePojo ModulePojos}
	 * @param taxonomies {@linkplain List} of {@linkplain TaxonomyGetAssignment}s
	 */
	@JsonCreator
	public TaxonomyAssignmentsGetResponse(
			@JsonProperty(value = "moduleCount") final long moduleCount,
			@JsonProperty(value = "taxonomies") final List<TaxonomyGetAssignment> taxonomies) {
		this.moduleCount = Long.valueOf(moduleCount);
		this.taxonomies = taxonomies;
	}
	
	/**
	 * Get the number of {@link ModulePojo ModulePojos} in the selection
	 *
	 * @return the number of {@link ModulePojo ModulePojos}
	 */
	public Long getModuleCount() {
		return moduleCount;
	}

	/**
	 * Get the {@link List} of {@link TaxonomyGetAssignment} for the response
	 *
	 * @return {@link List} of {@link TaxonomyGetAssignment}
	 */
	public List<TaxonomyGetAssignment> getTaxonomies() {
		return taxonomies;
	}


	public static class TaxonomyGetAssignment {

		@Nullable
		private AssignmentState state;

		private final TaxonomyPojo taxonomy;

		/**
		 * Creates an instance of {@linkplain TaxonomyGetAssignment}
		 * @param taxonomy the Taxonomy to reference
		 * @param state it's state
		 */
		@JsonCreator
		public TaxonomyGetAssignment(@JsonProperty("taxonomy") final TaxonomyPojo taxonomy, @JsonProperty("state") final AssignmentState state) {
			this.taxonomy = taxonomy;
			this.state = state;
		}

		/**
		 * Get {@linkplain AssignmentState} for referenced Taxonomy
		 *
		 * @return the {@linkplain AssignmentState}
		 */
		@Nullable
		public AssignmentState getState() {
			return state;
		}

		/**
		 * Set {@linkplain AssignmentState} for referenced Taxonomy
		 *
		 * @param state the {@linkplain AssignmentState}
		 */
		public void setState(final AssignmentState state) {
			this.state = state;
		}

		public TaxonomyPojo getTaxonomy() {
			return taxonomy;
		}

		@Override
		public int hashCode() {
			return Objects.hash(taxonomy, state);
		}

		@Override
		public boolean equals(@Nullable final Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			final TaxonomyGetAssignment other = (TaxonomyGetAssignment) obj;
			return Objects.equals(taxonomy, other.taxonomy) && state == other.state;
		}
	}
}
