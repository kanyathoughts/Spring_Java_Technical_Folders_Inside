/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.taxonomy.assignment;
 
import java.io.Serializable;
import java.util.List;
import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;

/**
 * Request model to assign Taxonomies to a selection of {@linkplain ModulePojo ModulePojos}.
 */
public class TaxonomyAssignmentsSetRequest implements Serializable {
	
	private ModuleMatcher modules;
	
	private List<TaxonomySetAssignment> taxonomies;
	
	/**
	 * Creates a new instance of {@linkplain TaxonomyAssignmentsSetRequest}
	 * 
	 * @param modules the {@linkplain ModuleMatcher} instance
	 * @param taxonomies the {@linkplain List} of {@linkplain TaxonomySetAssignment}s
	 */
	@JsonCreator
	public TaxonomyAssignmentsSetRequest(
			@JsonProperty(value = "modules") final ModuleMatcher modules,
			@JsonProperty(value = "taxonomies") final List<TaxonomySetAssignment> taxonomies) {
		this.modules = modules;
		this.taxonomies = taxonomies;
	}
	
	/**
	 * Get the {@linkplain ModuleMatcher} instance
	 *
	 * @return {@linkplain ModuleMatcher} instance
	 */
	public ModuleMatcher getModules() {
		return modules;
	}
	
	/**
	 * Get the {@linkplain List} of {@linkplain TaxonomySetAssignment}
	 *
	 * @return {@linkplain List} of {@linkplain TaxonomySetAssignment}
	 */
	public List<TaxonomySetAssignment> getTaxonomies() {
		return taxonomies;
	}

	public static class TaxonomySetAssignment implements Serializable {

		@Nullable
		private AssignmentState state;

		private final EntityId taxonomyId;

		/**
		 * Creates an instance of {@linkplain TaxonomySetAssignment}
		 * @param taxonomyId the id of the Taxonomy to reference
		 * @param state it's state
		 */
		@JsonCreator
		public TaxonomySetAssignment(@JsonProperty("taxonomyId") final EntityId taxonomyId, @JsonProperty("state") final AssignmentState state) {
			this.taxonomyId = taxonomyId;
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

		public EntityId getTaxonomyId() {
			return taxonomyId;
		}

		@Override
		public int hashCode() {
			return Objects.hash(taxonomyId, state);
		}

		@Override
		public boolean equals(@Nullable final Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			final TaxonomySetAssignment other = (TaxonomySetAssignment) obj;
			return Objects.equals(taxonomyId, other.taxonomyId) && state == other.state;
		}
	}
}
