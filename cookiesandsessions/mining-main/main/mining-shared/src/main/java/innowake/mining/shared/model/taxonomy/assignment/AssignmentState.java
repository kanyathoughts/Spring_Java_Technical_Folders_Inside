package innowake.mining.shared.model.taxonomy.assignment;


import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.TaxonomyPojo;

/**
 * An enum of the different assignment states for a {@linkplain TaxonomyPojo} in a selection of {@linkplain ModulePojo ModulePojos}
 */
public enum AssignmentState {
	/**
	 *
	 * All {@link ModulePojo ModulePojos} in the selection have this {@linkplain TaxonomyPojo} assignment.
	 */
	ALL,
	/**
	 * Some {@link ModulePojo ModulePojos} in the selection have this {@linkplain TaxonomyPojo} assignment.
	 */
	SOME,
	/**
	 * No {@link ModulePojo ModulePojos} in the selection have this {@linkplain TaxonomyPojo} assignment.
	 */
	NONE;
}