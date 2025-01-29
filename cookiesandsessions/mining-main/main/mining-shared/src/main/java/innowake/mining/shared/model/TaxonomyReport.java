package innowake.mining.shared.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.TaxonomyPojo;

/**
 * Model class for a taxonomy report.
 */
public class TaxonomyReport {
	private final ModulePojo module;
	private final List<TaxonomyPojo> taxonomies;
	
	/**
	 * Creates a new TaxonomyReport.
	 * 
	 * @param module the {@link Module}.
	 * @param taxonomies list {@link TaxonomyPojo}.
	 */
	@JsonCreator
	public TaxonomyReport(@JsonProperty("module") final ModulePojo module, @JsonProperty("taxonomies") final List<TaxonomyPojo> taxonomies) {
		this.module = module;
		this.taxonomies = taxonomies;
	}

	/**
	 * @return the module of the report
	 */
	public ModulePojo getModule() {
		return module;
	}

	/**
	 * @return a list of taxonomies associated with the module    
	 */
	public List<TaxonomyPojo> getTaxonomies() {
		return taxonomies;
	}
}
