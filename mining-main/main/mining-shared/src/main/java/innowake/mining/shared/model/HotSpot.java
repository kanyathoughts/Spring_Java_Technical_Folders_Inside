package innowake.mining.shared.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.lang.Assert;
import innowake.lib.core.lang.Assert.AssertionException;
import innowake.mining.shared.entities.ModulePojo;

/**
 * A Hotspot model used to hold {@link Module} with links/reference count.
 */
public class HotSpot {

	/**
	 * Enum for HotSpot type filter
	 */
	public enum FilterType {
		CALLS, REFERENCES, CANDIDATE_RULE, DATA_SETS, DATABASE_TABLES
	}
	
	private final ModulePojo module;
	private final Long count;

	/**
	 * Creates a new Hotspot.
	 * 
	 * @param module the {@link Module} used in this Hotspot
	 * @param count the count of references for the current module
	 */
	@JsonCreator
	public HotSpot(@JsonProperty("module") final ModulePojo module, @JsonProperty("count") final long count) {
		this.module = module;
		this.count = Long.valueOf(count);
	}

	/**
	 * @return a {@link Module}
	 * @throws AssertionException if the type is null
	 */
	public ModulePojo getModule() {
		return Assert.assertNotNull(module, "Module must not be null");
	}

	/**
	 * @return the count of references for the current module
	 */
	public Long getCount() {
		return count;
	}
}
