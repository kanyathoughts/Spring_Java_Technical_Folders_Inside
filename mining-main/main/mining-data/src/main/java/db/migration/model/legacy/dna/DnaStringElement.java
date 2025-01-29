package db.migration.model.legacy.dna;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.model.Entity;
import innowake.mining.shared.model.ModuleLocation;
import static innowake.lib.core.lang.Assert.assertNotNull;

/**
 * Represents one dna string element. Multiple dna string element constitutes one {@link DnaString}. Each string element has a {@link ModuleLocation}.
 * 
 * DO NOT CHANGE THIS FILE !!!
 * 
 * @deprecated must be used in old java based migration scripts only. Will be removed once migration from OrientDB to Postres is done.
 */
@Deprecated
public class DnaStringElement extends Entity {

	@Nullable
	private String dnaStringRecordId;
	@Nullable
	private ModuleLocation moduleLocation;
	@Nullable
	private String value;
	private int index;

	/**
	 * Gets the record id of the {@link DnaString}.
	 *
	 * @return the record id of the {@link DnaString}
	 */
	public String getDnaStringRecordId() {
		return assertNotNull(dnaStringRecordId, "DNA string record id cannot be null");
	}

	/**
	 * Sets the record id of the {@link DnaString}.
	 *
	 * @param dnaStringRecordId record id of the {@link DnaString}
	 */
	public void setDnaStringRecordId(final String dnaStringRecordId) {
		this.dnaStringRecordId = dnaStringRecordId;
	}

	/**
	 * Gets the {@link ModuleLocation}.
	 *
	 * @return the {@link ModuleLocation}
	 */
	public ModuleLocation getModuleLocation() {
		return assertNotNull(moduleLocation, "Module location cannot be null");
	}

	/**
	 * Sets the {@link ModuleLocation}.
	 *
	 * @param moduleLocation the {@link ModuleLocation}
	 */
	public void setModuleLocation(final ModuleLocation moduleLocation) {
		this.moduleLocation = moduleLocation;
	}

	/**
	 * Gets the value of an element in {@link DnaString}.
	 *
	 * @return the value of an element in {@link DnaString}
	 */
	public String getValue() {
		return assertNotNull(value, "String value cannot be null");
	}

	/**
	 * Sets the value of an element in {@link DnaString}.
	 *
	 * @param value the value of an element in {@link DnaString}
	 */
	public void setValue(final String value) {
		this.value = value;
	}

	/**
	 * Gets the index of an element in {@link DnaString}.
	 *
	 * @return the index of an element in {@link DnaString}
	 */
	public int getIndex() {
		return index;
	}
	
	/**
	 * Sets the index of an element in {@link DnaString}.
	 *
	 * @param index the index of an element in {@link DnaString}
	 */
	public void setIndex(final int index) {
		this.index = index;
	}
}
