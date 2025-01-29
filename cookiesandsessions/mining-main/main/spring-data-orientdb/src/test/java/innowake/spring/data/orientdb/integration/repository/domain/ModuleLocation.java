/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository.domain;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.springdata.annotations.Entity;

/**
 * Vertex entity.
 */
@Entity
public class ModuleLocation {
	
	@Nullable private Integer offset;
	@Nullable private Integer length;
	
	/**
	 * Instantiates ModuleLocation, used by proxy creation.
	 */
	public ModuleLocation() {}
	
	/**
	 * Instantiates ModuleLocation.
	 * 
	 * @param offset the offset
	 * @param length the length
	 */
	public ModuleLocation(final Integer offset, final Integer length) {
		super();
		this.offset = offset;
		this.length = length;
	}
	
	
	/**
	 * Returns the offset.
	 *
	 * @return the offset
	 */
	@Nullable
	public Integer getOffset() {
		return offset;
	}
	
	/**
	 * Sets the offset.
	 *
	 * @param offset the offset
	 */
	public void setOffset(final Integer offset) {
		this.offset = offset;
	}
	
	/**
	 * Returns the length.
	 *
	 * @return the length
	 */
	@Nullable
	public Integer getLength() {
		return length;
	}
	
	/**
	 * Sets the length.
	 *
	 * @param length the length
	 */
	public void setLength(final Integer length) {
		this.length = length;
	}


}
