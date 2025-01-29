/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.model.discovery;

import java.io.Serializable;
import java.util.Objects;

import com.google.common.base.MoreObjects;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.shared.entities.ModuleDeadCodePojoPrototype;

/**
 * This class is used to represent a deadcode in a model artifact.
 */
public class ModelDeadCode implements Serializable {

	@Nullable protected String name;
	
	protected int offset; /* Starting Line */
	
	protected int length; /* Number of Lines */
	
	public ModelDeadCode() {
		this.offset = -1;
		this.length = -1;
	}
	
	public ModelDeadCode(final String name, final int offset, final int length) {
		this.name = name;
		this.offset = offset;
		this.length = length;
	}

	public ModelDeadCode(final ModelDeadCode deadcode) {
		this.setFromDeadCode(deadcode);
	}
	
	@Override
	public String toString() {
		return MoreObjects.toStringHelper(this).omitNullValues()
				.add("name", name)
			.toString();
	}
	
	@Override
	public boolean equals(@Nullable final Object obj) {
		if (obj == null) {
			return false;
		}
		if (this == obj) {
			return true;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		
		final ModelDeadCode other = (ModelDeadCode) obj;
		return Objects.equals(name, other.name);
	}

	@Override
	public int hashCode() {
		return Objects.hash(name);
	}
	
	public String getName() {
		validate();
		return Assert.assertNotNull(name, "Name must not be NULL");
	}

	public ModelDeadCode setName(final String name) {
		this.name = name;
		return this;
	}
	
	public int getOffset() {
		validate();
		return offset;
	}

	
	public ModelDeadCode setOffset(final int offset) {
		this.offset = offset;
		return this;
	}

	
	public int getLength() {
		validate();
		return length;
	}

	
	public ModelDeadCode setLength(final int length) {
		this.length = length;
		return this;
	}

	public ModelDeadCode setFromDeadCode(final ModelDeadCode deadcode) {
		deadcode.validate();
		this.name = deadcode.name;
		this.offset = deadcode.offset;
		this.length = deadcode.length;
		return this;
	}
	
	public ModelDeadCode validate() {
		if (name == null) {
			throw new IllegalArgumentException("The 'name' must be set.");
		}
		if (offset == -1) {
			throw new IllegalArgumentException("The 'offset' (Starting Line) must be set.");
		}
		if (length == -1) {
			throw new IllegalArgumentException("The 'length' (Number of Lines) must be set.");
		}
		return this;
	}

	/**
	 * @return a new {@link ModuleDeadCodePojoPrototype} instance containing all values of this {@link ModelDeadCode}.
	 */
	public ModuleDeadCodePojoPrototype convertToPojoPrototype() {
		return new ModuleDeadCodePojoPrototype()
				.setDeadCode(getName())
				.setStartingLine(offset)
				.setNumberOfLines(length);
	}
}
