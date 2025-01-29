/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.discovery.featurereport.model;

import java.util.Comparator;
import java.util.Objects;

import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

public class TechnologyAndType implements Comparable<TechnologyAndType> {
	private final Technology technology;
	private final Type type;

	public TechnologyAndType(Technology technology, Type type) {
		this.technology = technology;
		this.type = type;
	}
	
	public Technology getTechnology() {
		return technology;
	}

	public Type getType() {
		return type;
	}

	@Override
	public int hashCode() {
		return Objects.hash(technology, type);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		TechnologyAndType other = (TechnologyAndType) obj;
		return technology == other.technology && type == other.type;
	}
	
	@Override
	public String toString() {
		return "" + technology + "_" + type;
	}

	@Override
	public int compareTo(TechnologyAndType o) {
		return Comparator.comparing(TechnologyAndType::getTechnology).thenComparing(TechnologyAndType::getType).compare(this, o);
	}
}