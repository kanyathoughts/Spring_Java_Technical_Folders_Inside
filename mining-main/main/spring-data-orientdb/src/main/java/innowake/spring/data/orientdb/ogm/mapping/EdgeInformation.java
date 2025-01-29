/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.ogm.mapping;

import com.orientechnologies.orient.core.record.ODirection;

import innowake.mining.shared.springdata.EdgeDirection;
import innowake.spring.data.orientdb.commons.exception.MetadataException;

/**
 * Defines the edge name in DB and it's direction with respect to a vertex.
 */
public class EdgeInformation {
	
	private final String name;
	private final EdgeDirection direction;
	private final String sequenceName;
	
	/**
	 * Instantiates {@link EdgeInformation} class.
	 * 
	 * @param name the edge name
	 * @param direction the direction of the edge
	 * @param sequenceName the sequence name of id field.
	 */
	public EdgeInformation(final String name, final EdgeDirection direction, final String sequenceName) {
		this.name = name;
		this.direction = direction;
		this.sequenceName = sequenceName;
	}

	/**
	 * Returns the direction of the edge.
	 *
	 * @return the direction of the edge
	 */
	public ODirection getDirection() {
		switch (direction) {
			case OUT:
				return ODirection.OUT;
			case IN:
				return ODirection.IN;
			case BOTH:
				return ODirection.BOTH;
			default:
				throw new MetadataException("Not a valid direction type");

		}
	}

	/**
	 * Returns the name of edge in DB.
	 *
	 * @return the name of edge in DB.
	 */
	public String getName() {
		return name;
	}

	/**
	 * Returns the sequence name for {@code id} field.
	 *
	 * @return the sequence name for {@code id} field
	 */
	public String getSequenceName() {
		return sequenceName;
	}
	
}
