/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.integration.repository.domain;

import java.util.List;
import java.util.Map;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.springdata.annotations.Entity;
import innowake.mining.shared.springdata.annotations.Id;
import innowake.mining.shared.springdata.annotations.RId;
import innowake.mining.shared.springdata.annotations.RelationshipProperties;

/**
 * Edge entity class.
 */
@Entity
@RelationshipProperties
public class Reviewed {

	@Id(sequence = "Reviewed_Sequence")
	@Nullable
	private Long id;
	@RId
	@Nullable
	private String rid;
	@Nullable
	private Customer out;
	@Nullable
	private Product in;
	@Nullable
	private String description;
	@Nullable
	private Map<String, String> properties; /* Embedded Map */
	@Nullable
	private Address sellerAddress; /* Embedded */
	@Nullable
	private List<RatingEnum> rating; /* Enum */

	/**
	 * Instantiates edge Reviewed.
	 * Used only for proxy creation.
	 */
	public Reviewed() {
	}

	/**
	 * Instantiates edge Reviewed.
	 * 
	 * @param out from vertex {@link Customer}
	 * @param in out vertex {@link Product}
	 */
	public Reviewed(final Customer out, final Product in) {
		this.out = out;
		this.in = in;
	}

	/**
	 * Returns the id of edge entity.
	 *
	 * @return the id of edge entity
	 */
	@Nullable
	public Long getId() {
		return id;
	}

	/**
	 * Sets the id of edge entity.
	 *
	 * @param id the id value
	 */
	public void setId(final Long id) {
		this.id = id;
	}

	/**
	 * Returns the rid of the edge.
	 *
	 * @return the rid of the edge
	 */
	@Nullable
	public String getRid() {
		return rid;
	}

	/**
	 * Sets the rid of the edge.
	 *
	 * @param rid the rid
	 */
	public void setRid(final String rid) {
		this.rid = rid;
	}

	/**
	 * Returns the {@link Customer}.
	 *
	 * @return the {@link Customer}
	 */
	@Nullable
	public Customer getOut() {
		return out;
	}

	/**
	 * Sets the from {@link Customer} for the edge.
	 *
	 * @param out the from {@link Customer} for the edge
	 */
	public void setOut(final Customer out) {
		this.out = out;
	}

	/**
	 * Returns the to vertex {@link Product} for the edge.
	 *
	 * @return the to vertex {@link Product} for the edge
	 */
	@Nullable
	public Product getIn() {
		return in;
	}

	/**
	 * Sets the to vertex {@link Product} for the edge.
	 *
	 * @param in the to vertex {@link Product} for the edge
	 */
	public void setIn(final Product in) {
		this.in = in;
	}

	/**
	 * Returns the description.
	 *
	 * @return the description
	 */
	@Nullable
	public String getDescription() {
		return description;
	}

	/**
	 * Sets the description.
	 *
	 * @param description the description
	 */
	public void setDescription(final String description) {
		this.description = description;
	}

	/**
	 * Returns the map of properties.
	 *
	 * @return the map of properties  
	 */
	@Nullable
	public Map<String, String> getProperties() {
		return properties;
	}

	/**
	 * Sets the map of properties.
	 *
	 * @param properties the map of properties
	 */
	public void setProperties(final Map<String, String> properties) {
		this.properties = properties;
	}

	/**
	 * Returns the embedded field {@link Address}.
	 *
	 * @return the embedded field {@link Address}
	 */
	@Nullable
	public Address getSellerAddress() {
		return sellerAddress;
	}

	/**
	 * Sets the embedded field {@link Address}
	 *
	 * @param sellerAddress the embedded field {@link Address}
	 */
	public void setSellerAddress(final Address sellerAddress) {
		this.sellerAddress = sellerAddress;
	}

	/**
	 * Returns the list of enum {@link RatingEnum}.
	 *
	 * @return the list of enum {@link RatingEnum}
	 */
	@Nullable
	public List<RatingEnum> getRating() {
		return rating;
	}

	/**
	 * Sets the list of enum {@link RatingEnum}
	 *
	 * @param rating the list of enum {@link RatingEnum}
	 */
	public void setRating(final List<RatingEnum> rating) {
		this.rating = rating;
	}

	/**
	 * Enum link class used as property by edge entity.
	 */
	@Entity
	public enum RatingEnum {
		
		/**
		 * ONE.
		 */
		ONE,
		/**
		 * TWO.
		 */
		TWO, 
		/**
		 * THREE.
		 */
		THREE, 
		/**
		 * FOUR.
		 */
		FOUR, 
		/**
		 * FIVE.
		 */
		FIVE;
	}

}
