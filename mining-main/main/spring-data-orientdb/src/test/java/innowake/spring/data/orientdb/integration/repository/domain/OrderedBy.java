/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository.domain;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.springdata.annotations.Entity;
import innowake.mining.shared.springdata.annotations.RelationshipProperties;

/**
 * Edge class linking {@link Customer} and  {@link Product}.
 */
@Entity
@RelationshipProperties
public class OrderedBy {
	
	@Nullable private Product in;
	@Nullable private Customer out;
	
	/**
	 * Default constructor.
	 */
	public OrderedBy() {}
	
	/**
	 * Parameterized constructor.
	 * @param out the {@link Customer}
	 * @param in the {@link Product}
	 */
	public OrderedBy(final Product in, final Customer out) {
		super();
		this.in = in;
		this.out = out;
	}
	
	/**
	 * Returns the {@link Customer}
	 *
	 * @return the {@link Customer}
	 */
	@Nullable
	public Customer getOut() {
		return out;
	}
	
	/**
	 * Sets the {@link Customer}.
	 *
	 * @param out the {@link Customer}
	 */
	public void setOut(final Customer out) {
		this.out = out;
	}
	
	/**
	 * Returns the {@link Product}.
	 *
	 * @return the {@link Product}
	 */
	@Nullable
	public Product getIn() {
		return in;
	}
	
	/**
	 * Sets the {@link Product}.
	 *
	 * @param in the {@link Product}
	 */
	public void setIn(final Product in) {
		this.in = in;
	}
	

}
