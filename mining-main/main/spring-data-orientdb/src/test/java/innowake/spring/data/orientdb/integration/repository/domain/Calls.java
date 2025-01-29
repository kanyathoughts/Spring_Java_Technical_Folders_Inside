/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.integration.repository.domain;

import java.util.List;
import java.util.Map;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.springdata.annotations.Entity;
import innowake.mining.shared.springdata.annotations.RelationshipProperties;

/**
 * Edge entity.
 */
@Entity
@RelationshipProperties
public class Calls extends Reference {

	@Nullable
	private Customer out;
	@Nullable
	private Customer in;
	@Nullable
	private List<Employee> linkList;

	/**
	 * Instantiates Calls, used by proxy creation.
	 */
	public Calls() {
	}

	/**
	 * Instantiates Calls.
	 * 
	 * @param in instance of {@link Customer}
	 * @param out instance of {@link Customer}
	 * @param fromModuleLocationLink instance of {@link ModuleLocation}
	 * @param toModuleLocation instance of {@link ModuleLocation}
	 * @param properties map of {@link String}
	 */
	public Calls(final Customer in, final Customer out, final ModuleLocation fromModuleLocationLink, final ModuleLocation toModuleLocation,
			final Map<String, String> properties) {
		super(fromModuleLocationLink, toModuleLocation, properties);
		this.out = out;
		this.in = in;
	}

	/**
	 * Parameterized constructor for Calls class.
	 * @param in incoming {@link Customer} entity
	 * @param out outgoing {@link Customer} entity
	 */
	public Calls(final Customer in, final Customer out) {
		this.out = out;
		this.in = in;
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
	 * Sets the {@link Customer}.
	 *
	 * @param out the {@link Customer}
	 */
	public void setOut(final Customer out) {
		this.out = out;
	}

	/**
	 * Returns the {@link Customer}.
	 *
	 * @return the {@link Customer}
	 */
	@Nullable
	public Customer getIn() {
		return in;
	}

	/**
	 * Sets the {@link Customer}.
	 *
	 * @param in the {@link Customer}
	 */
	public void setIn(final Customer in) {
		this.in = in;
	}

	
	/**
	 * Returns list of employees.
	 *
	 * @return returns list of employees.
	 */
	@Nullable
	public List<Employee> getLinkList() {
		return linkList;
	}

	
	/**
	 * Sets list of {@link Employee}.
	 *
	 * @param linkList list of {@link Employee}
	 */
	public void setLinkList(final List<Employee> linkList) {
		this.linkList = linkList;
	}

}
