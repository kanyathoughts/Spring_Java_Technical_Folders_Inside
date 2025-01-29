/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.spring.data.orientdb.integration.repository.domain;

import java.util.List;
import java.util.Map;
import java.util.Set;
import innowake.mining.shared.springdata.annotations.Entity;

/**
 * An entity with primitive collection fields.
 */
@Entity
public class EntityWithCollectionParams {

	private List<String> stringList;
	private Set<Long> longSet;
	private Map<String, String> stringMap;
	
	/**
	 * Constructor.
	 * @param stringList the list of string
	 * @param longSet the set of long
	 * @param stringMap the map of string
	 */
	public EntityWithCollectionParams(final List<String> stringList, final Set<Long> longSet, final Map<String, String> stringMap) {
		this.stringList = stringList;
		this.longSet = longSet;
		this.stringMap = stringMap;
	}

	/**
	 * Gets the String map.
	 *
	 * @return the String map
	 */
	public Map<String, String> getStringMap() {
		return stringMap;
	}

	/**
	 * Sets the String map.
	 *
	 * @param stringMap the String map
	 */
	public void setStringMap(final Map<String, String> stringMap) {
		this.stringMap = stringMap;
	}

	/**
	 * Gets the list of string.
	 *
	 * @return the list of string
	 */
	public List<String> getStringList() {
		return stringList;
	}

	/**
	 * Gets the set of Long values.
	 *
	 * @return the set of Long values
	 */
	public Set<Long> getLongSet() {
		return longSet;
	}

	/**
	 * Sets the list of string.
	 *
	 * @param stringList the list of string
	 */
	public void setStringList(final List<String> stringList) {
		this.stringList = stringList;
	}

	/**
	 * Sets the set of long values.
	 *
	 * @param longSet the set of long values
	 */
	public void setLongSet(final Set<Long> longSet) {
		this.longSet = longSet;
	}
	
}
