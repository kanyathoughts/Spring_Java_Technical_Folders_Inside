/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.discovery.config;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * List of properties.
 * Used as container for JAXB
 */
@XmlRootElement(name = "properties")
@XmlAccessorType(XmlAccessType.FIELD)
public class Properties {
	
	@XmlElement(name="property")
	private final List<PropertyEntry> entries = new ArrayList<>();
	
	/**
	 * Add a new property to the list.
	 *
	 * @param entry The property instance.
	 */
	public void add(final PropertyEntry entry) {
		entries.add(entry);
	}
	
	/**
	 * Get the properties as unmodifiable list.
	 *
	 * @return The list of properties.
	 */
	public List<PropertyEntry> getProperties() {
		return Collections.unmodifiableList(entries);
	}
	
	/**
	 * Check if a property with the given name exist.
	 * @param key The key to verify.
	 * 
	 * @return true or false.
	 */
	public boolean contains(final String key) {
		return get(key).isPresent();
	}
	
	/**
	 * Get the property with the given name.
	 *
	 * @param key The key name of the property
	 * @return The property as optional or {@link Optional#empty()}
	 */
	public Optional<PropertyEntry> get(final String key) {
		return entries.stream().filter(entry -> entry.getKey().equals(key)).findFirst();
	}
}
