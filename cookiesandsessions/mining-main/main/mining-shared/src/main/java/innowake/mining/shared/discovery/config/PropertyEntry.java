/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.discovery.config;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import innowake.lib.core.api.lang.Nullable;

/**
 * One property as JAXB entry.
 */
@XmlAccessorType(XmlAccessType.FIELD)
public class PropertyEntry {
	
	@XmlAttribute(required = true, name = "key")
	private final String key;
	
	@XmlAttribute(required = true, name = "value")
	private final String value;
	
	@XmlAttribute(required = true, name = "title")
	private final String title;
	
	@Nullable
	@XmlElement(name = "comment")
	private String comment;

	/**
	 * Constructor required by JAXB.
	 */
	public PropertyEntry() {
		key = "";
		value = "";
		title = "";
	}
	
	PropertyEntry(final String key, final String value, final String title) {
		this(key, value, title, null);
	}
	
	PropertyEntry(final String key, final String value, final String title, @Nullable final String comment) {
		this.key = key;
		this.value = value;
		this.title = title;
		this.comment = comment;
	}

	/**
	 * Get the key of this property.
	 * 
	 * @return The key name.
	 */
	public String getKey() {
		return key;
	}

	/**
	 * Get the property value.
	 *
	 * @return The property value.
	 */
	public String getValue() {
		return value;
	}

	/**
	 * Get the property title.
	 *
	 * @return The property title.
	 */
	public String getTitle() {
		return title;
	}
	
}
