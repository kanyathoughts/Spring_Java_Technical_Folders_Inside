/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.extensions;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;

import innowake.mining.shared.extensions.MiningWebUiExtension.Kind;
import innowake.mining.shared.extensions.MiningWebUiExtension.Property;

/**
 * Model for holding response for the WebUiExtension. 
 */
public class WebUiExtensionDescription {
	private final String name;
	private final String pageIdentifier;
	private final Kind kind;
	private final List<String> hostElementStyleNames;
	private final List<String> styleSheets;
	private final List<String> scripts;
	private final Map<Property, String> properties;
	private final String inlineStyles;

	/**
	 * Default constructor for {@link WebUiExtensionDescription}
	 */
	public WebUiExtensionDescription() {
		this.name = StringUtils.EMPTY;
		this.pageIdentifier = StringUtils.EMPTY;
		this.kind = Kind.IFRAME;
		this.hostElementStyleNames = Collections.emptyList();
		this.styleSheets = Collections.emptyList();
		this.scripts = Collections.emptyList();
		this.properties = Collections.emptyMap();
		this.inlineStyles = StringUtils.EMPTY;
	}

	/**
	 * Constructor for {@link WebUiExtensionDescription}
	 * 
	 * @param name name of extension
	 * @param pageIdentifier extension identifier
	 * @param kind extension kind
	 * @param hostElementStyleNames list of extension host element styles
	 * @param styleSheets list of extension styles
	 * @param scripts list of extension scripts
	 * @param properties extension properties
	 * @param inlineStyles extension in line styles
	 */
	public WebUiExtensionDescription(final String name, final String pageIdentifier, final Kind kind, final List<String> hostElementStyleNames,
			final List<String> styleSheets, final List<String> scripts, final Map<Property, String> properties, final String inlineStyles) {
		this.name = name;
		this.pageIdentifier = pageIdentifier;
		this.kind = kind;
		this.hostElementStyleNames = hostElementStyleNames;
		this.styleSheets = styleSheets;
		this.scripts = scripts;
		this.properties = properties;
		this.inlineStyles = inlineStyles;
	}

	/**
	 * Returns the data for the web ui extension
	 * 
	 * @param extension MiningWebuiExtension for which data is being fetched
	 * @return WebUiExtensionDescription for the extension
	 */
	public static WebUiExtensionDescription fromWebUiExtension(final MiningWebUiExtension extension) {
		return new WebUiExtensionDescription(
				extension.getName(),
				extension.getPageIdentifier(),
				extension.getKind(),
				extension.getHostElementStyleNames(),
				extension.getStyleSheets(),
				extension.getScripts(),
				extension.getProperties(),
				extension.getInlineStyles());
	}

	/**
	 * gets the name of extension.
	 *
	 * @return extension name
	 */
	public String getName() {
		return name;
	}

	/**
	 * gets the name of extension identifier.
	 *
	 * @return extension identifier
	 */
	public String getPageIdentifier() {
		return pageIdentifier;
	}

	/**
	 * gets the kind of extension.
	 *
	 * @return extension kind
	 */
	public Kind getKind() {
		return kind;
	}

	/**
	 * gets the styles for extension host element.
	 *
	 * @return extension host styles 
	 */
	public List<String> getHostElementStyleNames() {
		return hostElementStyleNames;
	}

	/**
	 * gets the styles for extension.
	 *
	 * @return extension styles
	 */
	public List<String> getStyleSheets() {
		return styleSheets;
	}

	/**
	 * gets the scripts for extension.
	 *
	 * @return extension scripts
	 */
	public List<String> getScripts() {
		return scripts;
	}

	/**
	 * gets the properties for extension.
	 *
	 * @return extension properties
	 */
	public Map<Property, String> getProperties() {
		return properties;
	}
	
	/**
	 * gets the inline styles for extension.
	 *
	 * @return extension inline styles
	 */
	public String getInlineStyles() {
		return inlineStyles;
	}
}