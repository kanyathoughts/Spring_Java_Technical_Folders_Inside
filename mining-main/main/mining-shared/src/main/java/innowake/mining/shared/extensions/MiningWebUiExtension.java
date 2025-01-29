/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.extensions;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;

/**
 * Interface for Mining extensions that register additional pages for the web-ui.
 * <p>
 * Such an extension registers a new menu entry on the mining-ui and can load a custom user interface.
 * <p>
 * URLs and properties can contain placeholder using the <pre>${variableName}</pre> syntax. The supported variables are:
 * <ul>
 * <li>clientId</li>
 * <li>projectId</li>
 * </ul>
 * Unsupported variables are replaced by empty string.
 * <p>
 * Classes that implement this interface must be put within (or underneath) the package
 * "innowake.mining.extensions". That way they will be picked up by Spring's component scan mechanism
 * and registered automatically.
 */
public interface MiningWebUiExtension {

	/**
	 * The kind of UI extension that is registered.
	 */
	enum Kind {
		/**
		 * The extension is contributed via an iframe.
		 */
		IFRAME;
		/* can support different options in the future, e.g.:
		 * - WEB_COMPONENT (custom element that follows web component standard; loaded from external script file)
		 * - EXTERNAL_LINK (link to external web page; not integrated)
		 */
	}
	
	/**
	 * The properties of UI extension that is registered.
	 */
	enum Property {
		/**
		 * For {@link Kind#IFRAME}: determines the value of the {@code src} attribute of the iframe.
		 */
		IFRAME_SRC,
		/**
		 * For {@link Kind#IFRAME}: determines the value of the {@code sandbox} attribute of the iframe.
		 */
		IFRAME_SANDBOX
	}

	/**
	 * The name of the extension that is displayed on the menu label in the UI.
	 *
	 * @return the name of the extension
	 */
	String getName();

	/**
	 * An identifier for the extension page, it should be unique.
	 *
	 * @return a page identifier
	 */
	String getPageIdentifier();

	/**
	 * The kind of extension.
	 *
	 * @return one of {@link Kind}
	 */
	Kind getKind();

	/**
	 * CSS classes that are applied to the host element of the extension - e.g. to the {@code <iframe>} element in case of {@link Kind#IFRAME}.
	 * <p>
	 * By default the list is empty.
	 *
	 * @return a list of CSS classes
	 */
	default List<String> getHostElementStyleNames() {
		return Collections.emptyList();
	}

	/**
	 * URLs of additional style sheets that are to be loaded when the extension is activated. The style sheets that are listed here are registered
	 * globally, so care must be taken not to conflict with existing styles.
	 * <p>
	 * Usually there is no need to register any global styles, as the extension can bundle its own styles.
	 * <p>
	 * By default the list is empty.
	 *
	 * @return list of CSS style sheet URLs
	 */
	default List<String> getStyleSheets() {
		return Collections.emptyList();
	}

	/**
	 * URLs of additional scripts that are to be loaded when the extension is activated. The scripts that are listed here are registered globally.
	 * <p>
	 * Usually there is no need to register any global scripts, as the extension can bundle its own scripts. Note that same origin policy may prevent
	 * execution of scripts loaded from an external location.
	 * <p>
	 * By default the list is empty.
	 *
	 * @return list of script URLs
	 */
	default List<String> getScripts() {
		return Collections.emptyList();
	}

	/**
	 * Additional properties that depend on the {@linkplain #getKind() kind} of UI extension.
	 *
	 * @return map of additional properties
	 */
	Map<Property, String> getProperties();
	
	/**
	 * In line styles that depend on the {@linkplain #getKind() kind} of UI extension.
	 * Example: prop1:value1;prop2:value2
	 *  
	 * @return string containing styles
	 */
	default String getInlineStyles() {
		return StringUtils.EMPTY;
	}	
}