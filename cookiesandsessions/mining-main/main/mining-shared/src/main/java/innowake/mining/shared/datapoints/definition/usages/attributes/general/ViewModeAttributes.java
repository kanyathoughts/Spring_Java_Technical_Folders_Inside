/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.datapoints.definition.usages.attributes.general;

import innowake.mining.shared.datapoints.definition.usages.Usages;

/**
 * Usage attributes for {@link Usages#VIEW_MODE}.
 */
public class ViewModeAttributes {

	/**
	 * By default, data points are displayed like plain strings. This attribute sets an alternative view mode.
	 * Attribute value must be one of:
	 * <ul>
	 * <li> {@link #DISPLAY_AS_LINK}
	 * <li> {@link #DISPLAY_AS_TAG}
	 * </ul>
	 */
	public static final String DISPLAY_AS = "displayAs";
	
	/**
	 * Value for the {@link #DISPLAY_AS} attribute. Determines that the data point should be displayed as HTML.
	 */
	public static final String DISPLAY_AS_HTML = "HTML";
	
	/**
	 * Value for the {@link #DISPLAY_AS} attribute. Determines that the data point should be displayed as date.
	 */
	public static final String DISPLAY_AS_DATE = "date";
	
	/**
	 * Value for the {@link #DISPLAY_AS} attribute. Determines that the data point should be displayed
	 * as a click-able link. The {@link #LINK_TEMPLATE} attribute determines the link target.
	 */
	public static final String DISPLAY_AS_LINK = "link";
	
	/**
	 * Value for the {@link #DISPLAY_AS_LINK_OPEN_MODAL} attribute. Determines that the data point should be displayed
	 * as a click-able link which open a modal.
	 */
	public static final String DISPLAY_AS_LINK_OPEN_MODAL = "linkOpenModal";
	
	  /**
	   * Value for the {@link #DISPLAY_AS} attribute.  Determines that the data point should be displayed
	   * as a click-able link that will not apply any routing information applied to the URL and will open
	   * in a new browser tab.
	   */
	public static final String DISPLAY_AS_EXTERNAL_LINK = "externalLink";
	
	/**
	 * Value for the {@link #DISPLAY_AS} attribute. Determines that the data point should be displayed
	 * as a "tag" (a small badge).
	 */
	public static final String DISPLAY_AS_TAG = "tag";
	
	/**
	 * When using {@link #DISPLAY_AS_LINK}, this attribute holds a template string that is used to build the link target.
	 * <p>
	 * This property holds a template string which requires the following context variables for replacement:
	 * <ul>
	 * <li> {@code $clientId}: the id of the current client
	 * <li> {@code $projectId}: the id of the current project
	 * </ul>
	 */
	public static final String LINK_TEMPLATE = "linkTemplate";
	
	/**
	 * Determines that when this data point is displayed, then other data points should be retrieved as well.
	 * The typical use-case is that the information from the other data points is required to construct
	 * the link target from the {@link #LINK_TEMPLATE}.
	 * <p>
	 * This property holds a "relative path" to another data point. This relative path identifies a sibling
	 * data point. The target data point is resolved by dropping the last segment from the path of the current data point
	 * and then appending the value of the "togetherWith" property.
	 * For example, if the current data point's "path" is "foo.bar" 
	 * and the "togetherWith" property holds the value "baz.qux", 
	 * then the data point "foo.baz.qux" should be selected as well.
	 */
	public static final String TOGETHER_WITH = "togetherWith";
	/**
	 * Determines that when this data point is displayed the label mappings should be retrieved.
	 */
	public static final String LABEL_MAPPING = "labelMapping";
	
	private ViewModeAttributes() { }
}
