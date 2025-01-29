/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.shared.datapoints.definition.usages.attributes.general;

import innowake.mining.shared.datapoints.definition.usages.Usages;

/**
 * Usage attributes for {@link Usages#EDIT_MODE}.
 */
public class EditModeAttributes {

	/**
	 * Defines what UI component should be used when editing this data point.
	 * Attribute value must be one of:
	 * <ul>
	 *     <li>{@link #EDIT_AS_TEXT}</li>
	 *     <li>{@link #EDIT_AS_TEXT_AREA}</li>
	 * </ul>
	 */
	public static final String EDIT_AS = "editAs";
	/**
	 * Value for the {@link #EDIT_AS} attribute. Defines that the data point should be edited using a one-line text field.
	 */
	public static final String EDIT_AS_TEXT = "text";
	/**
	 * Value for the {@link #EDIT_AS} attribute. Defines that the data point should be edited using a multi-line text area.
	 */
	public static final String EDIT_AS_TEXT_AREA = "textArea";

	/**
	 * The REST endpoint to use for updating this data point. By convention, the endpoint must use the PUT operation
	 * and consume a JSON-object in the body of the request.
	 * <p>
	 * This usage attribute may contain a link template, like {@link ViewModeAttributes#LINK_TEMPLATE}.
	 */
	public static final String EDIT_ENDPOINT = "editEndpoint";
	/**
	 * The field name that should be used for this data point on the JSON object that is sent to the edit endpoint.
	 * @see #EDIT_ENDPOINT
	 */
	public static final String EDIT_ENDPOINT_FIELD_NAME = "editEndpointFieldName";

	/**
	 * Like {@link ViewModeAttributes#TOGETHER_WITH}, this attribute indicates that when editing this data point, a neighboring datapoint
	 * must also be selected. Usually this property is used because the value of the other data point is required for constructing the {@link #EDIT_ENDPOINT} url,
	 * i.e. it is used in the url template.
	 */
	public static final String TOGETHER_WITH = "togetherWith";

	private EditModeAttributes() {}
}
