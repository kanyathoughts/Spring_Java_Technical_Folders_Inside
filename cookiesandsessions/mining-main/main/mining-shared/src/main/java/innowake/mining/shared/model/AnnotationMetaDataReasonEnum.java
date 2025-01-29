/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model;

import java.util.HashMap;
import java.util.Map;

import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.springdata.annotations.Entity;

/**
 * Annotation Metadata reason Enum
 */
@Entity(name = "AnnotationMetaDataReasonEnum")
@MiningDataType(name = "AnnotationMetaDataReasonEnum")
public enum AnnotationMetaDataReasonEnum {

	IF_ELSE_CONDITION("If-else condition"),
	MULTI_EXPRESSION_IF_ELSE_CONDITION("Multi Expression If-else condition"),
	LOOP_CONDITION("Loop condition"),
	COBOL_EVALUATE_CONDITION("Cobol evaluate condition"),
	NATURAL_LOOP_FILE_ACCESS_CONDITION("Natural Loop file access condition"),
	SELECT_CONDITION("Select condition"),
	OTHER_CONDITION("Other condition"),
	COMPUTATION("Computation"),
	INPUT_FROM_EXTERNAL_DATA_SOURCE("Input from external data source"),
	OUTPUT_TO_EXTERNAL_DATA_SOURCE("Output to external data source"),
	NESTED_CONDITION("Nested condition"),
	BUSINESS_VARIABLE_IDENTIFIED("Business Variable Identified"),
	NESTED_IF_ELSE_CONDITION("Nested If Else condition"),
	NESTED_LOOP_CONDITION("Nested Loop condition"),
	BV_TRANSFORMATION("Business Variable Transformation"),
	NESTED_EVALUATE_CONDITION("Nested Evaluate condition");
	
	private final String description;
	
	private AnnotationMetaDataReasonEnum(final String description) {
		this.description = description;
	}
	
	private static final Map<String, AnnotationMetaDataReasonEnum> values = new HashMap<>();
	static {
		for (final AnnotationMetaDataReasonEnum value : AnnotationMetaDataReasonEnum.values()) {
			values.put(value.description.toLowerCase(), value);
			values.put(value.name().toLowerCase(), value);
		}
	}

	/**
	 * Gets the description of the Metadata reason
	 *
	 * @return the description of the Metadata reason
	 */
	public String getDescription() {
		return description;
	}
	
	public static AnnotationMetaDataReasonEnum valueOfIgnoreCase(final String name) {
		return values.get(name.toLowerCase());
	}
}
