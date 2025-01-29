/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.datapoints.definition.usages.attributes.general;

import innowake.mining.shared.datapoints.definition.usages.Usages;

/**
 * Usage attributes for {@link Usages#SEARCH_FILTER}.
 */
public class SearchFilterAttributes {
	
	/**
	 * Determines the filter mode to use for the data point. Attribute value must be one of
	 * <ul>
	 * <li> {@link #FILTER_MODE_TEXT}
	 * <li> {@link #FILTER_MODE_NUMBER}
	 * <li> {@link #FILTER_MODE_MULTI_SELECT}
	 * </ul>
	 */
	public static final String FILTER_MODE = "filterMode";
	
	/**
	 * Value for the {@link #FILTER_MODE} attribute. Determines that a free text filter should be used
	 * for the data point.
	 */
	public static final String FILTER_MODE_TEXT = "text";
	public static final String FILTER_MODE_NUMBER = "number";
	
	/**
	 * Value for the {@link #FILTER_MODE} attribute. Determines that a custom property tag filter should be used
	 * for the data point.
	 */
	public static final String FILTER_MODE_CUSTOM_PROPERTY_TAG = "customPropertyTag";

	/**
	 * Value for the {@link #FILTER_MODE} attribute. Determines that filter values can be selected
	 * from a list of options. The {@link #MULTI_SELECT_VALUE_RETRIEVAL_MODE} attribute determines how this list is retrieved.
	 */
	public static final String FILTER_MODE_MULTI_SELECT = "multiSelect";

	/**
	 * Value for the {@link #FILTER_MODE} attribute. Determines that filter values can be selected
	 * from a tree structure. The { #MULTI_SELECT_VALUE_RETRIEVAL_MODE} attribute determines how this list is retrieved.
	 */
	public static final String FILTER_MODE_TREE_SELECT = "treeSelect";
	
	/**
	 * Fragment to use in an RSQL filter string. The fragment can be chained to other fragments
	 * using "and" or "or" operators.
	 * <p>
	 * This property holds a template string which requires the following context variables for replacement:
	 * <ul>
	 * <li> {@code $query}: the search value
	 * </ul>
	 */
	public static final String RSQL_FRAGMENT = "rsqlFragment";

	/**
	 * SQL Fragment to be used for filtering when an "equals" filter is applied to the data point.
	 */
	public static final String SQL_FRAGMENT_EQ = "sqlFragmentEq";
	/**
	 * SQL Fragment to be used for filtering when a boolean filter with value "true" is applied to the data point.
	 */
	public static final String SQL_FRAGMENT_TRUE = "sqlFragmentTrue";
	/**
	 * SQL Fragment to be used for filtering when a boolean filter with value "false" is applied to the data point.
	 */
	public static final String SQL_FRAGMENT_FALSE = "sqlFragmentFalse";
	/**
	 * SQL Fragment to be used for filtering when a "greater than or equals" filter is applied to the data point.
	 */
	public static final String SQL_FRAGMENT_GTE = "sqlFragmentGte";
	/**
	 * SQL Fragment to be used for filtering when a "greater than" filter is applied to the data point.
	 */
	public static final String SQL_FRAGMENT_GT = "sqlFragmentGt";
	/**
	 * SQL Fragment to be used for filtering when a "null" filter is applied to the data point.
	 */
	public static final String SQL_FRAGMENT_NONE = "sqlFragmentNone";
	/**
	 * SQL Fragment to be used for filtering when a "less than or equals" filter is applied to the data point.
	 */
	public static final String SQL_FRAGMENT_LTE = "sqlFragmentLte";
	/**
	 * SQL Fragment to be used for filtering when a "less than" filter is applied to the data point.
	 */
	public static final String SQL_FRAGMENT_LT = "sqlFragmentLt";
	/**
	 * SQL Fragment to be used to match a value against a list of possible values is applied to the data point.
	 */
	public static final String SQL_FRAGMENT_IN = "sqlFragmentIn";

	/**
	 * Suffix to append to the SQL_FRAGMENT_* usage attributes to define additional flags for the fragment
	 */
	public static final String SQL_FRAGMENT_FLAGS_SUFFIX = "Flags";
	/**
	 * Flag indicating that the search value must be converted to lower case.
	 */
	public static final String SQL_FRAGMENT_FLAG_TO_LOWERCASE = "toLowercase";
	/**
	 * Flag indicating that '{@code %}' needs to be added at the end of the input.
	 */
	public static final String SQL_FRAGMENT_FLAG_BEGINS_WITH = "beginsWith";
	/**
	 * Flag indicating that '{@code %}' needs to be added at the start of the input.
	 */
	public static final String SQL_FRAGMENT_FLAG_ENDS_WITH = "endsWith";
	/**
	 * Flag indicating that escaping must be applied to the search value so it can be used in a Lucene index query.
	 */
	public static final String SQL_FRAGMENT_FLAG_ESCAPE_LUCENE = "escapeLucene";
	
	/**
	 * Determines where the list of possible values for a {@link #FILTER_MODE_MULTI_SELECT}
	 * filter can be retrieved from.
	 */
	public static final String MULTI_SELECT_VALUE_RETRIEVAL_MODE = "multiSelectValueRetrievalMode";
	
	/**
	 * Value for the {@link #MULTI_SELECT_VALUE_RETRIEVAL_MODE} attribute. Determines that the list of values is retrieved
	 * via a request to {@code AnnotationController.getAggregatedValues()}.
	 */
	public static final String MULTI_SELECT_VALUE_RETRIEVAL_MODE_ANNOTATION_AGGREGATED_VALUES = "annotationControllerAggregatedValues";
	
	/**
	 * Value for the {@link #MULTI_SELECT_VALUE_RETRIEVAL_MODE} attribute. Determines that the list of values is retrieved
	 * via a request to {@code ModuleController.getAggregatedValues()}.
	 */
	public static final String MULTI_SELECT_VALUE_RETRIEVAL_MODE_MODULE_AGGREGATED_VALUES = "moduleControllerAggregatedValues";
	
	/**
	 * Value for the {@link #MULTI_SELECT_VALUE_RETRIEVAL_MODE} attribute. Determines that the list of values is retrieved
	 * via a request to {@code TaxonomyController.getDistinctFieldValues()}.
	 */
	public static final String MULTI_SELECT_VALUE_RETRIEVAL_MODE_TAXONOMY_AGGREGATED_VALUES = "taxonomyControllerAggregatedValues";
	
	/**
	 * Value for the {@link #MULTI_SELECT_VALUE_RETRIEVAL_MODE} attribute. Determines that the list of values is retrieved
	 * via a request to {@code ModuleController.getDistinctFieldValues()}.
	 */
	public static final String MULTI_SELECT_VALUE_RETRIEVAL_MODE_MODULE_DISTINCT_FIELD_VALUES = "moduleControllerDistinctFieldValues";
	
	/**
	 * When using {@link #MULTI_SELECT_VALUE_RETRIEVAL_MODE_MODULE_DISTINCT_FIELD_VALUES},
	 * determines the value to use for the {@code field} parameter of {@code ModuleController.getDistinctFieldValues()}.
	 * <p>
	 * When using {@link #MULTI_SELECT_VALUE_RETRIEVAL_MODE_ANNOTATION_AGGREGATED_VALUES}
	 * or {@link #MULTI_SELECT_VALUE_RETRIEVAL_MODE_MODULE_AGGREGATED_VALUES},
	 * determines the value to use for the {@code groupBy} parameter of the aggregation request.
	 */
	public static final String MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME = "multiSelectValueRetrievalFieldName";
	
	/**
	 * Value for the {@link #MULTI_SELECT_VALUE_RETRIEVAL_MODE} attribute.
	 * Determines the field to use as RSQL filter argument when querying data.
	 */
	public static final String MULTI_SELECT_VALUE_RETRIEVAL_KEY_FIELD = "multiSelectValueRetrievalKeyField";
	
	/**
	 * Value for the {@link #MULTI_SELECT_VALUE_RETRIEVAL_MODE} attribute.
	 * Optionally specifies an RSQL filter to be passed to the aggregation service.
	 */
	public static final String MULTI_SELECT_VALUE_RETRIEVAL_FILTER = "multiSelectValueRetrievalFilter";
	/**
	 * Value for the {@link #MULTI_SELECT_VALUE_RETRIEVAL_MODE} attribute. Determines that the list of values is retrieved
	 * via a request to {@code DataDictionaryController.getAggregatedValues()}.
	 */
	public static final String MULTI_SELECT_VALUE_RETRIEVAL_MODE_DATA_DICTIONARY_AGGREGATED_VALUES = "dataDictionaryControllerAggregatedValues";
	
	/**
	 * Determines whether to use None option for {@link #FILTER_MODE_MULTI_SELECT} or not
	 */
	public static final String MULTI_SELECT_SHOW_NONE_OPTION = "showNoneOption";
	
	/**
	 * Value for the {@link #MULTI_SELECT_VALUE_RETRIEVAL_MODE} attribute. Determines that the list of values is retrieved
	 * via a request to {@code ReferenceController.getAggregatedValues()}.
	 */
	public static final String MULTI_SELECT_VALUE_RETRIEVAL_MODE_REFERENCES_AGGREGATED_VALUES = "referenceControllerAggregatedValues";
	
	/**
	 * Value for the {@link #MULTI_SELECT_FIXED_VALUES} attribute. Determines that the list of fixedValues values is retrieved
	 * via a request to {@code ReferenceController.getAggregatedValues()}.
	 */
	public static final String MULTI_SELECT_FIXED_VALUES = "fixedValues"; 
	
	private SearchFilterAttributes() {}
}
