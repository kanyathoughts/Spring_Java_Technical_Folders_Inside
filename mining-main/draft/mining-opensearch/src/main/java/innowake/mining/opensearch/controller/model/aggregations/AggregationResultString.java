/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.opensearch.controller.model.aggregations;

import java.util.Map;

import innowake.mining.shared.model.aggregations.AggregationResult;

/**
* Result with aggregated field values.
*/
public class AggregationResultString extends AggregationResult<String>{

	public AggregationResultString(final Map<String, Object> aggregationGroup, final Map<String, Object> aggregationFields) {
		this.setGroup(aggregationGroup);
		this.setFields(aggregationFields);
	}

}
