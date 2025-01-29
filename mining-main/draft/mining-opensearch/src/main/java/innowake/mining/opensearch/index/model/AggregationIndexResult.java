/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.opensearch.index.model;

import java.util.Map;

/**
 * Model class for the AggregationIndexResult
 */
public class AggregationIndexResult {
	
	private Map<String, Object> keyMap;
	private Map<String, Object> aggregationMap;
	
	public AggregationIndexResult(final Map<String, Object> keyMap2, final Map<String, Object> operatorMap) {
        this.keyMap = keyMap2;
        this.aggregationMap = operatorMap;
    }
	
	public Map<String, Object> getKeyMap() {
        return keyMap;
    }
	
	public void setKeyMap(final Map<String, Object> keyMap) {
        this.keyMap = keyMap;
    }
	
	public Map<String, Object> getAggregationMap() {
        return aggregationMap;
    }
	
	public void setAggregationMap(final Map<String, Object> aggregationMap) {
        this.aggregationMap = aggregationMap;
    }
}
