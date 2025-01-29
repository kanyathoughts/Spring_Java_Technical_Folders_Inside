/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model.aggregations;

import java.util.*;

/**
 * Request for aggregated field values.
 *
 * @param <F> the set of valid field names
 */
public class AggregationRequest<F> {

	private Map<F, Map<String, Object>> filterObject = new HashMap<>();
	private Set<F> groupBy = new HashSet<>();
	private List<F> orderBy = new ArrayList<>();
	private Map<F, AggregationOperator> fields = new HashMap<>();
	private Map<F, String> csvHeaders = new HashMap<>();

	/**
	 * Gets the filter
	 *
	 * @return the filter
	 */
	public Map<F, Map<String, Object>> getFilterObject() {
		return filterObject;
	}

	
	/**
	 * Sets the filter
	 *
	 * @param filterObject the filter
	 */
	public void setFilterObject(final Map<F, Map<String, Object>> filterObject) {
		this.filterObject = filterObject;
	}

	/**
	 * Gets the GroupBy set
	 *
	 * @return the GroupBy set
	 */
	public Set<F> getGroupBy() {
		return groupBy;
	}

	/**
	 * Sets the GroupBy set
	 *
	 * @param groupBy the GroupBy set
	 */
	public void setGroupBy(final Set<F> groupBy) {
		this.groupBy = groupBy;
	}

	/**
	 * Gets the OrderBy list
	 *
	 * @return the OrderBy list
	 */
	public List<F> getOrderBy() {
		return orderBy;
	}

	/**
	 * Sets the OrderBy list
	 *
	 * @param orderBy the OrderBy list
	 */
	public void setOrderBy(final List<F> orderBy) {
		this.orderBy = orderBy;
	}

	/**
	 * Gets the fields map
	 *
	 * @return the fields map
	 */
	public Map<F, AggregationOperator> getFields() {
		return fields;
	}

	/**
	 * Sets the fields map
	 *
	 * @param fields the fields map
	 */
	public void setFields(final Map<F, AggregationOperator> fields) {
		this.fields = fields;
	}
	
	/**
	 * Gets the csvHeaders map
	 *
	 * @return the csvHeaders map
	 */
	public final Map<F, String> getCsvHeaders() {
		return csvHeaders;
	}
	
	/**
	 * Sets the csvHeaders
	 *
	 * @param csvHeaders the csvHeaders
	 */
	public final void setCsvHeaders(final Map<F, String> csvHeaders) {
		this.csvHeaders = csvHeaders;
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass()) {
			return false;
		}
		AggregationRequest<?> that = (AggregationRequest<?>) o;
		return Objects.equals(filterObject, that.filterObject)
				&& Objects.equals(groupBy, that.groupBy)
				&& Objects.equals(orderBy, that.orderBy)
				&& Objects.equals(fields, that.fields)
				&& Objects.equals(csvHeaders, that.csvHeaders);
	}

	@Override
	public int hashCode() {
		return Objects.hash(filterObject, groupBy, orderBy, fields, csvHeaders);
	}
}
