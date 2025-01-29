package innowake.mining.data.datapoints;

import java.util.Collection;

import innowake.lib.core.api.lang.NonNullByDefault;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.datapoints.annotations.Usage;
import innowake.mining.shared.datapoints.definition.usages.Usages;

/**
 * Test class for validating filtering and sorting on the datapoints.
 */
@MiningDataType(name = "TestFilterAndSorting")
@NonNullByDefault(false)
class TestFilterAndSorting {

	private static final String FILTER_QUERY_NAME = "testQuery";
	@Usage(value = Usages.GRAPHQL_QUERY_PREFIX + FILTER_QUERY_NAME)
	private String testStringProp;
	@Usage(value = Usages.GRAPHQL_QUERY_PREFIX + FILTER_QUERY_NAME)
	private Long testLongProp;

	/**
	 * Gets the string property.
	 *
	 * @return the string property
	 */
	public String getTestStringProp() {
		return testStringProp;
	}

	/**
	 * Sets the string property.
	 *
	 * @param testStringProp the string property
	 */
	public void setTestStringProp(final String testStringProp) {
		this.testStringProp = testStringProp;
	}

	/**
	 * Gets the long property.
	 *
	 * @return the long property
	 */
	public Long getTestLongProp() {
		return testLongProp;
	}

	/**
	 * Sets the long property.
	 *
	 * @param testLongProp the long property
	 */
	public void setTestLongProp(final Long testLongProp) {
		this.testLongProp = testLongProp;
	}

	/**
	 * Test class which contains the callback functions for different filtering types.
	 */
	class TestInquiryBuilder {

		final StringBuilder queryBuilder = new StringBuilder();

		TestInquiryBuilder withId(final Long id) {
			queryBuilder.append("testLongProp = ").append(id);
			return this;
		}
		
		TestInquiryBuilder withIdNotEqual(final Long id) {
			queryBuilder.append("testLongProp != ").append(id);
			return this;
		}
		
		TestInquiryBuilder withIdAbove(final Long id) {
			queryBuilder.append("testLongProp > ").append(id);
			return this;
		}
		
		TestInquiryBuilder withIdBelow(final Long id) {
			queryBuilder.append("testLongProp < ").append(id);
			return this;
		}
		
		TestInquiryBuilder withIds(final Collection<Long> ids) {
			queryBuilder.append("testLongProp = any(");
			for (final Long id : ids) {
				queryBuilder.append(id).append(",");
			}
			queryBuilder.deleteCharAt(queryBuilder.length() - 1);
			queryBuilder.append(")");
			return this;
		}
		
		TestInquiryBuilder withNameNotIn(final Collection<String> ids) {
			queryBuilder.append("testStringProp != any(");
			for (final String id : ids) {
				queryBuilder.append(id).append(",");
			}
			queryBuilder.deleteCharAt(queryBuilder.length() - 1);
			queryBuilder.append(")");
			return this;
		}

		@SuppressWarnings("unused")
		TestInquiryBuilder isTrue(final Boolean value) {
			queryBuilder.append("testStringProp = TRUE");
			return this;
		}

		@SuppressWarnings("unused")
		TestInquiryBuilder isFalse(final Boolean value) {
			queryBuilder.append("testStringProp = FALSE");
			return this;
		}

		@SuppressWarnings("unused")
		TestInquiryBuilder isAbsent(final Object value) {
			queryBuilder.append("testStringProp IS NULL");
			return this;
		}
		
		String getQueryFilter() {
			return queryBuilder.toString();
		}
		
	}
	
	/**
	 * Test class which contains the callback functions for sorting.
	 */
	class TestOrderBuilder {
		
		final StringBuilder queryBuilder = new StringBuilder();
		
		TestOrderBuilder sortId(final SortDirection direction) {
			queryBuilder.append(" testLongProp " + direction);
			return this;
		}
		
		TestOrderBuilder sortName(final SortDirection direction) {
			queryBuilder.append(" testStringProp " + direction);
			return this;
		}
		
		String getQueryFilter() {
			return queryBuilder.toString();
		}
	}
	
}
