package innowake.mining.shared.access;

import innowake.mining.shared.model.FieldName;
import innowake.mining.shared.model.aggregations.AggregationOperator;

/**
 * The interface for all entity aggregation inquiry builder.
 * @param <F> the concrete type of the aggregation {@link FieldName FieldNames}
 * @param <B> the concrete type of this {@code AggregationInquiryBuilder}.
 */
public interface AggregationInquiryBuilder<F extends Enum<F> & FieldName, B extends AggregationInquiryBuilder<F, B>> {

	/**
	 * Adds a field to the aggregation.
	 *
	 * @param fieldName the field to aggregate
	 * @param operator the aggregation operator to use
	 * @return the builder
	 */
	B aggregate(F fieldName, AggregationOperator operator);

	/**
	 * Group by the given {@code field}. The field is selected too with the alias: {@code field.name().toLowerCase()}
	 *
	 * @param field the field to group by
	 * @return this instance for method chaining
	 */
	B groupBy(F field);

	/**
	 * Order by the given {@code field}.
	 *
	 * @param field the field to order by
	 * @return this instance for method chaining
	 */
	B orderBy(F field);

	/**
	 * Call if fields must be fetched distinct. The inquiry builder will then do a {@code SELECT DISTINCT field(s)} instead of a {@code SELECT field(s)}.
	 *
	 * @return this instance for method chaining
	 */
	B distinct();
}
