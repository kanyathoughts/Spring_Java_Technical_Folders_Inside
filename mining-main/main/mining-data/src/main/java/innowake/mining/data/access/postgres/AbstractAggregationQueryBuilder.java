/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.access.postgres;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apache.commons.collections4.map.ListOrderedMap;

import innowake.mining.data.access.postgres.PgDao.FilterStreamBuilder;
import innowake.mining.data.access.postgres.PgDao.GroupStreamBuilder;
import innowake.mining.data.access.postgres.PgDao.OrderStreamBuilder;
import innowake.mining.data.access.postgres.PgDao.QueryBuilder;
import innowake.mining.data.access.postgres.PgUtil.TableBuilder;
import innowake.mining.shared.access.AggregationInquiryBuilder;
import innowake.mining.shared.access.Table;
import innowake.mining.shared.model.FieldName;
import innowake.mining.shared.model.aggregations.AggregationOperator;

/**
 * 
 * @param <F> The type of the FieldName
 * @param <B> The concrete type of the {@link AbstractAggregationQueryBuilder}
 */
public abstract class AbstractAggregationQueryBuilder<F extends Enum<F> & FieldName, B extends AggregationInquiryBuilder<F, B>>
	implements AggregationInquiryBuilder<F, B> {

	protected final FilterStreamBuilder filters = new FilterStreamBuilder();
	protected final GroupStreamBuilder groupBys = new GroupStreamBuilder();
	protected final OrderStreamBuilder orderBys = new OrderStreamBuilder();

	protected final Map<F, AggregationOperator> aggregations = new ListOrderedMap<>();
	protected final List<F> groupByFields = new ArrayList<>();
	protected boolean distinct;

	/**
	 * @param dao the {@link PgDao} of the builder
	 * @return the prepared {@link QueryBuilder} for executing the aggregation query
	 */
	protected Optional<Table> buildAggregation(final PgDao dao) {
		return Optional.ofNullable(dao.query("SELECT ") /* outer select */
				.with(this::buildOuterFields) /* outer select: group by fields and aggregations */
				.append(" FROM (SELECT ") /* inner select */
				.when(distinct, q -> q.append("DISTINCT "))
				.with(this::buildInnerFields) /* inner select: group by fields and aggregations */
				.append(" FROM ")
				.append(getFromClause())
				.append(" ")
				.with(this::buildJoins)
				.append(" ")
				.with(filters::build)
				.append(") AS a ")
				.with(groupBys::build)
				.with(orderBys::build)
				.build(TableBuilder::build));
	}

	/**
	 * @return the from clause for the aggregation query
	 */
	protected abstract String getFromClause();

	/**
	 * Builds all required table joins.
	 *
	 * @param query the {@link QueryBuilder}
	 */
	protected abstract void buildJoins(final QueryBuilder query);

	/**
	 * Returns the SQL fragment for the provided {@code field}.
	 *
	 * @param field the field
	 * @return the field query string
	 */
	protected abstract String getFieldQueryFragment(final F field);

	@SuppressWarnings("unchecked")
	@Override
	public B aggregate(final F field, final AggregationOperator operator) {
		aggregations.put(field, operator);
		return (B) this;
	}

	@SuppressWarnings("unchecked")
	@Override
	public B groupBy(final F field) {
		groupByFields.add(field);
		groupBys.accept(q -> q.append(field.name().toLowerCase()));
		return (B) this;
	}

	@SuppressWarnings("unchecked")
	@Override
	public B orderBy(final F field) {
		orderBys.accept(q -> q.append(field.name().toLowerCase()));
		return (B) this;
	}

	@SuppressWarnings("unchecked")
	@Override
	public B distinct() {
		this.distinct = true;
		return (B) this;
	}
	
	/**
	 * Method to build the outer select query for aggregating values for a given FieldName.
	 *
	 * @param query The query builder
	 */
	private void buildOuterFields(final QueryBuilder query) {
		if ( ! groupByFields.isEmpty()) {
			final var it = groupByFields.iterator();
			while (it.hasNext()) {
				query.append(it.next().name());
				if (it.hasNext()) {
					query.append(", ");
				}
			}
			if ( ! aggregations.isEmpty()) {
				query.append(", ");
			}
		}

		if ( ! aggregations.isEmpty()) {
			final var it = aggregations.entrySet().iterator();
			while (it.hasNext()) {
				final var entry = it.next();
				addOuterField(query, entry.getKey(), entry.getValue());
				if (it.hasNext()) {
					query.append(", ");
				}
			}
		}
	}

	/**
	 * Helper method to build the aggregation function for a given FieldName and AggregationOperator.
	 *
	 * @param query The query builder
	 * @param field The FieldName
	 * @param operator The AggregationOperator
	 */
	private void addOuterField(final QueryBuilder query, final F field, final AggregationOperator operator) {
		switch (operator) {
			case AVG:
				query.append("AVG(");
				break;
			case COUNT:
				query.append("COUNT(");
				break;
			case COUNT_DISTINCT:
				query.append("COUNT(DISTINCT ");
				break;
			case MIN:
				query.append("MIN(");
				break;
			case MAX:
				query.append("MAX(");
				break;
			case SUM:
				query.append("SUM(");
				break;
			case LIST:
				query.append("ARRAY_AGG(");
				break;
			default:
				throw new UnsupportedOperationException("The aggregation function is not supported yet: " + operator);
		}

		query.append(field.name())
			 .append(") AS ")
			 .append(field.name()).toString();
	}

	/**
	 * Method to build the inner select query for aggregating values for a given FieldName.
	 *
	 * @param query The query builder
	 */
	private void buildInnerFields(final PgDao.QueryBuilder query) {
		if ( ! groupByFields.isEmpty()) {
			final var it = groupByFields.iterator();
			while (it.hasNext()) {
				query.append(getFieldQueryFragment(it.next()));
				if (it.hasNext()) {
					query.append(", ");
				}
			}
			if ( ! aggregations.isEmpty()) {
				query.append(", ");
			}
		}

		if ( ! aggregations.isEmpty()) {
			final var it = aggregations.keySet().iterator();
			while (it.hasNext()) {
				query.append(getFieldQueryFragment(it.next()));
				if (it.hasNext()) {
					query.append(", ");
				}
			}
		}
	}
}
