/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.access.postgres;

import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import javax.persistence.PersistenceException;

import org.springframework.jdbc.core.JdbcTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EffortSummaryService.EffortSummaryInquiryBuilder;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.entities.EffortSummaryPojo;
import innowake.mining.shared.entities.EffortSummaryPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.EffortSummaryType;

/**
 * Postgres specific access methods for the {@code effort_summary} entity.
 */
public class EffortSummaryPgDao extends PgDao {

	public class EffortSummaryQueryBuilder implements EffortSummaryInquiryBuilder {

		private final FilterStreamBuilder filters = new FilterStreamBuilder();
		private final OrderStreamBuilder orders = new OrderStreamBuilder();
		
		protected Paged.Builder<EffortSummaryPojo> build(@Nullable final Pagination paging) {
			return query("SELECT (SELECT to_jsonb(pids) FROM (SELECT uid, nid FROM project WHERE uid = es.project) pids) as projectIds,"	/* 1 */
						+ "index,"																											/* 2 */
						+ "type,"																											/* 3 */
						+ "properties "																										/* 4 */
						+ "FROM effort_summary es")
					.with(filters::build)
					.toPageable(paging,
							(rs, row) -> new EffortSummaryPojo(PgJSON.fromPGobject(rs.getObject(1), EntityId.class),				/* project */
																rs.getLong(2),														/* id */
																EffortSummaryType.valueOf(rs.getString(3)),							/* type */
																PgJSON.fromPGobject(rs.getObject(4))));								/* properties */
		}

		protected int buildDelete() {
			return query("DELETE FROM effort_summary")
					.with(filters::build)
					.update();
		}

		@Override
		public EffortSummaryQueryBuilder ofProject(final EntityId project) {
			filters.accept(q -> q.append("project = ").with(ProjectPgDao.referenceUidOrNid(project)));
			return this;
		}

		@Override
		public EffortSummaryQueryBuilder byType(final EffortSummaryType type) {
			filters.accept(q -> q.append("type = ?::effort_summary_type", type.name()));
			return this;
		}

		@Override
		public EffortSummaryQueryBuilder byIndex(final Long index) {
			filters.accept(q -> q.append("index = ?", index));
			return this;
		}

		@Override
		public EffortSummaryInquiryBuilder sortIndex(final SortDirection sortDirection) {
			orders.accept(q -> q.appendOrder("index", sortDirection));
			return this;
		}

	}

	public EffortSummaryPgDao(final JdbcTemplate jdbcTemplate) {
		super(jdbcTemplate);
	}

	/**
	 * Creates a new {@code effort_summary} entity or updates the existing one. The update method allows you only to change the properties of the entity.
	 *
	 * @param effortSummary the {@linkplain EffortSummaryPojoPrototype} to create.
	 * @param isNew {@code true} to create a new {@code effort_summary} entity. {@code false} to update an existing one
	 * @return the index of the created or updated {@code effort_summary} entity or -1 if nothing was updated
	 */
	public long put(final EffortSummaryPojoPrototype effortSummary, final boolean isNew) {
		final QueryBuilder query;
		if (isNew) {
			final var fields = new FieldBuilder();
			fields.add(effortSummary.project, "project", ProjectPgDao::referenceUidOrNid);
			if (effortSummary.index.isPresent()) {
				fields.add(effortSummary.index, "index", "?");
			} else {
				fields.add("index", q -> q.append("(SELECT COALESCE(max(index) + 1, 1) FROM effort_summary WHERE project = ")
											.with(ProjectPgDao.referenceUidOrNid(effortSummary.project.getNonNull()))
											.append(" AND type = ?::effort_summary_type)", effortSummary.type.getNonNull().name()));
			}

			fields.add(effortSummary.type, "type", "?::effort_summary_type", EffortSummaryType::name);
			fields.add(effortSummary.properties, "properties", "?", PgJSON::toPGobject);
			query = query("INSERT INTO effort_summary ")
						.with(fields::buildInsert);
		} else {
			query = query("UPDATE effort_summary SET properties = ? WHERE index = ? AND type = ?::effort_summary_type AND project = ")
					.addArg(PgJSON.toPGobject(effortSummary.properties.getNonNull()))
					.addArg(effortSummary.index.getNonNull())
					.addArg(effortSummary.type.getNonNull().name())
					.with(ProjectPgDao.referenceUidOrNid(effortSummary.project.getNonNull()));
		}

		return query.append(" RETURNING index")
				.first(rs -> rs.getLong(1))
				.orElseThrow(() -> new PersistenceException("Failed to " + (isNew ? "create" : "update") + " effort_summary: '" + effortSummary));
	}

	/**
	 * Creates a new {@code effort_summary} entity for each {@linkplain EffortSummaryPojoPrototype} in {@code effortSummaries}.
	 *
	 * @param effortSummaries the {@linkplain EffortSummaryPojoPrototype EffortSummaryPojoPrototypes} to create.
	 * @return number of created {@code effort_summary} entities
	 */
	public int createBatch(final List<EffortSummaryPojoPrototype> effortSummaries) {
		final var batchArgs = effortSummaries.stream()
				.map(effortSummary -> Stream.<Object>of(
						effortSummary.project.getNonNull().getUidOptional().orElse(null),	/* 1 'project': project uid, can be null */
						effortSummary.project.getNonNull().getNidOptional().orElse(null),	/* 2 'project': project nid, can be null */
						effortSummary.index.orElse(null),									/* 3 'index': set index, can be null */
						effortSummary.project.getNonNull().getUidOptional().orElse(null),	/* 4 'index': project uid for calculating next 'index', can be null */
						effortSummary.project.getNonNull().getNidOptional().orElse(null),	/* 5 'index': project nid for calculating next 'index', can be null */
						effortSummary.type.getNonNull().name(),								/* 6 'index': type for calculating next 'index' */
						effortSummary.type.getNonNull().name(),								/* 7 'type' */
						PgJSON.toPGobject(effortSummary.properties.getNonNull())			/* 8 'properties' */
				));

		/* TODO npr: inefficient. maybe the query can fetch the starting ordinal only once in a CTE */
		query("INSERT INTO effort_summary VALUES ("
				/* project where uid or nid can be null, but not both */
				+ "COALESCE(?, (SELECT uid FROM project WHERE nid = ?)), "
				/* index, if present - OR - calculate next index for project + type, min is 1 */
				+ "(SELECT COALESCE(?, (SELECT COALESCE(max(index) + 1, 1) FROM effort_summary "
									 + "WHERE project = COALESCE(?, (SELECT uid FROM project WHERE nid = ?)) AND type = ?::effort_summary_type))), "
				/* type, properties */
				 + "?::effort_summary_type, ?)")
			.updateBatch(batchArgs, 1000);

		return effortSummaries.size();
	}

	/**
	 * Returns the first {@code effort_summary} entitiy that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain EffortSummaryInquiryBuilder} containing the filter criteria and sort options.
	 * @return the {@linkplain EffortSummaryPojo}
	 */
	public Optional<EffortSummaryPojo> findAny(final BuildingConsumer<EffortSummaryInquiryBuilder> builder) {
		return builder.prepare(new EffortSummaryQueryBuilder())
				.build(Pagination.FIRST)
				.first();
	}

	/**
	 * Returns all {@code effort_summary} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain EffortSummaryInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain EffortSummaryPojo EffortSummaryPojos}
	 */
	public List<EffortSummaryPojo> find(final BuildingConsumer<EffortSummaryInquiryBuilder> builder) {
		return builder.prepare(new EffortSummaryQueryBuilder())
				.build(null)
				.all();
	}

	/**
	 * Returns paged subset of {@code effort_summary} entities that match with the filters in the given {@code builder}.
	 *
	 * @param paging Pagination specification.
	 * @param builder the {@linkplain EffortSummaryInquiryBuilder} containing the filter criteria and sort options.
	 * @return Paged subset of matching {@code effort_summary} entities.
	 */
	public Paged<EffortSummaryPojo> find(final Pagination paging, final BuildingConsumer<EffortSummaryInquiryBuilder> builder) {
		return builder.prepare(new EffortSummaryQueryBuilder())
					.build(paging)
					.page();
	}

	/**
	 * Deletes all {@code effort_summary} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain EffortSummaryInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of deleted {@code effort_summary} entities
	 */
	public int delete(final BuildingConsumer<EffortSummaryInquiryBuilder> builder) {
		return builder.prepare(new EffortSummaryQueryBuilder())
				.buildDelete();
	}

}
