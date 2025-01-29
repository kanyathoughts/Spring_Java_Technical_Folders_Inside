/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.access.postgres;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Stream;

import innowake.mining.shared.model.AstNodeLocation;
import org.springframework.jdbc.core.JdbcTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService.ErrorMarkerInquiryBuilder;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.Table;
import innowake.mining.shared.entities.ErrorMarkerPojo;
import innowake.mining.shared.entities.ErrorMarkerPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.Severity;

/**
 * Postgres specific access methods for the {@code error_marker} entity.
 */
public class ErrorMarkerPgDao extends PgDao {

	public class ErrorMarkerQueryBuilder implements ErrorMarkerInquiryBuilder {

		protected final FilterStreamBuilder filters = new FilterStreamBuilder();

		protected Paged.Builder<ErrorMarkerPojo> build(@Nullable final Pagination paging) {
			return query("SELECT project, module, severity, key, cause, to_jsonb(location), line")
					.append(" FROM error_marker ")
					.with(filters::build)
					.toPageable(paging,
							(rs, row) -> new ErrorMarkerPojo(EntityId.of((UUID) rs.getObject(1)), null, null,	/* project ids */
															 EntityId.of((UUID) rs.getObject(2)), null, null,	/* module ids */
															 rs.getString(3) == null ? null : Severity.fromString(rs.getString(3)),
															 rs.getString(4) == null ? null : ErrorKey.fromString(rs.getString(4)),
															 rs.getString(5),
															 PgJSON.fromPGobjectOrNull(rs.getObject(6), AstNodeLocation.class),
															 Integer.valueOf(rs.getInt(7))));
		}

		protected Map<UUID, Long> buildCountGroupByModule() {
			return query("SELECT module, count(*) FROM error_marker ")
					.with(filters::build)
					.append(" GROUP BY module")
					.toMap((rs, m) -> m.put((UUID) rs.getObject(1), rs.getLong(2)));
		}

		protected QueryBuilder buildCount() {
			return query("SELECT count(*) FROM error_marker ").with(filters::build);
		}

		protected QueryBuilder buildDelete() {
			return query("DELETE FROM error_marker ").with(filters::build);
		}

		@Override
		public ErrorMarkerQueryBuilder ofProject(final EntityId project) {
			filters.accept(q -> q.append("project = ").with(ProjectPgDao.referenceUidOrNid(project)));
			return this;
		}

		@Override
		public ErrorMarkerQueryBuilder ofModule(final EntityId module) {
			filters.accept(q -> q.append("module = ").with(ModulePgDao.referenceUidOrNid(module)));
			return this;
		}

		@Override
		public ErrorMarkerInquiryBuilder ofModules(final Collection<EntityId> modules) {
			filters.accept(q -> q.appendIds(modules, "module = ANY(?)", "module IN (SELECT uid FROM module WHERE nid = ANY(?))"));
			return this;
		}

		@Override
		public ErrorMarkerQueryBuilder withKey(final ErrorKey key) {
			filters.accept(q -> q.append("key = ?", key.name()));
			return this;
		}
	}

	/**
	 * Creates a new {@code error_marker} data access for Postgres.
	 * @param jdbcTemplate Access to the Postgres database.
	 */
	public ErrorMarkerPgDao(final JdbcTemplate jdbcTemplate) {
		super(jdbcTemplate);
	}

	/**
	 * Creates a new {@code error_marker} entity.
	 *
	 * @param errorMarker the {@link ErrorMarkerPojoPrototype} to create
	 */
	public void create(final ErrorMarkerPojoPrototype errorMarker) {
		final boolean requiresProject = ! errorMarker.module.isPresent();
		final var fields = new FieldBuilder()
				.add(errorMarker.project.required(requiresProject), "project", ProjectPgDao::referenceUidOrNid)
				.add(errorMarker.severity.required(true), "severity", "?", Severity::name)
				.add(errorMarker.key.required(true), "key", "?", ErrorKey::name)
				.add(errorMarker.cause.required(true), "cause", "?")
				.add(errorMarker.location, "location", ModulePgDao::appendAstNodeLocation)
				.add(errorMarker.line, "line", "?");

		if (errorMarker.module.isPresent()) {
			fields.add(errorMarker.module, "module", ModulePgDao::referenceUidOrNid);
		}

		query("INSERT INTO error_marker ")
				.with(fields::buildInsert)
				.update();
	}

	/**
	 * Creates new {@code error_marker} entity for each {@link ErrorMarkerPojoPrototype} in {@code code errorMarkers}.
	 *
	 * @param errorMarkers the {@link ErrorMarkerPojoPrototype ErrorMarkerPojoPrototypes} to create
	 */
	public void create(final Collection<ErrorMarkerPojoPrototype> errorMarkers) {
		query("INSERT INTO error_marker(project, module, severity, key, cause, location, line) "
				+ "VALUES (coalesce(?, (SELECT uid FROM project WHERE nid = ?)), coalesce(?, (SELECT uid FROM module WHERE nid = ?)),"
				+ " ?, ?, ?, jsonb_populate_record(null::ast_node_location, ?), ?)")
			.updateBatch(errorMarkers.stream().map(errorMarker -> Stream.<Object>of(
					errorMarker.project.getNonNull().getUidOptional().orElse(null),
					errorMarker.project.getNonNull().getNidOptional().orElse(null),
					errorMarker.module.optional().flatMap(EntityId::getUidOptional).orElse(null),
					errorMarker.module.optional().flatMap(EntityId::getNidOptional).orElse(null),
					errorMarker.severity.optional().map(Severity::name).orElse(null),
					errorMarker.key.optional().map(ErrorKey::name).orElse(null),
					errorMarker.cause.orElse(null),
					PgJSON.toPGobjectConvertToSnakeCase(errorMarker.location.orElse(null)),
					errorMarker.line.orElse(null)
				)), 1_000);
	}

	/**
	 * Returns all {@code error_marker} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ErrorMarkerInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain ErrorMarkerPojo ErrorMarkerPojos}
	 */
	public List<ErrorMarkerPojo> find(final BuildingConsumer<ErrorMarkerInquiryBuilder> builder) {
		return builder.prepare(new ErrorMarkerQueryBuilder())
					.build(null)
					.all();
	}

	/**
	 * Returns a page of {@code error_marker} entities that match with the filters in the given {@code builder}.
	 *
	 * @param paging the {@linkplain Pagination} object containing the page number and size
	 * @param builder the {@linkplain ErrorMarkerInquiryBuilder} containing the filter criteria and sort options.
	 * @return page of {@linkplain ErrorMarkerPojo ErrorMarkerPojos}
	 */
	public Paged<ErrorMarkerPojo> find(final Pagination paging, final BuildingConsumer<ErrorMarkerInquiryBuilder> builder) {
		return builder.prepare(new ErrorMarkerQueryBuilder())
				.build(paging)
				.page();
	}

	/**
	 * Returns the number of {@code error_marker} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ErrorMarkerInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of matching {@code error_marker} entities
	 */
	public long count(final BuildingConsumer<ErrorMarkerInquiryBuilder> builder) {
		return builder.prepare(new ErrorMarkerQueryBuilder())
					.buildCount()
					.first(rs -> Long.valueOf(rs.getLong(1)))
					.orElse(Long.valueOf(0l))
					.longValue();
	}

	/**
	 * Returns a map of module id with the number of {@code error_marker} entities for that module matching with the filters in the given {@code builder}.
	 *
	 * @param builder
	 * 		the {@linkplain ErrorMarkerInquiryBuilder} containing the filter criteria and sort options.
	 * @return Map of module id with corresponding number of matching {@code error_marker} entities
	 */
	public Map<UUID, Long> countByModule(final BuildingConsumer<ErrorMarkerInquiryBuilder> builder) {
		return builder.prepare(new ErrorMarkerQueryBuilder())
				.buildCountGroupByModule();
	}

	/**
	 * Deletes all {@code error_marker} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ErrorMarkerInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of deleted {@code error_marker} entities
	 */
	public int deleteErrorMarkers(final BuildingConsumer<ErrorMarkerInquiryBuilder> builder) {
		return builder.prepare(new ErrorMarkerQueryBuilder())
				.buildDelete()
				.update();
	}

	/**
	 * Returns a container with a list of values of all {@code module} and {@code error_marker} entities of the given {@code project} for the discovery exporter.
	 *
	 * @param project the id of the project for which the data export is done
	 * @param sorted {@code true} if the values must be sorted. Otherwise {@code false}
	 * @return container with list of values
	 */
	public Optional<Table> getModuleErrorMarkerExport(final EntityId project, final boolean sorted) {
		final var query = query("SELECT m.nid, m.name, e.severity, e.key, e.cause, CASE WHEN e.location IS Null THEN -1 ELSE (e.location).assembled_offset " +
				"END, " + "CASE WHEN e.location IS Null THEN -1 ELSE (e.location).assembled_length END FROM error_marker e INNER JOIN module m ON e.module = m.uid " +
				"WHERE e.project=")
							.with(ProjectPgDao.referenceUidOrNid(project));
		if (sorted) {
			query.append(" ORDER BY m.name, m.path");
		}

		return Optional.ofNullable(query.build(TableBuilder::build));
	}
}
