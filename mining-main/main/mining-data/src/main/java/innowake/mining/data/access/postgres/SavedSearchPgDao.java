/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.access.postgres;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springframework.jdbc.core.JdbcTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SavedSearchService.SavedSearchInquiryBuilder;
import innowake.mining.shared.entities.SavedSearchPojo;
import innowake.mining.shared.entities.SavedSearchPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.ScopeEnum;

/**
 * Postgres specific access methods for the {@code saved_search} entity.
 */
public class SavedSearchPgDao extends PgDao {

	public class SavedSearchQueryBuilder implements SavedSearchInquiryBuilder {

		private final FilterStreamBuilder filters = new FilterStreamBuilder();
		
		protected Paged.Builder<SavedSearchPojo> build(@Nullable final Pagination paging) {
			return query("SELECT id,"																												/* 1 */
						+ "(SELECT to_jsonb(client_ids) FROM (SELECT uid, nid from client WHERE uid = saved_search.client) client_ids),"			/* 2 */
						+ "(SELECT to_jsonb(project_ids) FROM (SELECT uid, nid FROM project WHERE uid = project) project_ids),"						/* 3 */
						+ "name,"																													/* 4 */
						+ "saved_search,"																											/* 5 */
						+ "scope,"																													/* 6 */
						+ "usage,"																													/* 7 */
						+ "modifiers,"																												/* 8 */
						+ "created_by "																												/* 9 */
						+ "FROM saved_search")
					.with(filters::build)
					.toPageable(paging,
							(rs, row) -> new SavedSearchPojo(Long.valueOf(rs.getLong(1)),															/* id */
															 PgJSON.fromPGobjectOrNull(rs.getObject(2), EntityId.class), null, null,				/* client uid and nid */
															 PgJSON.fromPGobjectOrNull(rs.getObject(3), EntityId.class), null, null,				/* project uid and nid */
															 rs.getString(4),																		/* name */
															 rs.getString(5),																		/* savedSearch */
															 ScopeEnum.valueOf(rs.getString(6)),													/* scope */
															 rs.getString(7),																		/* usage */
															 PgUtil.<String>streamArray(rs.getArray(8)).collect(Collectors.toUnmodifiableList()),	/* modifiers (List<String>) */
															 rs.getString(9)));																		/* created_by */
		}

		protected int buildDelete() {
			return query("DELETE FROM saved_search")
					.with(filters::build)
					.update();
		}

		@Override
		public SavedSearchQueryBuilder byId(final Long id) {
			filters.accept(q -> q.append("id = ?", id));
			return this;
		}

		@Override
		public SavedSearchQueryBuilder ofProject(@Nullable final EntityId project) {
			if (project == null) {
				filters.accept(q -> q.append("project IS Null"));
			} else {
				filters.accept(q -> q.append("project = ").with(ProjectPgDao.referenceUidOrNid(project)));
			}
			return this;
		}

		@Override
		public SavedSearchQueryBuilder ofClient(@Nullable final EntityId client) {
			if (client == null) {
				filters.accept(q -> q.append("client IS Null"));
			} else {
				filters.accept(q -> q.append("client = ").with(ClientPgDao.referenceUidOrNid(client)));
			}
			return this;
		}

		@Override
		public SavedSearchQueryBuilder withName(final String name) {
			filters.accept(q -> q.append("name = ?", name));
			return this;
		}

		@Override
		public SavedSearchQueryBuilder withNames(final Collection<String> names) {
			filters.accept(q -> q.append("name = any(?)").addArg(PgType.STRING, names));
			return this;
		}

		@Override
		public SavedSearchQueryBuilder withUsage(final String usage) {
			filters.accept(q -> q.append("usage = ?", usage));
			return this;
		}

		@Override
		public SavedSearchQueryBuilder withScope(final ScopeEnum scope) {
			filters.accept(q -> q.append("scope = ?", scope.name()));
			return this;
		}

		@Override
		public SavedSearchQueryBuilder withCreatedByUserId(final String createdByUserId) {
			filters.accept(q -> q.append("created_by = ?", createdByUserId));
			return this;
		}
	}

	/**
	 * Creates a new {@code saved_search} data access for Postgres.
	 * @param jdbcTemplate Access to the Postgres database.
	 */
	public SavedSearchPgDao(final JdbcTemplate jdbcTemplate) {
		super(jdbcTemplate);
	}

	/**
	 * Creates a new {@code saved_search} entity or updates the existing one.
	 *
	 * @param savedSearch the {@link SavedSearchPojoPrototype} to create
	 * @param isNew {@code true} to insert a new entity, {@code false} to update an existing one
	 * @return the id of the created or updated {@code savedSearch}.
	 */
	public Long put(final SavedSearchPojoPrototype savedSearch, final boolean isNew) {
		final var fields = new FieldBuilder();
		fields.add(savedSearch.client, "client", ClientPgDao::referenceUidOrNid);
		fields.add(savedSearch.project, "project", ProjectPgDao::referenceUidOrNid);
		fields.add(savedSearch.name.required(isNew), "name", "?");
		fields.add(savedSearch.savedSearch.required(isNew), "saved_search", "?");
		fields.add(savedSearch.scope.required(isNew), "scope", "?", ScopeEnum::name);
		fields.add(savedSearch.usage.required(isNew), "usage", "?");
		fields.add(savedSearch.modifiers, "modifiers", PgType.STRING, isNew);
		fields.add(savedSearch.createdByUserId, "created_by", "?");

		final QueryBuilder q;
		if (isNew) {
			q = query("INSERT INTO saved_search ")
					.with(fields::buildInsert);
		} else {
			q = query("UPDATE saved_search SET ")
					.with(fields::buildUpdate)
					.append(" WHERE id = ?", savedSearch.id.getNonNull());
		}

		return q.append(" RETURNING id")
				.first(rs -> Long.valueOf(rs.getLong(1)))
				.orElseThrow(() -> new MiningEntityNotFoundException(SavedSearchPojo.class, savedSearch.toString()));
	}

	/**
	 * Deletes all {@code saved_search} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain SavedSearchInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of deleted {@code saved_search} entities
	 */
	public int delete(final BuildingConsumer<SavedSearchInquiryBuilder> builder) {
		return builder.prepare(new SavedSearchQueryBuilder())
						.buildDelete();
	}

	/**
	 * Returns all {@code saved_search} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain SavedSearchInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain SavedSearchPojo SavedSearchPojos}
	 */
	public List<SavedSearchPojo> find(final BuildingConsumer<SavedSearchInquiryBuilder> builder) {
		return builder.prepare(new SavedSearchQueryBuilder())
						.build(null)
						.all();
	}

	/**
	 * Returns all {@code saved_search} entities that match with the filters in the given {@code builder}.
	 * @param paging the paging to apply to the result, or null
	 *
	 * @param builder the {@linkplain SavedSearchInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain SavedSearchPojo SavedSearchPojos}
	 */
	public Paged<SavedSearchPojo> find(@Nullable final Pagination paging, final BuildingConsumer<SavedSearchInquiryBuilder> builder) {
		return builder.prepare(new SavedSearchQueryBuilder())
						.build(paging)
						.page();
	}

	/**
	 * Returns the first {@code saved_search} entity that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain SavedSearchInquiryBuilder} containing the filter criteria and sort options.
	 * @return first matching {@linkplain SavedSearchPojo}
	 */
	public Optional<SavedSearchPojo> findAny(final BuildingConsumer<SavedSearchInquiryBuilder> builder) {
		return builder.prepare(new SavedSearchQueryBuilder())
				.build(Pagination.FIRST)
				.first();
	}

	/**
	 * Returns all the {@link SavedSearchPojo SavedSearchPojos} by the provided project, usage and createdByUserId.
	 *
	 * @param clientId the client id of the {@link SavedSearchPojo}
	 * @param projectId the project id of the {@link SavedSearchPojo}
	 * @param usage of {@link SavedSearchPojo}
	 * @param createdByUserId of {@link SavedSearchPojo}
	 * @return List of {@link SavedSearchPojo}
	 */
	public List<SavedSearchPojo> findByUsageAndUserId(final EntityId clientId, final EntityId projectId, final String usage, final String createdByUserId) {
		final SavedSearchQueryBuilder builder = new SavedSearchQueryBuilder()
														.withUsage(usage);
		builder.filters.accept(q -> {
			q.append("(scope = ? AND project = (SELECT uid FROM project WHERE nid = ?)", ScopeEnum.GLOBAL.name(), 0);
			q.append(" OR scope = ? AND created_by = ? AND project = ", ScopeEnum.INDIVIDUAL.name(), createdByUserId).with(ProjectPgDao.referenceUidOrNid(projectId));
			q.append(" OR scope = ? AND project = ", ScopeEnum.PROJECT.name()).with(ProjectPgDao.referenceUidOrNid(projectId));
			q.append(" OR scope = ? AND client = ", ScopeEnum.CLIENT.name()).with(ClientPgDao.referenceUidOrNid(clientId));
			q.append(")");
		});

		return builder.build(null)
				.all();
	}
}
