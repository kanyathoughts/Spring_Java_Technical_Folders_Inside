/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.access.postgres;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.springframework.jdbc.core.JdbcTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FieldInfoService.FieldInfoInquiryBuilder;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.entities.FieldInfoPojo;
import innowake.mining.shared.entities.FieldInfoPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Postgres specific access methods for entities such as {@code field_info} that are used by the database schema import.
 */
public class FieldInfoPgDao extends PgDao {

	public class FieldInfoQueryBuilder implements FieldInfoInquiryBuilder {

		private final FilterStreamBuilder filters = new FilterStreamBuilder();

		protected Paged.Builder<FieldInfoPojo> build(@Nullable final Pagination paging) {
			return query("SELECT id,"																							/* 1 */
						+ "(SELECT to_jsonb(module_ids) FROM (SELECT uid, nid from module WHERE uid = module) module_ids),"		/* 2 */
						+ "ordinal,"																							/* 3 */
						+ "name,"																								/* 4 */
						+ "reference,"																							/* 5 */
						+ "comment,"																							/* 6 */
						+ "properties "																							/* 7 */
						+ "FROM field_info")
					.with(filters::build)
					.toPageable(paging,
							(rs, row) -> new FieldInfoPojo((UUID) rs.getObject(1),												/* id */
															PgJSON.fromPGobject(rs.getObject(2), EntityId.class), null, null,	/* module uid and nid */
															rs.getInt(3),														/* ordinal */
															rs.getString(4),													/* name */
															rs.getString(5),													/* reference */
															rs.getString(6),													/* comment */
															PgJSON.fromPGobject(rs.getObject(7))));								/* properties */
		}

		protected int buildDelete() {
			return query("DELETE FROM field_info")
					.with(filters::build)
					.update();
		}

		@Override
		public FieldInfoQueryBuilder ofProject(final EntityId project) {
			filters.accept(q -> q.append("module IN (SELECT uid FROM module WHERE project = ").with(ProjectPgDao.referenceUidOrNid(project)).append(")"));
			return this;
		}

		@Override
		public FieldInfoQueryBuilder ofModule(final EntityId module) {
			filters.accept(q -> q.append("module = ").with(ModulePgDao.referenceUidOrNid(module)));
			return this;
		}
		
		@Override
		public FieldInfoQueryBuilder withOrdinal(final Integer ordinal) {
			filters.accept(q -> q.append("ordinal = ?", ordinal));
			return this;
		}
	}

	/**
	 * Creates a new data access for Postgres.
	 * @param jdbcTemplate Access to the Postgres database.
	 */
	public FieldInfoPgDao(final JdbcTemplate jdbcTemplate) {
		super(jdbcTemplate);
	}

	/**
	 * Creates a new {@code field_info} entity or updates the existing one.
	 *
	 * @param fieldInfo the {@link FieldInfoPojoPrototype} to create
	 * @param isNew {@code true} to insert a new entity, {@code false} to update an existing one
	 * @return the {@link UUID} of the created or updated {@code fieldInfo}.
	 */
	public UUID put(final FieldInfoPojoPrototype fieldInfo, final boolean isNew) {
		final var fields = new FieldBuilder();
		fields.add(fieldInfo.module.required(isNew), "module", ModulePgDao::referenceUidOrNid);
		fields.add(fieldInfo.name.required(isNew), "name", "?");
		fields.add(fieldInfo.ordinal.required(isNew), "ordinal", "?");
		fields.add(fieldInfo.reference, "reference", "?");
		fields.add(fieldInfo.comment, "comment", "?");
		fields.add(fieldInfo.properties, "properties", "?", PgJSON::toPGobject);

		final QueryBuilder query;
		if (isNew) {
			fields.add("id", "?", fieldInfo.id.orElseNonNull(UUID::randomUUID));
			query = query("INSERT INTO field_info ")
						.with(fields::buildInsert);
		} else {
			final var id = fieldInfo.id.getNonNull();
			query = query("UPDATE field_info SET ")
						.with(fields::buildUpdate)
						.append(" WHERE id = ?", id);
		}

		return query.append(" RETURNING id")
				.first(rs -> (UUID) rs.getObject(1))
				.orElseThrow(() -> new MiningEntityNotFoundException(FieldInfoPojo.class, fieldInfo.toString()));
	}

	/**
	 * Deletes all {@code field_info} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain FieldInfoInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of deleted {@code field_info} entities
	 */
	public int delete(final BuildingConsumer<FieldInfoInquiryBuilder> builder) {
		return builder.prepare(new FieldInfoQueryBuilder())
						.buildDelete();
	}

	/**
	 * Returns all {@code field_info} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain FieldInfoInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain FieldInfoPojo FieldInfoPojos}
	 */
	public List<FieldInfoPojo> find(final BuildingConsumer<FieldInfoInquiryBuilder> builder) {
		return builder.prepare(new FieldInfoQueryBuilder())
						.build(null)
						.all();
	}

	/**
	 * Returns all {@code field_info} entities that match with the filters in the given {@code builder}.
	 * @param paging the paging to apply to the result, or null
	 *
	 * @param builder the {@linkplain FieldInfoInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain FieldInfoPojo FieldInfoPojos}
	 */
	public Paged<FieldInfoPojo> find(@Nullable final Pagination paging, final BuildingConsumer<FieldInfoInquiryBuilder> builder) {
		return builder.prepare(new FieldInfoQueryBuilder())
						.build(paging)
						.page();
	}

	/**
	 * Returns the first {@code field_info} entity that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain FieldInfoInquiryBuilder} containing the filter criteria and sort options.
	 * @return first matching {@linkplain FieldInfoPojo}
	 */
	public Optional<FieldInfoPojo> findAny(final BuildingConsumer<FieldInfoInquiryBuilder> builder) {
		return builder.prepare(new FieldInfoQueryBuilder())
				.build(Pagination.FIRST)
				.first();
	}
}
