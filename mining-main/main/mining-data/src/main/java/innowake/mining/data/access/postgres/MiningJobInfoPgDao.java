/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.data.access.postgres;

import java.util.Collection;
import java.util.List;
import java.util.UUID;

import org.springframework.jdbc.core.JdbcTemplate;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.MiningJobInfoService;
import innowake.mining.shared.entities.MiningJobInfoPojo;
import innowake.mining.shared.entities.MiningJobInfoPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Postgres specific access methods for the {@code mining_job_info} entity.
 */
public class MiningJobInfoPgDao extends PgDao {
	
	/**
	 * Creates a new  {@code mining_job_info} entity access for Postgres.
	 * @param jdbcTemplate Access to the Postgres database.
	 */
	public MiningJobInfoPgDao(final JdbcTemplate jdbcTemplate) {
		super(jdbcTemplate);
	}
	
	public class MiningJobInfoQueryBuilder implements MiningJobInfoService.MiningJobInfoInquiryBuilder {
		protected final FilterStreamBuilder filters = new FilterStreamBuilder();
		
		protected List<MiningJobInfoPojo> buildSelect() {
			return query("SELECT mji.job_id,"
					+ " (SELECT to_jsonb(project_ids) FROM (SELECT uid, nid FROM project WHERE uid = mji.project) project_ids),"
					+ " (SELECT to_jsonb(module_ids) FROM (SELECT uid, nid FROM module WHERE uid = mji.module) module_ids) "
					+ "FROM mining_job_info mji ")
				.with(filters::build)
				.toList((rs, n) -> new MiningJobInfoPojo(
						PgJSON.fromPGobjectOrNull(rs.getObject(2), EntityId.class),
						PgJSON.fromPGobjectOrNull(rs.getObject(3), EntityId.class),
						(UUID) rs.getObject(1)
				));
		}
		
		protected List<UUID> buildJobIds() {
			return query("SELECT mji.job_id "
					+ "FROM mining_job_info mji ")
				.with(filters::build)
				.toList((rs, n) -> (UUID) rs.getObject(1));
		}
		
		protected int buildDelete() {
			return query("DELETE FROM mining_job_info mji ")
					.with(filters::build)
					.update();
		}
		
		protected Long buildCount() {
			return query("SELECT count(*) FROM mining_job_info mji ")
					.with(filters::build)
					.first(rs -> rs.getLong(1)).orElse(0l);
		}

		@Override
		public MiningJobInfoQueryBuilder byJobId(final UUID jobId) {
			filters.accept(q -> q.append("mji.job_id = ?", jobId));
			return this;
		}

		@Override
		public MiningJobInfoQueryBuilder byJobIds(final Collection<UUID> jobIds) {
			filters.accept(q -> q.append("mji.job_id = any(?)").addArg(PgType.UUID, jobIds));
			return this;
		}

		@Override
		public MiningJobInfoQueryBuilder ofProject(final EntityId project) {
			filters.accept(q -> q.append("mji.project = ").with(ProjectPgDao.referenceUidOrNid(project)));
			return this;
		}

		@Override
		public MiningJobInfoQueryBuilder ofModule(final EntityId module) {
			filters.accept(q -> q.append("mji.module = ").with(ModulePgDao.referenceUidOrNid(module)));
			return this;
		}

		@Override
		public MiningJobInfoQueryBuilder ofModules(final Collection<UUID> modules) {
			filters.accept(q -> q.append("mji.module = any(?)").addArg(PgType.UUID, modules));
			return this;
		}
	}

	/**
	 * Counts the number of Job Information based on the fuilter builder
	 *
	 * @param builder the filter builder
	 * @return the count
	 */
	public Long count(final BuildingConsumer<MiningJobInfoService.MiningJobInfoInquiryBuilder> builder) {
		return builder.prepare(new MiningJobInfoQueryBuilder()).buildCount();
	}
	
	/**
	 * Finds {@linkplain MiningJobInfoPojo} based on provided filters
	 * @param builder filter builder
	 * @return List of {@linkplain MiningJobInfoPojo}
	 */
	public List<MiningJobInfoPojo> find(final BuildingConsumer<MiningJobInfoService.MiningJobInfoInquiryBuilder> builder) {
		return builder.prepare(new MiningJobInfoQueryBuilder()).buildSelect();
	}
	
	/**
	 * Finds {@linkplain MiningJobInfoPojo} based on provided filters
	 * @param builder filter builder
	 * @return List of {@linkplain MiningJobInfoPojo}
	 */
	public List<UUID> findJobId(final BuildingConsumer<MiningJobInfoService.MiningJobInfoInquiryBuilder> builder) {
		return builder.prepare(new MiningJobInfoQueryBuilder()).buildJobIds();
	}
	
	/**
	 * Creates a new {@linkplain MiningJobInfoPojo} based on the prototype
	 * @param prototype the prototype
	 */
	public void create(final MiningJobInfoPojoPrototype prototype) {
		final QueryBuilder query = query("INSERT INTO mining_job_info ");
		new FieldBuilder()
				.add(prototype.module, "module", ModulePgDao :: referenceUidOrNid)
				.add(prototype.project, "project", ProjectPgDao :: referenceUidOrNid)
				.add(prototype.jobId.required(true), "job_id", "?")
				.buildInsert(query);
		query.update();
	}
	
	/**
	 * Deletes {@linkplain MiningJobInfoPojo}s based on provided filters
	 * @param builder filter builder
	 * @return Number of {@linkplain MiningJobInfoPojo}s deleted
	 */
	public int delete(final BuildingConsumer<MiningJobInfoService.MiningJobInfoInquiryBuilder> builder) {
		return builder.prepare(new MiningJobInfoQueryBuilder()).buildDelete();
	}
}
