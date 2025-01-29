/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.access.postgres;

import java.sql.Timestamp;
import java.time.Instant;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.stream.Stream;

import org.springframework.jdbc.core.JdbcTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.access.DnaDataService;
import innowake.mining.shared.access.DnaDataService.DnaCommunityInquiryBuilder;
import innowake.mining.shared.access.DnaDataService.DnaSnapshotInquiryBuilder;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.entities.dna.DnaClusterAlgorithm;
import innowake.mining.shared.entities.dna.DnaCommunityPojo;
import innowake.mining.shared.entities.dna.DnaCommunityPojoPrototype;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.mining.shared.entities.dna.DnaSimilarityAlgorithm;
import innowake.mining.shared.entities.dna.DnaSnapshotPojo;
import innowake.mining.shared.entities.dna.DnaSnapshotPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Postgres specific access methods for DNA related entities.
 */
public class DnaClusterPgDao extends PgDao {
	
	/**
	 * Creates a new DNA entity access for Postgres.
	 * @param jdbcTemplate Access to the Postgres database.
	 */
	public DnaClusterPgDao(final JdbcTemplate jdbcTemplate) {
		super(jdbcTemplate);
	}

	/**
	 * Query builder for performing queries on {@code dna_snapshot} entities.
	 */
	public class DnaSnapshotQueryBuilder implements DnaDataService.DnaSnapshotInquiryBuilder {
		protected final FilterStreamBuilder filters = new FilterStreamBuilder();
		protected final OrderStreamBuilder order = new OrderStreamBuilder();
		
		protected Paged.Builder<DnaSnapshotPojo> build(@Nullable final Pagination paging) {
			if (paging != null) {
				order.accept(q -> q.appendOrder("dna_snapshot.uid", SortDirection.ASCENDING));
			}
			return query("SELECT id, (SELECT to_jsonb(project_ids) FROM (SELECT uid, nid FROM project WHERE uid = dna_snapshot.project) project_ids),"
					+ " name, updated, module_count, dna_config")
				.append(" FROM dna_snapshot")
				.with(filters::build)
				.with(order::build)
				.toPageable(paging, (rs, row) -> new DnaSnapshotPojo(
					(UUID) rs.getObject(1),									/* snapshot uid */
					PgJSON.fromPGobject(rs.getObject(2), EntityId.class),	/* project uid and nid */
					rs.getString(3),										/* snapshot name */
					map(rs.getTimestamp(4), Timestamp::toInstant),			/* snapshot updated timestamp */
					rs.getInt(5),											/* snapshot module_count */
					PgJSON.fromPGobject(rs.getObject(6))					/* snapshot dna_config */
				));
		}
		
		protected Long buildCount() {
			return query("SELECT count(*) FROM dna_snapshot")
					.with(filters::build)
					.first(rs -> Long.valueOf(rs.getLong(1)))
					.orElse(Long.valueOf(0l));
		}
		
		public DnaSnapshotQueryBuilder byId(final UUID id) {
			filters.accept(q -> q.append("id = ?", id));
			return this;
		}
		
		protected DnaSnapshotQueryBuilder latestOfProject(final EntityId projectId) {
			filters.accept(q -> q.append("id in (SELECT id FROM (SELECT id, updated, max(updated) OVER (PARTITION BY project) updated_latest"
					+ " FROM dna_snapshot WHERE project = ").with(ProjectPgDao.referenceUidOrNid(projectId))
					.append(") t WHERE updated = updated_latest)"));
			return this;
		}
		
		@Override
		public DnaSnapshotQueryBuilder withUpdateAfter(final Instant t) {
			filters.accept(q -> q.append("updated > ?::timestamp_zoned_milli", Timestamp.from(t)));
			return this;
		}
		
		@Override
		public DnaSnapshotQueryBuilder ofProject(final EntityId projectId) {
			filters.accept(q -> q.append("project = ").with(ProjectPgDao.referenceUidOrNid(projectId)));
			return this;
		}
		
		@Override
		public DnaSnapshotQueryBuilder sortUpdated(final SortDirection direction) {
			order.accept(q -> q.appendOrder("updated", direction));
			return this;
		}
		
		@Override
		public DnaSnapshotInquiryBuilder withUpdateAt(final Instant t) {
			filters.accept(q -> q.append("updated = ?::timestamp_zoned_milli", Timestamp.from(t)));
			return this;
		}
		
		@Override
		public DnaSnapshotInquiryBuilder withUpdateAfterDiscoverMetrics(final EntityId projectId) {
			filters.accept(q -> q.append("updated > (SELECT metrics_date FROM project WHERE ")
								 .appendId(projectId)
								 .append(")"));
			return this;
		}
	}

	/**
	 * Query builder for performing queries on {@code dna_string} entities.
	 */
	public class DnaCommunityQueryBuilder implements DnaDataService.DnaCommunityInquiryBuilder {

		private static final String QUERY = "SELECT a.id, a.snapshot, a.sequencer, a.similarity_algo, a.cluster_algo, a.cluster_index, a.title, a.description"
				+ ", (SELECT array_agg(to_jsonb(module_ids)) FROM (SELECT module uid, (SELECT nid FROM module WHERE uid = module) nid"
				+ " FROM dna_community_modules WHERE community = a.id) module_ids) modules";
		private static final String QUERY_CTE = "WITH cte_t AS (" + QUERY;

		protected final FilterStreamBuilder filters = new FilterStreamBuilder();
		protected final OrderStreamBuilder order = new OrderStreamBuilder();
		private boolean needSnapshot = false;
		private boolean orderByCount;

		protected Paged.Builder<DnaCommunityPojo> build(@Nullable final Pagination paging) {
			if (paging != null) {
				order.accept(q -> q.appendOrder("dna_community.uid", SortDirection.ASCENDING));
			}
			var queryBuilder = query(orderByCount ? QUERY_CTE : QUERY)
				.append(" FROM dna_community a")
				.when(needSnapshot, q -> q.append(" INNER JOIN dna_snapshot b ON b.id = a.snapshot"))
				.with(filters::build);

			if (orderByCount) {
				queryBuilder = queryBuilder.append(") SELECT * FROM cte_t")
											.with(order::build);
			} else {
				queryBuilder = queryBuilder.append(" ORDER BY a.snapshot, a,cluster_algo, a.cluster_index");
			}

			return queryBuilder
				.toPageable(paging, (rs, row) -> new DnaCommunityPojo(
					(UUID) rs.getObject(1),									/* community uid */
					(UUID) rs.getObject(2),									/* snapshot uid */
					DnaSequencer.valueOf(rs.getString(3)),					/* community sequencer */
					DnaSimilarityAlgorithm.valueOf(rs.getString(4)),		/* community similarity_algo */
					DnaClusterAlgorithm.valueOf(rs.getString(5)),			/* community cluster_algo */
					rs.getInt(6),											/* community cluster_index */
					rs.getString(7),										/* community title */
					rs.getString(8),										/* community description */
					PgJSON.fromPGobjects(rs.getArray(9), EntityId.class),	/* modules nid and uid from community_modules */
					null
				));
		}
		
		@Override
		public DnaCommunityQueryBuilder withIds(final Collection<UUID> ids) {
			filters.accept(q -> q.append("a.id = any(?)").addArg(PgType.UUID, ids));
			return this;
		}
		
		@Override
		public DnaCommunityQueryBuilder ofProject(final EntityId projectId) {
			needSnapshot = true;
			filters.accept(q -> q.append("b.project = ").with(ProjectPgDao.referenceUidOrNid(projectId)));
			return this;
		}
		
		@Override
		public DnaCommunityQueryBuilder ofSnapshot(final UUID snapshotId) {
			filters.accept(q -> q.append("a.snapshot = ?", snapshotId));
			return this;
		}

		@Override
		public DnaCommunityInquiryBuilder ofSequencer(final DnaSequencer sequencer) {
			filters.accept(q -> q.append("a.sequencer = ?", sequencer.name()));
			return this;
		}
		
		@Override
		public DnaCommunityQueryBuilder withSimilarityAlgorithm(final DnaSimilarityAlgorithm similarityAlgo) {
			filters.accept(q -> q.append("a.similarity_algo = ?", similarityAlgo.name()));
			return this;
		}
		
		@Override
		public DnaCommunityQueryBuilder withClusterAlgorithm(final DnaClusterAlgorithm clusterAlgo) {
			filters.accept(q -> q.append("a.cluster_algo = ?", clusterAlgo.name()));
			return this;
		}
		
		private DnaCommunityInquiryBuilder containingModules(final Consumer<QueryBuilder> subQuery) {
			filters.accept(q -> q.append("EXISTS (SELECT 1 FROM dna_community_modules WHERE community = a.id AND module ").with(subQuery).append(")"));
			return this;
		}

		@Override
		public DnaCommunityInquiryBuilder containingModules(final Collection<EntityId> ids) {
			containingModules(q -> {
				q.append(" = (SELECT uid FROM module WHERE nid = any(?) OR uid = any(?))")
						.addArg(PgType.LONG, EntityId.allNids(ids))
						.addArg(PgType.UUID, EntityId.allUids(ids));
			});
			return this;
		}

		@Override
		public DnaCommunityInquiryBuilder containingModulesByUid(final Collection<UUID> uids) {
			containingModules(q -> q.append(" = any(?)").addArg(PgType.UUID, uids));
			return this;
		}
		
		@Override
		public DnaCommunityInquiryBuilder containingModulesByNid(final Collection<Long> nids) {
			containingModules(q -> q.append(" = (SELECT uid FROM module WHERE nid =  any(?))").addArg(PgType.LONG, nids));
			return this;
		}
		
		@Override
		public DnaCommunityInquiryBuilder containingModuleWithName(final String pattern) {
			containingModules(q -> q.append(" = (SELECT uid FROM module WHERE name IKLIKE ?)", pattern));
			return this;
		}
		
		@Override
		public DnaCommunityInquiryBuilder withClusterIndex(final Integer clusterIndex) {
			filters.accept(q -> q.append("a.cluster_index = ?", clusterIndex));
			return this;
		}

		@Override
		public DnaCommunityInquiryBuilder sortCount(final SortDirection direction) {
			orderByCount = true;
			order.accept(q -> q.appendOrder("ARRAY_LENGTH(modules, 1)", direction));
			return this;
		}
		
		@Override
		public DnaCommunityInquiryBuilder withUpdateAt(final Instant t) {
			filters.accept(q -> q.append("b.updated = ?::timestamp_zoned_milli", Timestamp.from(t)));
			return this;
		}
	}

	public List<DnaSnapshotPojo> findSnapshots(final BuildingConsumer<DnaDataService.DnaSnapshotInquiryBuilder> builder) {
		return builder.prepare(new DnaSnapshotQueryBuilder()).build(null).all();
	}
	
	public Paged<DnaSnapshotPojo> findSnapshots(final Pagination paging, final BuildingConsumer<DnaDataService.DnaSnapshotInquiryBuilder> builder) {
		return builder.prepare(new DnaSnapshotQueryBuilder()).build(paging).page();
	}
	
	public Optional<DnaSnapshotPojo> findSnapshot(final UUID id) {
		return new DnaSnapshotQueryBuilder().byId(id).build(null).first();
	}
	
	public Optional<DnaSnapshotPojo> latestSnapshot(final EntityId projectId) {
		return new DnaSnapshotQueryBuilder().latestOfProject(projectId).build(null).first();
	}
	
	public UUID putSnapshot(final DnaSnapshotPojoPrototype dnaSnapshot, final boolean isNew) {
		final UUID id;
		final QueryBuilder q;
		final var fields = new FieldBuilder();
		
		if (isNew) {
			id = dnaSnapshot.id.orElseNonNull(UUID::randomUUID);
			q = query("INSERT INTO dna_snapshot ");
			fields.add("id", "?", id);
		} else {
			id = dnaSnapshot.id.getNonNull();
			q = query("UPDATE dna_snapshot SET ");
		}
		
		fields.add(dnaSnapshot.project.exclusive(isNew), "project", ProjectPgDao::referenceUidOrNid);
		fields.add(dnaSnapshot.name, "name", "?");
		fields.add(dnaSnapshot.updated, "updated", "?", Timestamp::from);
		fields.add(dnaSnapshot.totalModuleCount.required(isNew), "module_count", "?");
		fields.add(dnaSnapshot.dnaConfig.required(isNew), "dna_config", "?", PgJSON::toPGobject);
		
		if (isNew) {
			fields.buildInsert(q);
		} else {
			fields.buildUpdate(q);
			q.append(" WHERE id = ?", id);
		}
		
		return q.append(" RETURNING id").first(rs -> (UUID) rs.getObject(1))
			.orElseThrow(() -> new MiningEntityNotFoundException(DnaSnapshotPojo.class, id.toString()));
	}
	
	public void deleteSnapshot(final UUID id) {
		query("DELETE FROM dna_snapshot WHERE id = ?").addArg(id)
			.updateOrThrow(() -> new MiningEntityNotFoundException(DnaSnapshotPojo.class, id.toString()));
	}
	
	public int deleteSnapshots(final EntityId projectId) {
		return query("DELETE FROM dna_snapshot WHERE project = ").with(ProjectPgDao.referenceUidOrNid(projectId)).update();
	}

	public void deleteDnaData(final Collection<UUID> moduleNids) {
		if (query("DELETE FROM dna_community_modules WHERE module = any(?)")
				.addArg(PgType.UUID, moduleNids)
				.update() > 0 &&
			/* Clean up dna_community if there are no more dna_community_modules records present */
			query("DELETE FROM dna_community WHERE id NOT IN (SELECT community FROM dna_community_modules)")
				.update() > 0) {
			/* Clean up dna_snapshot if there are no more dna_community records present */
			query("DELETE FROM dna_snapshot WHERE id NOT IN (SELECT snapshot FROM dna_community)")
					.update();
		}
	}

	public List<DnaCommunityPojo> findCommunities(final BuildingConsumer<DnaDataService.DnaCommunityInquiryBuilder> builder) {
		return builder.prepare(new DnaCommunityQueryBuilder()).build(null).all();
	}
	
	public Paged<DnaCommunityPojo> findCommunities(final Pagination paging, final BuildingConsumer<DnaDataService.DnaCommunityInquiryBuilder> builder) {
		return builder.prepare(new DnaCommunityQueryBuilder()).build(paging).page();
	}
	
	public UUID putCommunity(final DnaCommunityPojoPrototype dnaCommunity, final boolean isNew) {
		final UUID id;
		final QueryBuilder q;
		final var fields = new FieldBuilder();
		
		if (isNew) {
			id = dnaCommunity.id.orElseNonNull(UUID::randomUUID);
			q = query("INSERT INTO dna_community ");
			fields.add("id", "?", id);
		} else {
			id = dnaCommunity.id.getNonNull();
			q = query("UPDATE dna_community SET ");
		}
		
		fields.add(dnaCommunity.snapshot.exclusive(isNew), "snapshot", "?");
		fields.add(dnaCommunity.sequencerId.required(isNew), "sequencer", "?", Enum::name);
		fields.add(dnaCommunity.similarityId.required(isNew), "similarity_algo", "?", Enum::name);
		fields.add(dnaCommunity.clusterAlgorithmId.required(isNew), "cluster_algo", "?", Enum::name);
		fields.add(dnaCommunity.clusterIndex.required(isNew), "cluster_index", "?");
		fields.add(dnaCommunity.title, "title", "?");
		fields.add(dnaCommunity.description, "description", "?");
		
		if (isNew) {
			fields.buildInsert(q);
		} else {
			fields.buildUpdate(q);
			q.append(" WHERE id = ?", id);
		}
		
		return q.append(" RETURNING id").first(rs -> (UUID) rs.getObject(1))
			.orElseThrow(() -> new MiningEntityNotFoundException(DnaCommunityPojo.class, id.toString()));
	}
	
	public void deleteCommunity(final UUID id) {
		query("DELETE FROM dna_community WHERE id = ?").addArg(id)
			.updateOrThrow(() -> new MiningEntityNotFoundException(DnaCommunityPojo.class, id.toString()));
	}
	
	public int deleteCommunities(final UUID snapshotId) {
		return query("DELETE FROM dna_community WHERE snapshot = ").addArg(snapshotId).update();
	}
	
	public void putCommunityModules(final UUID communityId, final List<UUID> modules) {
		query("INSERT into dna_community_modules (community, module) VALUES (?, ?)").addArg(communityId)
			.updateBatch(modules.stream().map(Stream::<Object>of), 100);
	}

	/**
	 * Searches for all {@code dna_community} records that match with the given {@code similarity}, {@code sequencer} and {@code snapshotId} and returns a map
	 * of their {@code Module} {@link UUID} as keys and their cluster index as values.
	 *
	 * @param similarityAlgorithm the {@linkplain DnaSimilarityAlgorithm} the records have been created with
	 * @param sequencer the {@link DnaSequencer} the records have been created with
	 * @param snapshotId the ID of their DNA snapshot
	 * @return map of{@code Module} {@link UUID} as keys and cluster index as values
	 */
	public Map<UUID, Integer> getClusterIndexAndModuleRidBySnapshot(final DnaSimilarityAlgorithm similarityAlgorithm, final DnaSequencer sequencer, final UUID snapshotId) {
		return query("SELECT m.module, c.cluster_index FROM dna_community c, dna_community_modules m WHERE c.similarity_algo=? AND c.sequencer=? AND c.snapshot=? AND c.id=m.community ORDER BY c.cluster_index")
			.addArgs(similarityAlgorithm.name(), sequencer.name(), snapshotId)
			.toMap((rs, map) -> map.put((UUID) rs.getObject(1), Integer.valueOf(rs.getInt(2))));
		
	}

	/**
	 * Returns if DNA was collected for the module with the given {@code moduleId}.
	 * 
	 * @param moduleId ID of the Module to check.
	 * @return {@code true} if DNA was collected. Otherwise {@code false}
	 */
	public boolean hasDnaCommunity(final EntityId moduleId) {
		return query("SELECT EXISTS(SELECT 1 FROM dna_community_modules WHERE module = ")
					.with(ModulePgDao.referenceUidOrNid(moduleId))
					.append(")")
					.first(rs -> Boolean.valueOf(rs.getBoolean(1)))
					.orElse(Boolean.FALSE)
					.booleanValue();
	}
	
	/**
	 * @return number of matching {@code dna_community} records
	 */
	public long getDnaCommunityCount() {
		return query("SELECT count(*) FROM dna_community")
					.first(rs -> Long.valueOf(rs.getLong(1)))
					.orElse(Long.valueOf(0))
					.longValue();
	}

	/**
	 * @return number of matching {@code dna_community_modules} records
	 */
	public long getDnaCommunityModulesCount() {
		return query("SELECT count(*) FROM dna_community_modules")
					.first(rs -> Long.valueOf(rs.getLong(1)))
					.orElse(Long.valueOf(0))
					.longValue();
	}
}
