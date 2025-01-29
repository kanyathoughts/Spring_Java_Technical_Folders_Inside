/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.access.postgres;

import java.sql.Timestamp;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Consumer;

import org.springframework.jdbc.core.JdbcTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.discovery.config.searchorder.SearchOrder;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Postgres specific access methods for the {@code project} entity.
 */
public class ProjectPgDao extends PgDao {
	
	/**
	 * Creates a new Project entity access for Postgres.
	 * @param jdbcTemplate Access to the Postgres database.
	 */
	public ProjectPgDao(final JdbcTemplate jdbcTemplate) {
		super(jdbcTemplate);
	}

	/**
	 * Consumer for applying the given {@code projectId} to a {@link QueryBuilder}.
	 * 
	 * <p>The given {@code projectId} must contain either a {@link UUID} or a numeric id, or both</p>
	 * <p>If the {@code projectId} contains a {@link UUID} then this {@link UUID} is used. Otherwise a sub-query is added to query for the project {@link UUID}
	 * by the numeric id in {@code projectId}.</p>
	 *
	 * @param projectId {@link EntityId} of a project
	 * @return
	 */
	static Consumer<QueryBuilder> referenceUidOrNid(@Nullable final EntityId projectId) {
		if (projectId == null || projectId == EntityId.VOID || projectId.value() == null) {
			return q -> q.append("?").addArg(null);
		}

		return q -> q.appendId(projectId, "?", "(SELECT uid FROM project WHERE nid = ?)");
	}

	/**
	 * Query builder for performing queries on {@code project} entities.
	 */
	public class ProjectQueryBuilder implements ProjectService.ProjectInquiryBuilder {
		@Nullable
		protected Boolean markedForDeletion = Boolean.FALSE;
		
		protected final FilterStreamBuilder filters = new FilterStreamBuilder() {
			@Override
			public int build(final QueryBuilder qb) {
				if (markedForDeletion != null) {
					accept(q -> q.append("(to_be_deleted OR (SELECT to_be_deleted FROM client WHERE uid = client)) = ?", markedForDeletion));
				}
				return super.build(qb);
			}
		};
		
		protected final OrderStreamBuilder order = new OrderStreamBuilder();
		
		protected Paged.Builder<ProjectPojo> build(@Nullable final Pagination paging) {
			if (paging != null) {
				order.accept(q -> q.appendOrder("project.uid", SortDirection.ASCENDING));
			}
			return query("SELECT uid, (SELECT to_jsonb(client_ids) FROM (SELECT uid, nid from client WHERE uid = project.client) client_ids), nid,"
					+ " name, to_be_deleted, custom_properties, source_code_revision, metrics_base_revision, metrics_version, metrics_date,"
					+ " search_orders, default_taxonomy_category, technical_taxonomy_category,"
					+ " (SELECT jsonb_object_agg(entity, property) FROM (SELECT e.entity, array_agg(p.name) property FROM custom_property_entities e"
					+ " INNER JOIN custom_property p ON p.id = e.property WHERE e.project = project.uid GROUP BY entity) cpc) cpec,"
					+ "metadata_backup_id FROM project")
				.with(filters::build)
				.with(order::build)
				.toPageable(paging, (rs, row) -> new ProjectPojo(
					(UUID) rs.getObject(1),												/* project uid */
					new CustomPropertiesMap(PgJSON.fromPGobject(rs.getObject(6))),		/* project custom_properties */
					PgJSON.fromPGobject(rs.getObject(2), EntityId.class), null, null,	/* client uid and nid */
					Long.valueOf(rs.getLong(3)),										/* project nid */
					rs.getString(4),													/* project name */
					rs.getBoolean(5),													/* project to_be_deleted */
					Long.valueOf(rs.getLong(7)),										/* project source_code_revision */
					Long.valueOf(rs.getLong(8)),										/* project metrics_base_revision */
					rs.getString(9),													/* project metrics_version */
					mapNullable(rs.getTimestamp(10), Timestamp::toInstant),				/* project metrics_date */
					PgJSON.fromPGobjects(rs.getArray(11), SearchOrder.class),			/* project search_orders */
					Long.valueOf(rs.getLong(12)),										/* project default_taxonomy_category */
					//is it correct that this provides Long? Shouldn't they be mapped to EntityIds?
					Long.valueOf(rs.getLong(13)),										/* project technical_taxonomy_category */
					mapCustomPropertyClasses(rs.getObject(14)), 						/* project custom_property_classes */
					rs.getString(15)));													/* project metaDataBackupId */
		}
		
		protected List<UUID> buildUids() {
			return query("SELECT uid FROM project")
				.with(filters::build)
				.with(order::build)
				.toList((rs, row) -> (UUID) rs.getObject(1));
		}
		
		protected List<Long> buildNids() {
			return query("SELECT nid FROM project")
				.with(filters::build)
				.with(order::build)
				.toList((rs, row) -> Long.valueOf(rs.getLong(1)));
		}
		
		protected Long buildCount() {
			return query("SELECT count(*) FROM project")
				.with(filters::build)
				.first(rs -> Long.valueOf(rs.getLong(1)))
				.orElse(Long.valueOf(0l));
		}
		
		protected ProjectQueryBuilder byUid(final UUID uid) {
			filters.accept(q -> q.append("uid = ?", uid));
			return this;
		}
		
		protected ProjectQueryBuilder byNid(final Long nid) {
			filters.accept(q -> q.append("nid = ?", nid));
			return this;
		}
		
		@Override
		public ProjectQueryBuilder withId(final EntityId id) {
			return id.apply(this::byUid, this::byNid);
		}
		
		@Override
		public ProjectQueryBuilder ofClient(final EntityId clientId) {
			filters.accept(q -> q.append("client = ").with(ClientPgDao.referenceUidOrNid(clientId)));
			return this;
		}
		
		@Override
		public ProjectQueryBuilder ofClientUIDs(final Collection<UUID> clientIds) {
			filters.accept(q -> q.append("client = any(?)").addArg(PgType.UUID, clientIds));
			return this;
		}
		
		@Override
		public ProjectQueryBuilder ofClientNIDs(final Collection<Long> clientIds) {
			filters.accept(q -> q.append("client in (select uid from client where nid = any(?))").addArg(PgType.LONG, clientIds));
			return this;
		}
		
		@Override
		public ProjectQueryBuilder withName(final String name) {
			filters.accept(q -> q.append("name ILIKE ?", name));
			return this;
		}
		
		@Override
		public ProjectQueryBuilder withIdAbove(final Long nid) {
			filters.accept(q -> q.append("nid > ?", nid));
			return this;
		}
		
		@Override
		public ProjectQueryBuilder withIds(final Collection<Long> nids) {
			filters.accept(q -> q.append("nid = any(?)").addArg(PgType.LONG, nids));
			return this;
		}
		
		@Override
		public ProjectQueryBuilder withIds(final Collection<Long> projectNids, final Collection<Long> clientNids) {
			filters.accept(q -> q.append("nid = any(?) OR client in (select uid from client where nid = any(?))")
					.addArg(PgType.LONG, projectNids)
					.addArg(PgType.LONG, clientNids));
			return this;
		}
		
		@Override
		public ProjectQueryBuilder filterMarkedForDeletion(@Nullable final Boolean toBeDeleted) {
			this.markedForDeletion = toBeDeleted;
			return this;
		}
		
		@Override
		public ProjectQueryBuilder sortNid(final SortDirection direction) {
			order.accept(q -> q.appendOrder("nid", direction));
			return this;
		}
		
		@Override
		public ProjectQueryBuilder sortName(final SortDirection direction) {
			order.accept(q -> q.appendOrder("name", direction));
			return this;
		}
	}
	
	/**
	 * Count Project entries.
	 * @param builder Builder for filter criteria.
	 * @return The number of Project entries.
	 */
	public Long count(final BuildingConsumer<ProjectService.ProjectInquiryBuilder> builder) {
		return builder.prepare(new ProjectQueryBuilder()).buildCount();
	}
	
	/**
	 * Retrieves a paged subset of optionally filtered Projects.
	 * @param paging Pagination specification.
	 * @param builder Builder for filter criteria and sorting options.
	 * @return Paged subset of matching Project entities.
	 */
	public Paged<ProjectPojo> find(final Pagination paging, final BuildingConsumer<ProjectService.ProjectInquiryBuilder> builder) {
		return builder.prepare(new ProjectQueryBuilder()).build(paging).page();
	}
	
	/**
	 * Retrieves all or a filtered subset of Projects.
	 * @param builder Builder for filter criteria and sorting options.
	 * @return All matching Project entities.
	 */
	public List<ProjectPojo> find(final BuildingConsumer<ProjectService.ProjectInquiryBuilder> builder) {
		return builder.prepare(new ProjectQueryBuilder()).build(null).all();
	}
	
	/**
	 * Retrieves all or a filtered subset of unique IDs of Projects.
	 * @param builder Builder for filter criteria and sorting options.
	 * @return List of project IDs.
	 */
	public List<UUID> findUids(final BuildingConsumer<ProjectService.ProjectInquiryBuilder> builder) {
		return builder.prepare(new ProjectQueryBuilder()).buildUids();
	}
	
	/**
	 * Retrieves all or a filtered subset of numeric IDs of Projects.
	 * @param builder Builder for filter criteria and sorting options.
	 * @return List of project IDs.
	 */
	public List<Long> findNids(final BuildingConsumer<ProjectService.ProjectInquiryBuilder> builder) {
		return builder.prepare(new ProjectQueryBuilder()).buildNids();
	}
	
	/**
	 * Retrieve any Project by its unique ID.
	 * @param projectId ID of the Project.
	 * @return The Project if it exists.
	 */
	public Optional<ProjectPojo> find(final UUID projectId) {
		return new ProjectQueryBuilder().byUid(projectId).build(null).first();
	}
	
	/**
	 * Retrieve a Project by its ID.
	 * @param projectId ID of the Project.
	 * @return The Project if it exists and id valid.
	 */
	public Optional<ProjectPojo> find(final EntityId projectId) {
		return new ProjectQueryBuilder().withId(projectId).build(null).first();
	}
	
	private static Map<String, Set<String>> mapCustomPropertyClasses(final Object value) {
		final var map = PgJSON.fromPGobject(value);
		final var cpc = new HashMap<String, Set<String>>();
		for (final Map.Entry<String, Object> e : map.entrySet()) {
			if (e.getValue() != null) {
				cpc.put(e.getKey(), new HashSet<>(collection(e.getValue())));
			}
		}
		return cpc;
	}
	
	private EntityId put(final ProjectPojoPrototype project, final boolean isNew) {
		final EntityId id;
		final QueryBuilder q;
		final FieldBuilder fields = new FieldBuilder();
		
		if (isNew) {
			id = EntityId.of(project.uid.orElseNonNull(UUID::randomUUID));
			q = query("INSERT INTO project ");
			fields.add("uid", "?", id.getUid());
		} else {
			id = project.identityProvisional();
			q = query("UPDATE project SET ");
		}
		
		fields.add(project.client.required(isNew), "client", ClientPgDao::referenceUidOrNid);
		fields.add(project.name.required(isNew), "name", "?");
		fields.add(project.sourceCodeRevision, "source_code_revision", "?");
		fields.add(project.metricsBaseRevision, "metrics_base_revision", "?");
		fields.add(project.metricsVersion, "metrics_version", "?");
		fields.add(project.metricsDate, "metrics_date", "?", passNull(Timestamp::from));
		fields.add(project.searchOrders, "search_orders", "?", PgJSON::toPGobjects);
		fields.add(project.defaultTaxonomyCategory, "default_taxonomy_category", "?");
		fields.add(project.technicalTaxonomyCategory, "technical_taxonomy_category", "?");
		fields.add(project.metaDataBackupId, "metadata_backup_id", "?");
		fields.and(CustomPropertiesPgDao.addField(project, isNew));

		if (isNew) {
			fields.buildInsert(q);
		} else {
			fields.buildUpdate(q);
			q.append(" WHERE ");
			q.appendId(id);
		}
		
		return q.append(" RETURNING uid, nid").first(rs -> EntityId.of((UUID) rs.getObject(1), Long.valueOf(rs.getLong(2))))
			.orElseThrow(() -> new MiningEntityNotFoundException(ProjectPojo.class, id.toString()));
	}
	
	public EntityId create(final ProjectPojoPrototype project) {
		return put(project, true);
	}
	
	public EntityId update(final ProjectPojoPrototype project) {
		return put(project, false);
	}
	
	public List<String> findConfigs(final EntityId projectId) {
		return query("SELECT name FROM project_configuration WHERE project = ").with(referenceUidOrNid(projectId))
				.toList((rs, row) -> rs.getString(1));
	}

	public Map<String, Map<String, Object>> fetchConfigs(final EntityId projectId) {
		return query("SELECT name, value FROM project_configuration WHERE project = ").with(referenceUidOrNid(projectId))
				.toMap((rs, m) -> m.put(rs.getString(1), PgJSON.fromPGobject(rs.getObject(2))));
	}

	public <T> Optional<T> fetchConfigByName(final EntityId projectId, final Class<T> valueType, final String name) {
		return query("SELECT value FROM project_configuration WHERE project = ").with(referenceUidOrNid(projectId))
				.append(" AND name = ?", name)
				.first(rs -> PgJSON.fromPGobject(rs.getObject(1), valueType));
	}

	public Map<String, String> fetchConfigsByKeyAndNames(final EntityId projectId, final String key, final Collection<String> names) {
		return query("SELECT name, value->>? FROM project_configuration WHERE project = ")
				.addArg(key)
				.with(referenceUidOrNid(projectId)).append(" AND name = any(?)").addArg(PgType.STRING, names)
				.toMap((rs, m) -> m.put(rs.getString(1), rs.getString(2)));
	}

	public Optional<String> fetchConfigByKeyAndName(final EntityId projectId, final String key, final String name) {
		return query("SELECT value->>? FROM project_configuration WHERE project = ")
				.addArg(key)
				.with(referenceUidOrNid(projectId)).append(" AND name = ?", name)
				.first(rs -> rs.getString(1));
	}
	
	public void putConfig(final EntityId projectId, final String name, final Object value) {
		query("INSERT INTO project_configuration (project, name, value) SELECT uid, ?, ? FROM project WHERE ")
			.addArgs(name, PgJSON.toPGobject(value)).appendId(projectId)
			.append(" AND to_be_deleted = false ON CONFLICT (project, name) DO UPDATE SET value = EXCLUDED.value")
			.update();
	}
	
	public void deleteConfig(final EntityId projectId, final String name) {
		query("DELETE FROM project_configuration WHERE ")
			.with(referenceUidOrNid(projectId))
			.append(" AND name = ?", name)
			.updateOrThrow(() -> new MiningEntityNotFoundException(ProjectPojo.class, projectId.toString()));
	}
	
	private QueryBuilder markForDeletion(final Consumer<QueryBuilder> selection) {
		return query("UPDATE project SET to_be_deleted = true, name = concat('_TO_BE_DELETED_', uid, '_', name) WHERE ")
			.with(selection).append(" AND nid > 0 AND to_be_deleted = false");
	}
	
	/**
	 * Marks a Project for deletion.
	 * Only works on Projects not already marked for deletion and not on Project 0.
	 * @param projectId ID of the Project.
	 */
	public void markForDeletion(final EntityId projectId) {
		markForDeletion(q -> q.appendId(projectId)).updateOrThrow(() -> new MiningEntityNotFoundException(ProjectPojo.class, projectId.toString()));
	}
	
	/**
	 * Marks all Projects of a Client for deletion.
	 * Only works on Projects not already marked for deletion and not on Project 0.
	 * @param clientId ID of the Client.
	 * @return Number of Projects affected.
	 */
	public int markForDeletionByClient(final EntityId clientId) {
		return markForDeletion(q -> q.append("client = ").with(ClientPgDao.referenceUidOrNid(clientId))).update();
	}
	
	/**
	 * Deletes a Project entry. Will not delete Project 0.
	 * @param projectId ID of the Project.
	 */
	public void delete(final EntityId projectId) {
		query("DELETE FROM project WHERE nid > 0 AND ").appendId(projectId)
			.updateOrThrow(() -> new MiningEntityNotFoundException(ProjectPojo.class, projectId.toString()));
	}
	
	/**
	 * Increments the source code revision of a Project or sets it to 1 if it is is null.
	 * @param projectId ID of the Project.
	 * @return The updated source code revision.
	 */
	public Long incrementSourceCodeRevision(final EntityId projectId) {
		return query("UPDATE project SET source_code_revision = COALESCE(source_code_revision + 1, 1) WHERE ").appendId(projectId)
			.append(" RETURNING source_code_revision")
			.first(rs -> Long.valueOf(rs.getLong(1)))
			.orElseThrow(() -> new MiningEntityNotFoundException(ProjectPojo.class, projectId.toString()));
	}
	
}
