/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.access.postgres;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.CachingFunction;
import innowake.mining.shared.PatternConverter;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.BinaryValue;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.access.SourceService.SourceInquiryBuilder;
import innowake.mining.shared.access.SourceService.SourceOrderBuilder;
import innowake.mining.shared.entities.SourceContentPojo;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.entities.SourcePojoPrototype;
import innowake.mining.shared.hashing.CityHash;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import org.springframework.jdbc.core.JdbcTemplate;

import javax.persistence.EntityNotFoundException;
import javax.persistence.PersistenceException;
import java.nio.ByteOrder;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Postgres specific access methods for source code data.
 */
public class SourcePgDao extends PgDao {
	
	/**
	 * Creates a new source code data access for Postgres.
	 * @param jdbcTemplate Access to the Postgres database.
	 */
	public SourcePgDao(final JdbcTemplate jdbcTemplate) {
		super(jdbcTemplate);
	}
	
	static Consumer<QueryBuilder> referenceUidOrNid(final EntityId sourceId) {
		return q -> q.appendId(sourceId, "?", "(SELECT uid FROM source_info WHERE nid = ?)");
	}
	
	private static void filterProject(final QueryBuilder q, final EntityId project) {
		q.append(" AND project = ").with(ProjectPgDao.referenceUidOrNid(project));
	}
	
	public class SourceQueryBuilder implements SourceService.SourceInquiryBuilder {
		protected final FilterStreamBuilder filters = new FilterStreamBuilder();
		protected final OrderStreamBuilder order = new OrderStreamBuilder();
		
		protected boolean selectContent = false;
		protected boolean joinModule = false;
		protected @Nullable CachingFunction<UUID, BinaryString> contentCache;
		
		private void joinModule(final QueryBuilder q) {
			q.append(" LEFT JOIN module c ON c.source = a.uid");
		}
		
		protected Paged.Builder<SourcePojo> build(@Nullable final Pagination paging) {
			if (paging != null) {
				order.accept(q -> q.appendOrder("a.uid", SortDirection.ASCENDING));
			}
			return query("SELECT a.uid, (SELECT to_jsonb(project_ids) FROM (SELECT uid, nid FROM project WHERE uid = b.project) project_ids), a.nid,"
					+ " a.name, a.path, a.technology, a.type, a.meta_data_revision, a.content_revision, a.content_hash, a.custom_properties, "
					+ (selectContent ? "b.content" : "null::bytea content")
					+ " FROM source_info a INNER JOIN source b ON b.id = a.uid")
				.when(joinModule, this::joinModule)
				.with(filters::build)
				.with(order::build)
				.toPageable(paging, (rs, row) -> {
					final UUID uid = (UUID) rs.getObject(1); 
					final byte[] content = rs.getBytes(12);
					return new SourcePojo(
						uid,
						rs.getLong(3),
						PgJSON.fromPGobject(rs.getObject(2), EntityId.class),
						rs.getString(4),
						rs.getString(5),
						Technology.valueOf(rs.getString(6)),
						Type.valueOf(rs.getString(7)),
						rs.getLong(8),
						rs.getLong(9),
						new BinaryValue(rs.getBytes(10)),
						content == null ? cachable(contentCache, uid, k -> getSource(k, null)) : () -> new BinaryString(content),
								new CustomPropertiesMap(PgJSON.fromPGobject(rs.getObject(11)))
					);
				});
		}
		
		protected Paged.Builder<SourceContentPojo> buildContent(@Nullable final Pagination paging) {
			return query("SELECT b.id, a.nid, a.path, b.content FROM source b LEFT JOIN source_info a ON b.id = a.uid")
				.when(joinModule, this::joinModule)
				.with(filters::build)
				.with(order::build)
				.toPageable(paging, (rs, row) -> new SourceContentPojo(
						EntityId.of((UUID) rs.getObject(1), (Long) rs.getObject(2)),
						rs.getString(3), 
						new BinaryString(rs.getBytes(4))));
		}
		
		protected Paged.Builder<EntityId> buildIDs() {
			return query("SELECT b.id, a.nid FROM source b LEFT JOIN source_info a ON b.id = a.uid")
				.when(joinModule, this::joinModule)
				.with(filters::build)
				.toPageable(null, (rs, row) -> EntityId.of((UUID) rs.getObject(1), (Long) rs.getObject(2)));
		}
		
		protected Long buildCount() {
			return query("SELECT count(*) FROM source b LEFT JOIN source_info a ON b.id = a.uid")
				.when(joinModule, this::joinModule)
				.with(filters::build)
				.with(order::build)
				.first(rs -> rs.getLong(1)).orElse(0l);
		}
		
		protected List<UUID> buildAndRemove() {
			return query("DELETE FROM source _delete USING source b LEFT JOIN source_info a ON b.id = a.uid")
				.when(joinModule, this::joinModule)
				.append(" WHERE _delete.id = b.id")
				.with(filters::buildSubsequent)
				.append(" RETURNING _delete.id")
				.toList((rs, n) -> (UUID) rs.getObject(1));
		}
		
		@Override
		public SourceInquiryBuilder byUid(final UUID id) {
			filters.accept(q -> q.append("b.id = ?", id));
			return this;
		}
		
		@Override
		public SourceInquiryBuilder byNid(final Long id) {
			filters.accept(q -> q.append("a.nid = ?", id));
			return this;
		}
		
		@Override
		public SourceOrderBuilder sortNid(final SortDirection sort) {
			order.accept(q -> q.appendOrder("a.nid", sort));
			return this;
		}
		
		@Override
		public SourceOrderBuilder sortName(final SortDirection sort) {
			order.accept(q -> q.appendOrder("a.name", sort));
			return this;
		}
		
		@Override
		public SourceOrderBuilder sortPath(final SortDirection sort) {
			order.accept(q -> q.appendOrder("a.path", sort));
			return this;
		}
		
		@Override
		public SourceInquiryBuilder byId(final EntityId id) {
			return id.apply(this::byUid, this::byNid);
		}
		
		@Override
		public SourceInquiryBuilder withName(final String name) {
			filters.accept(q -> q.append("a.name LIKE ?", name));
			return this;
		}

		@Override
		public SourceInquiryBuilder withNames(final Collection<String> names) {
			filters.accept(q -> { 
				final Iterator<String> iterator = names.iterator();
				if (iterator.hasNext()) {
					q.append("a.name LIKE ?", iterator.next());
				}
				while (iterator.hasNext()) {
					q.append(" OR a.name LIKE ?", iterator.next()); 
				}
				
			});
			return this;
		}
		
		@Override
		public SourceInquiryBuilder withPath(final String path) {
			filters.accept(q -> q.append("a.path LIKE ?", path));
			return this;
		}

		@Override
		public SourceInquiryBuilder withPathRegex(final String pathRegex) {
			filters.accept(q -> q.append("a.path ~* ?", pathRegex));
			return this;
		}
		
		@Override
		public SourceInquiryBuilder withPathGlob(final String antPattern) {
			return withPathRegex(PatternConverter.convertAntToRegexPattern(antPattern));
		}
		
		@Override
		public SourceInquiryBuilder withUids(final Collection<UUID> nids) {
			filters.accept(q -> q.append("a.uid = any(?)").addArg(PgType.UUID, nids));
			return this;
		}
		
		@Override
		public SourceInquiryBuilder withNids(final Collection<Long> nids) {
			filters.accept(q -> q.append("a.nid = any(?)").addArg(PgType.LONG, nids));
			return this;
		}
		
		@Override
		public SourceInquiryBuilder includeContent(final boolean includeContent) {
			this.selectContent = includeContent;
			return this;
		}
		
		@Override
		public SourceInquiryBuilder ofProject(final EntityId projectId) {
			filters.accept(q -> q.append("b.project = ").with(ProjectPgDao.referenceUidOrNid(projectId)));
			return this;
		}
		
		@Override
		public SourceInquiryBuilder usingContentCache(final CachingFunction<UUID, BinaryString> cache) {
			selectContent = false;
			this.contentCache = cache;
			return this;
		}
		
		@Override
		public SourceInquiryBuilder withTechnology(final Technology technology) {
			filters.accept(q -> q.append("a.technology = ?", technology.name()));
			return this;
		}
		
		@Override
		public SourceInquiryBuilder withTechnology(final Collection<Technology> technology) {
			filters.accept(q -> q.append("a.technology = any(?)").addArg(PgType.STRING, technology.stream().map(Technology::name).collect(Collectors.toList())));
			return this;
		}
		
		@Override
		public SourceInquiryBuilder withType(final Type type) {
			filters.accept(q -> q.append("a.type = ?", type.name()));
			return this;
		}
		
		@Override
		public SourceInquiryBuilder withType(final Collection<Type> type) {
			filters.accept(q -> q.append("a.type = any(?)").addArg(PgType.STRING, type.stream().map(Type::name).collect(Collectors.toList())));
			return this;
		}
		
		@Override
		public SourceInquiryBuilder withReferenceFrom(final EntityId sourceId) {
			filters.accept(q -> q.append("a.uid IN (SELECT dst FROM source_references WHERE src = ").with(referenceUidOrNid(sourceId)).append(")"));
			return this;
		}
		
		@Override
		public SourceInquiryBuilder withModuleExists(final boolean predicate) {
			joinModule = true;
			filters.accept(q -> q.append("c.uid IS ").when(predicate, qq -> qq.append(" NOT")).append(" NULL"));
			return this;
		}
		
		@Override
		public SourceInquiryBuilder withModuleHashDiffers(final boolean predicate, final boolean includeNoModule) {
			joinModule = true;
			filters.accept(q -> q.when(predicate, qq -> qq.append("NOT "))
								 .append("c.content_hash = a.content_hash")
								 .when(includeNoModule, qq -> qq.append(" OR c.uid IS NULL")));
			return this;
		}
		
		@Override
		public <T> SourceInquiryBuilder ofProjectNIDsWithPaths(final Iterable<T> predicate, final Function<T, Long> projects, final Function<T, String> paths) {
			filters.accept(q -> q.append("a.uid IN (SELECT t.uid FROM source_info t INNER JOIN source s ON s.id = t.uid"
					+ " INNER JOIN ((SELECT unnest(?) project, unnest(?) path) u"
					+ " INNER JOIN project v ON v.nid = u.project) ON s.project = v.uid AND t.path = u.path)")
				.addArgs(pivot(predicate, projects, paths), PgType.LONG, PgType.STRING));
			return this;
		}
		
		@Override
		public SourceInquiryBuilder withModule(final EntityId moduleId) {
			filters.accept(q -> q.append("a.uid = (SELECT source FROM module WHERE ").appendId(moduleId).append(")"));
			return this;
		}
	}
	
	public UUID putSource(final EntityId project, @Nullable final UUID id, final byte[] content) {
		return query("INSERT INTO source (id, project, content) VALUES (" + (id != null ? "?" : "gen_random_uuid()") + ", ")
			.when(id, QueryBuilder::addArg)
			.with(ProjectPgDao.referenceUidOrNid(project))
			.addArg(content)
			.append(", ?) ON CONFLICT (id) DO UPDATE SET content = EXCLUDED.content WHERE source.project = ")
			.with(ProjectPgDao.referenceUidOrNid(project))
			.append(" RETURNING id")
			.first(rs -> (UUID) rs.getObject(1))
			.orElseThrow(() -> new PersistenceException("Failed to store Source with" + (id != null ? " ID " + id : "out ID") + " for Project " + project));
	}
	
	public void putSource(final UUID id, final byte[] content) {
		query("UPDATE source SET content = ? WHERE id = ?").addArgs(content, id)
			.updateOrThrow(() -> new MiningEntityNotFoundException(SourcePojo.class, id.toString()));
	}
	
	public BinaryString getSource(final UUID id, @Nullable final EntityId project) {
		return query("SELECT content FROM source WHERE id = ?").addArg(id)
			.when(project, SourcePgDao::filterProject)
			.first(rs -> new BinaryString(rs.getBytes(1)))
			.orElseThrow(() -> new EntityNotFoundException("Source " + id + " not found"
					+ (project != null ? " in Project " + project.toString() : "")));
	}
	
	public Optional<SourceContentPojo> findAnySource(final BuildingConsumer<SourceService.SourceInquiryBuilder> builder) {
		return builder.prepare(new SourceQueryBuilder()).buildContent(Pagination.FIRST).first();
	}
	
	public List<SourceContentPojo> findSource(final BuildingConsumer<SourceService.SourceInquiryBuilder> builder) {
		return builder.prepare(new SourceQueryBuilder()).buildContent(null).all();
	}
	
	public List<SourcePojo> find(final BuildingConsumer<SourceService.SourceInquiryBuilder> builder) {
		return builder.prepare(new SourceQueryBuilder()).build(null).all();
	}
	
	public Paged<SourcePojo> find(final Pagination page, final BuildingConsumer<SourceService.SourceInquiryBuilder> builder) {
		return builder.prepare(new SourceQueryBuilder()).build(page).page();
	}
	
	public List<EntityId> findIDs(final BuildingConsumer<SourceService.SourceInquiryBuilder> builder) {
		return builder.prepare(new SourceQueryBuilder()).buildIDs().all();
	}
	
	public Optional<SourcePojo> findAny(final BuildingConsumer<SourceService.SourceInquiryBuilder> builder) {
		return builder.prepare(new SourceQueryBuilder()).build(Pagination.FIRST).first();
	}
	
	public Optional<SourcePojo> findOne(final BuildingConsumer<SourceService.SourceInquiryBuilder> builder) {
		return builder.prepare(new SourceQueryBuilder()).build(Pagination.SINGLE).single();
	}
	
	private Consumer<QueryBuilder> referenceCodeRevisionOfProjectInSourceInfo(UUID id, @Nullable final EntityId projectId) {
		return q -> {
			/* 1 is the default which is also specified in the source_tables_create.sql. However project.source_code_revision can be null and if null
			 * is explicitly set then Postgres does not use the default value but errors out with '... violates not-null constraint' */
			q.append("(SELECT CASE WHEN source_code_revision IS NULL THEN 1 ELSE source_code_revision END FROM project WHERE ");
			if (projectId != null) {
				q.appendId(projectId);
			} else {
				q.append("uid = (SELECT project FROM source WHERE id = ?)", id);
			}
			q.append(")");
		};
	}
	
	public EntityId put(final SourcePojoPrototype source, final boolean isNew) {
		final EntityId id = isNew ? EntityId.of(source.uid.orElseNonNull(UUID::randomUUID)) : source.identityProvisional();
		
		final var fieldsSource = new FieldBuilder();
		fieldsSource.add(source.project, "project", ProjectPgDao::referenceUidOrNid);
		fieldsSource.add(source.content, "content", "?", BinaryString::get);
		
		if (source.content.isDefined() && ! source.contentHash.isDefined()) {
			source.contentHash.set(new BinaryValue(CityHash.cityHash128(ByteOrder.BIG_ENDIAN, source.content.getNonNull().get())));
		}
		
		final var fieldsInfo = new FieldBuilder();
		fieldsInfo.add(source.name, "name", "?");
		fieldsInfo.add(source.path, "path", "?", path -> mapNullable(path, p -> p.replace('\\', '/')));
		fieldsInfo.add(source.technology, "technology", "?", Technology::name);
		fieldsInfo.add(source.type, "type", "?", Type::name);
		fieldsInfo.add("meta_data_revision", q -> {
			if (source.metaDataRevision.isDefined()) {
				q.append("?", source.metaDataRevision.getNonNull());
			} else {
				q.with(referenceCodeRevisionOfProjectInSourceInfo(id.getUid(), source.project.orElse(null)));
			}
		});
		fieldsInfo.add("content_revision", () -> {
			if (source.contentRevision.isDefined()) {
				return q -> q.append("?", source.contentRevision.getNonNull());
			} else if (source.content.isDefined()) {
				return q -> q.with(referenceCodeRevisionOfProjectInSourceInfo(id.getUid(), source.project.orElse(null)));
			}
			return null;
		});
		fieldsInfo.add(source.contentHash, "content_hash", "?", BinaryValue::get);
		
		if (isNew) {
			fieldsSource.add("id", "?", id.getUid());
			fieldsInfo.add("uid", "?", id.getUid());
			query().when(source.content.isDefined(), q -> q.append("INSERT INTO source ").with(fieldsSource::buildInsert).append("; "))
				.append("INSERT INTO source_info ").with(fieldsInfo::buildInsert)
				.update();
		} else {
			query().when(source.content.isDefined(), q -> q.append("UPDATE source SET ").with(fieldsSource::buildUpdate)
					.append(" WHERE id = ").with(referenceUidOrNid(source.identityProvisional())).append("; "))
				.append("UPDATE source_info SET ").with(fieldsInfo::buildUpdate).append(" WHERE ").appendId(id)
				.update();
		}
		
		return id;
	}
	
	public int putReference(final EntityId src, final EntityId dst) {
		return query("INSERT INTO source_references (src, dst) VALUES (")
			.with(referenceUidOrNid(src)).append(", ").with(referenceUidOrNid(dst))
			.append(") ON CONFLICT DO NOTHING")
			.update();
	}
	
	public int removeReferences(final EntityId sourceId) {
		return query("DELETE FROM source_references WHERE ")
				.with(referenceUidOrNid(sourceId))
				.append(" = any(ARRAY[src, dst])")
				.update();
	}
	
	public int removeAllReferences(final EntityId projectId) {
		return query("DELETE FROM source_references WHERE src IN (SELECT id FROM source WHERE project = ")
			.with(ProjectPgDao.referenceUidOrNid(projectId))
			.append(")")
			.update();
	}
	
	public void remove(final EntityId id, @Nullable final EntityId project) {
		query("DELETE FROM source WHERE id = ").with(referenceUidOrNid(id))
			.when(project, SourcePgDao::filterProject)
			.updateOrThrow(() -> new MiningEntityNotFoundException(SourcePojo.class, id + (project != null ? " in Project " + project : "")));
	}
	
	public int remove(final Collection<EntityId> ids, @Nullable final EntityId project) {
		return query("DELETE FROM source _delete USING source a LEFT JOIN source_info b ON b.uid = a.id"
				+ " INNER JOIN (SELECT unnest(?) uid, unnest(?) nid) u ON a.id = u.uid OR b.nid = u.nid"
				+ " WHERE _delete.id = a.id")
			.addArgs(pivot(ids, EntityId::getUid, EntityId::getNid), PgType.UUID, PgType.LONG)
			.when(project, (q, p) -> q.append(" AND a.project = ").with(ProjectPgDao.referenceUidOrNid(p)))
			.update();
	}
	
	public List<UUID> removeAll(final BuildingConsumer<SourceInquiryBuilder> builder) {
		return builder.prepare(new SourceQueryBuilder()).buildAndRemove();
	}
	
	public long count(final BuildingConsumer<SourceInquiryBuilder> builder) {
		return builder.prepare(new SourceQueryBuilder()).buildCount();
	}

	public long countReferences() {
		return query("SELECT count(*) FROM source_references")
				.first(rs -> rs.getLong(1)).orElse(0l);
	}
	
}
