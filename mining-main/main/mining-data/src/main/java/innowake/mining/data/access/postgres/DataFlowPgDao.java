package innowake.mining.data.access.postgres;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

import org.apache.commons.lang3.tuple.Triple;
import org.springframework.jdbc.core.JdbcTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.CachingFunction;
import innowake.mining.shared.access.Comperator;
import innowake.mining.shared.access.DataFlowService;
import innowake.mining.shared.access.DataFlowService.DataFlowErrorInquiryBuilder;
import innowake.mining.shared.access.DataFlowService.DataFlowNodeInquiryBuilder;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.dataflow.DataFlowErrorPojo;
import innowake.mining.shared.entities.dataflow.DataFlowErrorPojoPrototype;
import innowake.mining.shared.entities.dataflow.DataFlowNodePojo;
import innowake.mining.shared.entities.dataflow.DataFlowNodePojoPrototype;
import innowake.mining.shared.entities.dataflow.DataFlowNodeRelationshipType;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.datalineage.DataFlowId;

public class DataFlowPgDao extends PgDao {

	private final AstPgDao astPgDao;

	public class DataFlowNodeQueryBuilder implements DataFlowService.DataFlowNodeInquiryBuilder {

		private final FilterStreamBuilder filters = new FilterStreamBuilder();
		private final OrderStreamBuilder order = new OrderStreamBuilder();

		@Nullable
		private CachingFunction<UUID, AstNodePojo> astNodeCache = null;
		@Nullable
		private CachingFunction<UUID, ProxyContainerPojo> proxyContainerCache = null;
		@Nullable
		private List<UUID> joinIDs;

		protected Paged.Builder<DataFlowNodePojo> build(@Nullable final Pagination paging) {
			if (joinIDs != null ) {
				if ( ! order.isEmpty()) {
					throw new IllegalArgumentException("Order must not be changed if IDs are to be joined");
				}
				order.accept(q -> q.append("min(ids.row)"));
			}

			return query("SELECT d.id,"																										/* 1 */
							+ "d.data_flow_id,"																								/* 2 */
							+ "d.module, "																									/* 3 */
							+ "m.nid,"																										/* 4 */
							+ "d.ast_node, "																								/* 5 */
							+ "CASE WHEN d.ast_node IS Null THEN Null ELSE"
									+ " (SELECT to_jsonb(loc) FROM (SELECT (an.location).assembled_offset as offset,"
																		+ "(an.location).assembled_length as length) loc) END location,"	/* 6 */
							+ "d.proxy_container,"																							/* 7 */
							+ "d.name,"																										/* 8 */
							+ "d.traced,"																									/* 9 */
							+ "d.type "																										/* 10 */
							+ "FROM data_flow_node d "
							+ "INNER JOIN module m ON d.module = m.uid "
							+ "LEFT JOIN ast_node an ON an.id = d.ast_node")
					.when(joinIDs, (q, ids) -> q.append(" INNER JOIN unnest(?) WITH ORDINALITY ids (id, row) ON d.id = ids.id").addArg(PgType.UUID, ids))
					.with(filters::build)
					.when(joinIDs != null, q -> q.append(" GROUP BY d.id, m.nid, an.location"))
					.with(order::build)
					.toPageable(paging, (rs, row) -> new DataFlowNodePojo(
							(UUID) rs.getObject(1),																	/* data flow node uid */
							DataFlowId.fromDb(rs.getString(2)),														/* data flow id */
							EntityId.of((UUID) rs.getObject(3), rs.getLong(4)), null, null,							/* module uid, nid */
							Optional.of(cachableWithKey(astNodeCache, (UUID) rs.getObject(5), astPgDao::get)),		/* ast node uid in supplier*/
							PgJSON.fromPGobjectOrNull(rs.getObject(6), ModuleLocation.class),						/* ast_node assembled location */
							Optional.of(cachableWithKey(proxyContainerCache, (UUID) rs.getObject(7),				/* proxy container uid in supplier */
									x -> DataFlowPgDao.this.findAnyProxyContainer(q -> q.byId(x))
											.orElseThrow(() -> new MiningEntityNotFoundException(ProxyContainerPojo.class, x.toString())))),
							rs.getString(8),																		/* data flow node name */
							rs.getBoolean(9),																		/* data flow node traced */
							DataFlowNodePojo.Type.valueOf(rs.getString(10))											/* data flow node type */
					));
		}

		protected Paged.Builder<UUID> buildIds(@Nullable final Pagination paging) {
			return query("SELECT d.id FROM data_flow_node d ")
					.append("INNER JOIN module m ON d.module = m.uid ")
					.with(filters::build)
					.with(order::build)
					.toPageable(paging, (rs, row) -> (UUID) rs.getObject(1));
		}

		protected QueryBuilder buildDelete() {
			return query("DELETE FROM data_flow_node WHERE id IN ("
							+ "SELECT id FROM data_flow_node d "
							+ "INNER JOIN module m ON d.module = m.uid ")
							.with(filters::build)
					.append(")");
		}

		@Override
		public DataFlowService.DataFlowNodeInquiryBuilder byId(final UUID id) {
			filters.accept(q -> q.append("d.id = ?", id));
			return this;
		}

		@Override
		public DataFlowNodeQueryBuilder byIds(final Collection<UUID> ids) {
			filters.accept(q -> q.append("d.id = any(?)").addArg(PgType.UUID, ids));
			return this;
		}

		@Override
		public DataFlowNodeQueryBuilder byIdsWithOrdinality(final List<UUID> ids) {
			joinIDs = ids;
			return this;
		}

		@Override
		public DataFlowNodeQueryBuilder ofModule(final EntityId id) {
			filters.accept(q -> q.append("d.module = ").with(ModulePgDao.referenceUidOrNid(id)));
			return this;
		}

		@Override
		public DataFlowNodeQueryBuilder ofModules(final Collection<EntityId> moduleIds) {
			final var uids = EntityId.allUids(moduleIds);
			if (uids.size() == moduleIds.size()) {
				filters.accept(q -> q.append("m.uid = any(?)").addArg(PgType.UUID, uids));
				return this;
			}

			final var nids = EntityId.allNids(moduleIds);
			if (nids.size() == moduleIds.size()) {
				filters.accept(q -> q.append("m.nid = any(?)").addArg(PgType.LONG, nids));
				return this;
			}

			filters.accept(q -> q.append("(m.uid = any(?) OR m.nid = any(?))")
								 .addArg(PgType.UUID, uids)
								 .addArg(PgType.LONG, nids));
			return this;
		}

		@Override
		public DataFlowNodeQueryBuilder ofProject(final EntityId projectId) {
			filters.accept(q -> q.append("m.project = ").with(ProjectPgDao.referenceUidOrNid(projectId)));
			return this;
		}

		@Override
		public DataFlowNodeQueryBuilder ofProxyContainer(final UUID id) {
			filters.accept(q -> q.append("d.proxy_container = ?", id));
			return this;
		}

		@Override
		public DataFlowNodeInquiryBuilder ofProxyContainer(final DataFlowId id) {
			filters.accept(q -> q.append("d.proxy_container = (SELECT id FROM proxy_container WHERE data_flow_id = ?)", id.getId()));
			return this;
		}

		@Override
		public DataFlowNodeQueryBuilder withInclusionCalleeModuleId(@Nullable final EntityId module) {
			if (module == null) {
				filters.accept(q -> q.append("an.included_module IS Null"));
			} else {
				filters.accept(q -> q.append("an.included_module = ").with(ModulePgDao.referenceUidOrNid(module)));
			}
			return this;
		}

		@Override
		public DataFlowNodeQueryBuilder withModuleLocation(final ModuleLocation location) {
			filters.accept(q -> q.append("m.location = ?", location));
			return this;
		}

		@Override
		public DataFlowNodeQueryBuilder withName(final String key) {
			filters.accept(q -> q.append("d.name = ?", key));
			return this;
		}

		@Override
		public DataFlowNodeQueryBuilder withType(final DataFlowNodePojo.Type type) {
			filters.accept(q -> q.append("d.type = ?", type.name()));
			return this;
		}

		@Override
		public DataFlowNodeQueryBuilder notWithType(final DataFlowNodePojo.Type type) {
			filters.accept(q -> q.append("d.type != ?", type.name()));
			return this;
		}

		@Override
		public DataFlowNodeQueryBuilder ofAstNode(final UUID astNode) {
			filters.accept(q -> q.append("d.ast_node = ?", astNode));
			return this;
		}

		@Override
		public DataFlowNodeQueryBuilder withAssembledOffset(final Comperator comperator, final int offset) {
			filters.accept(q -> q.appendComparison("(an.location).assembled_offset", comperator, offset));
			return this;
		}

		@Override
		public DataFlowNodeQueryBuilder withAssembledEndOffset(final Comperator comperator, final int offset) {
			filters.accept(q -> q.appendComparison("((an.location).assembled_offset + (an.location).assembled_length)", comperator, offset));
			return this;
		}

		@Override
		public DataFlowNodeQueryBuilder withRetracedOffset(final Comperator comperator, final int offset) {
			filters.accept(q -> q.appendComparison("(an.location).retraced_offset", comperator, offset));
			return this;
		}

		@Override
		public DataFlowNodeQueryBuilder withRetracedEndOffset(final Comperator comperator, final int offset) {
			filters.accept(q -> q.appendComparison("((an.location).retraced_offset + (an.location).retraced_length)", comperator, offset));
			return this;
		}

		@Override
		public DataFlowNodeQueryBuilder withDataFlowId(final DataFlowId dataFlowId) {
			filters.accept(q -> q.append("d.data_flow_id = ?", dataFlowId.getId()));
			return this;
		}

		@Override
		public DataFlowNodeQueryBuilder withRelationshipFrom(final UUID src, final DataFlowNodeRelationshipType... types) {
			filters.accept(q -> {
				q.append("d.id IN (SELECT dst FROM data_flow_node_relationship rs WHERE rs.src = ?", src);
				if (types.length == 1) {
					q.append(" AND rs.type = ?::data_flow_node_relationship_type)", types[0].name());
				} else if (types.length > 1) {
					q.append(" AND rs.type = any(?::data_flow_node_relationship_type[]))")
					 .addArg(PgType.STRING, Arrays.stream(types)
													.map(DataFlowNodeRelationshipType::name)
													.toList());
				}
			});
			return this;
		}

		@Override
		public DataFlowNodeQueryBuilder sortName(final SortDirection sort) {
			order.accept(q -> q.appendOrder("d.name", sort));
			return this;
		}

		@Override
		public DataFlowNodeQueryBuilder usingAstNodeCache(final CachingFunction<UUID, AstNodePojo> cache) {
			this.astNodeCache = cache;
			return this;
		}
		
		@Override
		public DataFlowNodeQueryBuilder usingProxyContainerCache(final CachingFunction<UUID, ProxyContainerPojo> cache) {
			this.proxyContainerCache = cache;
			return this;
		}
	}

	protected class ProxyContainerQueryBuilder implements DataFlowService.ProxyContainerInquiryBuilder {
		private final FilterStreamBuilder filters = new FilterStreamBuilder();
		private final OrderStreamBuilder order = new OrderStreamBuilder();

		@Nullable
		private CachingFunction<UUID, DataFlowNodePojo> fieldNodeCache = null;

		protected Paged.Builder<ProxyContainerPojo> build(@Nullable final Pagination paging) {
			return query("SELECT p.id, "																											/* 1 */
							+ "p.data_flow_id, "																									/* 2 */
							+ "p.statement, "																										/* 3 */
							+ "p.type, "																											/* 4 */
							+ "p.properties, "																										/* 5 */
							+ "m.uid, "																												/* 6 */
							+ "m.nid, "																												/* 7 */
							+ "CASE WHEN p.statement IS Null THEN Null ELSE"
										+ " (SELECT to_jsonb(loc) FROM (SELECT (an.location).assembled_offset as offset,"
																			+ "(an.location).assembled_length as length) loc) END location, "					/* 8 */
							+ "(SELECT ARRAY_AGG(data_flow_node ORDER BY ordinal) FROM proxy_container_field WHERE proxy_container = p.id) as DataFlowNodes "	/* 9 */
							+ "FROM proxy_container p "
							+ "INNER JOIN module m ON p.module = m.uid "
							+ "LEFT JOIN ast_node an ON an.id = p.statement ")
					.with(filters::build)
					.with(order::build)
					.toPageable(paging, (rs, row) -> new ProxyContainerPojo(
														(UUID) rs.getObject(1),														/* proxy container uid */
														EntityId.of((UUID) rs.getObject(6), rs.getLong(7)), null, null,				/* module */
														DataFlowId.fromDb(rs.getString(2)),											/* data flow id */
														(UUID) rs.getObject(3),														/* statement */
														PgJSON.fromPGobjectOrNull(rs.getObject(8), ModuleLocation.class),			/* module location */
														cachableKeyedBatch(fieldNodeCache,
																streamArray(rs.getArray(9)).map(UUID.class::cast).toList(),			/* field nodes ids ordered by proxy_container_field.ordinal*/
																k -> k.isEmpty() ? List.of() :
																	DataFlowPgDao.this.find(q -> q.byIdsWithOrdinality(k))),		/* load field nodes in same order as field node ids */
														ProxyContainerPojo.Type.valueOf(rs.getString(4)),							/* proxy container type */
														PgJSON.fromPGobject(rs.getObject(5)))
					);
		}

		protected QueryBuilder buildDelete() {
			return query("DELETE FROM proxy_container WHERE id IN ("
							+ "SELECT id FROM proxy_container p "
							+ "INNER JOIN module m ON p.module = m.uid ")
							.with(filters::build).append(")");
		}
		
		@Override
		public ProxyContainerQueryBuilder byId(final UUID id) {
			filters.accept(q -> q.append("p.id = ?", id));
			return this;
		}

		@Override
		public ProxyContainerQueryBuilder ofProject(final EntityId projectId) {
			filters.accept(q -> q.append("m.project = ", ProjectPgDao.referenceUidOrNid(projectId)));
			return this;
		}

		@Override
		public ProxyContainerQueryBuilder ofModule(final EntityId moduleId) {
			filters.accept(q -> q.appendId(moduleId, "m.uid = ?", "m.nid = ?"));
			return this;
		}

		@Override
		public ProxyContainerQueryBuilder ofModules(final List<EntityId> moduleIds) {
			final var uids = EntityId.allUids(moduleIds);
			if (uids.size() == moduleIds.size()) {
				filters.accept(q -> q.append("m.uid = any(?)").addArg(PgType.UUID, uids));
				return this;
			}

			final var nids = EntityId.allNids(moduleIds);
			if (nids.size() == moduleIds.size()) {
				filters.accept(q -> q.append("m.nid = any(?)").addArg(PgType.LONG, nids));
				return this;
			}

			filters.accept(q -> q.append("(m.uid = any(?) OR m.nid = any(?))")
								 .addArg(PgType.UUID, uids)
								 .addArg(PgType.LONG, nids));
			return this;
		}

		@Override
		public ProxyContainerQueryBuilder withType(final ProxyContainerPojo.Type type) {
			filters.accept(q -> q.append("p.type = ?", type.name()));
			return this;
		}

		@Override
		public ProxyContainerQueryBuilder withDataFlowId(final DataFlowId proxyContainerId) {
			filters.accept(q -> q.append("p.data_flow_id = ?", proxyContainerId.getId()));
			return this;
		}

		@Override
		public ProxyContainerQueryBuilder usingCache(final CachingFunction<UUID, DataFlowNodePojo> cache) {
			this.fieldNodeCache = cache;
			return this;
		}
	}

	public class DataFlowErrorQueryBuilder implements DataFlowService.DataFlowErrorInquiryBuilder {
		private final FilterStreamBuilder filters = new FilterStreamBuilder();
		private final OrderStreamBuilder order = new OrderStreamBuilder();

		protected Paged.Builder<DataFlowErrorPojo> build(@Nullable final Pagination paging) {
			return query("SELECT data_flow_node, severity, text FROM data_flow_error")
					.with(filters::build)
					.with(order::build)
					.toPageable(paging, (rs, row) -> new DataFlowErrorPojo(
							(UUID) rs.getObject(1),									/* data flow node uid */
							DataFlowErrorPojo.Severity.valueOf(rs.getString(2)),	/* severity */
							rs.getString(3)											/* text */
					));
		}

		protected QueryBuilder buildDelete() {
			return query("DELETE FROM data_flow_error")
					.with(filters::build);
		}

		@Override
		public DataFlowErrorQueryBuilder ofProject(final EntityId projectId) {
			filters.accept(q -> q.append("data_flow_node = (SELECT uid FROM data_flow_node WHERE module = (SELECT uid FROM module WHERE project = ")
								 .with(ProjectPgDao.referenceUidOrNid(projectId))
								 .append("))"));
			return this;
		}

		@Override
		public DataFlowErrorQueryBuilder ofNode(final UUID uid) {
			filters.accept(q -> q.append("data_flow_node = ?", uid));
			return this;
		}
	}

	public DataFlowPgDao(final JdbcTemplate jdbcTemplate) {
		super(jdbcTemplate);
		this.astPgDao = new AstPgDao(jdbcTemplate);
	}

	public UUID put(final DataFlowNodePojoPrototype node, final boolean isNew) {
		final FieldBuilder fields = createFieldBuilder(node, isNew);
		final QueryBuilder q;
		if (isNew) {
			fields.add("id", "?", node.id.orElseNonNull(UUID::randomUUID));
			q = query("INSERT INTO data_flow_node ")
					.with(fields::buildInsert);
		} else {
			q = query("UPDATE data_flow_node SET ")
					.with(fields::buildUpdate)
					.append(" WHERE id = ?", node.id.getNonNull());
		}

		q.append(" RETURNING id");
		return q.first(rs -> (UUID) rs.getObject(1))
				.orElseThrow(() -> new MiningEntityNotFoundException(DataFlowNodePojo.class, node.toString()));
	}

	public void update(final BuildingConsumer<DataFlowService.DataFlowNodeInquiryBuilder> builder, final DataFlowNodePojoPrototype node) {
		final var fields = createFieldBuilder(node, false);
		final var filter = builder.prepare(new DataFlowNodeQueryBuilder());

		query("UPDATE data_flow_node d SET ")
				.with(fields::buildUpdate)
				.with(filter.filters::build)
				.update();
	}

	private static FieldBuilder createFieldBuilder(final DataFlowNodePojoPrototype node, final boolean isNew) {
		return new FieldBuilder()
				.add(node.dataFlowId.required(isNew), "data_flow_id", "?", DataFlowId::getId)
				.add(node.module.required(isNew), "module", ModulePgDao::referenceUidOrNid)
				.add(node.astNode, "ast_node", "?")
				.add(node.proxyContainer, "proxy_container", "?")
				.add(node.name.required(isNew), "name", "?")
				.add(node.traced.required(isNew), "traced", "?")
				.add(node.type.required(isNew), "type", "?", DataFlowNodePojo.Type::name);
	}

	public List<DataFlowNodePojo> find(final BuildingConsumer<DataFlowService.DataFlowNodeInquiryBuilder> builder) {
		return builder.prepare(new DataFlowNodeQueryBuilder())
				.build(null)
				.all();
	}

	public List<UUID> findIds(final BuildingConsumer<DataFlowService.DataFlowNodeInquiryBuilder> builder) {
		return builder.prepare(new DataFlowNodeQueryBuilder())
				.buildIds(null)
				.all();
	}

	public Optional<DataFlowNodePojo> findAny(final BuildingConsumer<DataFlowService.DataFlowNodeInquiryBuilder> builder) {
		return builder.prepare(new DataFlowNodeQueryBuilder())
				.build(Pagination.FIRST)
				.first();
	}

	public Optional<UUID> findAnyId(final BuildingConsumer<DataFlowService.DataFlowNodeInquiryBuilder> builder) {
		return builder.prepare(new DataFlowNodeQueryBuilder())
				.buildIds(Pagination.FIRST)
				.first();
	}

	/**
	 * Batch creation of relationships between nodes.
	 * <p>The {@link Triple} entries in the given {@code relationships} collection must contain the {@code src} data_flow_node id as the 'left',
	 * the {@code dst} data_flow_node id as the 'middle' and the {@link DataFlowNodeRelationshipType} as the 'right' element.</p>
	 *
	 * @param relationships the relationship {@link Triple Triples} to create
	 */
	public void createRelationships(final Collection<Triple<UUID, UUID, DataFlowNodeRelationshipType>> relationships) {
		if ( ! relationships.isEmpty()) {
			final var batchArgs = relationships.stream()
					.map(t -> Stream.<Object>of(t.getLeft(), t.getMiddle(), t.getRight().name()));

			query("INSERT INTO data_flow_node_relationship VALUES (?, ?, ?::data_flow_node_relationship_type) ON CONFLICT DO NOTHING")
				.updateBatch(batchArgs, 1_000);
		}
	}

	public int deleteRelationships(final UUID node) {
		return query("DELETE FROM data_flow_node_relationship WHERE src = ? or dst = ?")
				.addArgs(node, node)
				.update();
	}

	public int deleteNodes(final BuildingConsumer<DataFlowService.DataFlowNodeInquiryBuilder> builder) {
		return builder.prepare(new DataFlowNodeQueryBuilder())
				.buildDelete()
				.update();
	}

	public UUID put(final ProxyContainerPojoPrototype proxyContainer, final boolean isNew) {
		final FieldBuilder fields = createFieldBuilder(proxyContainer, isNew);
		final QueryBuilder query;
		if (isNew) {
			fields.add("id", "?", proxyContainer.id.orElseNonNull(UUID::randomUUID));
			query = query("INSERT INTO proxy_container ")
					.with(fields::buildInsert);
		} else {
			query = query("UPDATE proxy_container SET ")
					.with(fields::buildUpdate)
					.append(" WHERE id = ?", proxyContainer.id.getNonNull());
		}

		return query.append(" RETURNING id")
					.first(rs -> (UUID) rs.getObject(1))
					.orElseThrow(() -> new MiningEntityNotFoundException(DataFlowNodePojo.class, proxyContainer.toString()));
	}

	private FieldBuilder createFieldBuilder(final ProxyContainerPojoPrototype proxyContainer, final boolean isNew) {
		return new FieldBuilder()
				.add(proxyContainer.dataFlowId.required(isNew), "data_flow_id", "?", DataFlowId::getId)
				.add(proxyContainer.module.required(isNew), "module", ModulePgDao::referenceUidOrNid)
				.add(proxyContainer.statement, "statement", "?")
				.add(proxyContainer.type.required(isNew), "type", "?", ProxyContainerPojo.Type::name)
				.add(proxyContainer.properties.required(isNew), "properties", "?", PgJSON::toPGobject);
	}

	public void updateProxyContainers(final BuildingConsumer<DataFlowService.ProxyContainerInquiryBuilder> builder, final ProxyContainerPojoPrototype proxyContainer) {
		final FieldBuilder fields = createFieldBuilder(proxyContainer, false);
		final var filter = builder.prepare(new ProxyContainerQueryBuilder());

		query("UPDATE proxy_container SET ")
					.with(fields::buildUpdate)
					.with(filter.filters::build)
					.update();
	}

	public List<ProxyContainerPojo> findProxyContainers(final BuildingConsumer<DataFlowService.ProxyContainerInquiryBuilder> builder) {
		return builder.prepare(new ProxyContainerQueryBuilder())
				.build(null)
				.all();
	}

	public Optional<ProxyContainerPojo> findAnyProxyContainer(final BuildingConsumer<DataFlowService.ProxyContainerInquiryBuilder> builder) {
		return builder.prepare(new ProxyContainerQueryBuilder())
				.build(Pagination.FIRST)
				.first();
	}

	public int deleteProxyContainers(final BuildingConsumer<DataFlowService.ProxyContainerInquiryBuilder> builder) {
		return builder.prepare(new ProxyContainerQueryBuilder())
				.buildDelete()
				.update();
	}

	public void createProxyContainerRelationships(final UUID proxyContainerId, final List<UUID> nodeIds) {
		if ( ! nodeIds.isEmpty()) {
			final AtomicInteger ordinal = new AtomicInteger(1);
			query("INSERT INTO proxy_container_field VALUES (?, ?, ?)")
				.updateBatch(nodeIds.stream()
									.map(id -> Stream.<Object>of(proxyContainerId, id, ordinal.getAndIncrement())),
							1_000);
		}
	}

	public int deleteProxyContainerRelationships(final UUID proxyContainerUid) {
		return query("DELETE FROM proxy_container_field WHERE proxy_container = ?")
				.addArg(proxyContainerUid)
				.update();
	}

	public UUID createError(final DataFlowErrorPojoPrototype error) {
		final FieldBuilder fields = new FieldBuilder()
			.add(error.nodeId, "data_flow_node", "?")
			.add(error.severity, "severity", "?", DataFlowErrorPojo.Severity::name)
			.add(error.text, "text", "?");

		return query("INSERT INTO data_flow_error ")
				.with(fields::buildInsert)
				.append(" RETURNING data_flow_node")
				.first(rs -> (UUID) rs.getObject(1))
				.orElseThrow(() -> new MiningEntityNotFoundException(DataFlowNodePojo.class, error.toString()));
	}

	public void createErrors(final Collection<DataFlowErrorPojoPrototype> errors) {
		final var args = errors.stream()
								.map(e -> Stream.<Object>of(e.nodeId.getNonNull(), e.severity.getNonNull().name(), e.text.getNonNull()));

		query("INSERT INTO data_flow_error VALUES (?, ?, ?)")
			.updateBatch(args, 1_000);
	}

	public int deleteErrors(final BuildingConsumer<DataFlowErrorInquiryBuilder> builder) {
		return builder.prepare(new DataFlowErrorQueryBuilder())
				.buildDelete()
				.update();
	}

	public List<DataFlowErrorPojo> findErrors(final BuildingConsumer<DataFlowService.DataFlowErrorInquiryBuilder> builder) {
		return builder.prepare(new DataFlowErrorQueryBuilder())
				.build(null)
				.all();
	}

	public int setTraced(final EntityId module, final boolean traced) {
		return query("UPDATE data_flow_node SET traced = ? WHERE module IN (")
				.addArg(traced)
				.append("SELECT distinct module FROM data_flow_node WHERE id IN ("
							+ "SELECT src FROM data_flow_node_relationship WHERE type IN ('WRITE_ACCESS', 'READ_ACCESS', 'RELATED_FIELD') AND "
									+ "dst IN (SELECT id FROM data_flow_node WHERE type = 'PROXY_FIELD' AND module = ").with(ModulePgDao.referenceUidOrNid(module))
						.append(") UNION "
							+ "SELECT dst FROM data_flow_node_relationship WHERE type IN ('WRITE_ACCESS', 'READ_ACCESS', 'RELATED_FIELD') AND "
									+ "src IN (SELECT id FROM data_flow_node WHERE type = 'PROXY_FIELD' AND module = ").with(ModulePgDao.referenceUidOrNid(module))
						.append(")) AND module != ").with(ModulePgDao.referenceUidOrNid(module))
				.append(")")
				.update();
	}

	public int setTraced(final Collection<UUID> modules, final boolean traced) {
		final PgArray pgArray = arrayFromCollection(PgType.UUID, modules);
		return query("UPDATE data_flow_node SET traced = ? WHERE module IN (")
				.addArg(traced)
				.append("SELECT distinct module FROM data_flow_node WHERE id IN ("
							+ "SELECT src FROM data_flow_node_relationship WHERE type IN ('WRITE_ACCESS', 'READ_ACCESS', 'RELATED_FIELD') AND "
									+ "dst IN (SELECT id FROM data_flow_node WHERE type = 'PROXY_FIELD' AND module = any(?)").addArg(pgArray)
						.append(") UNION "
							+ "SELECT dst FROM data_flow_node_relationship WHERE type IN ('WRITE_ACCESS', 'READ_ACCESS', 'RELATED_FIELD') AND "
									+ "src IN (SELECT id FROM data_flow_node WHERE type = 'PROXY_FIELD' AND module = any(?)").addArg(pgArray)
						.append(")) AND NOT module = any(?)").addArg(pgArray)
				.append(")")
				.update();
	}
}
