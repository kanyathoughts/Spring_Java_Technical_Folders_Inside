/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.data.access.postgres;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import javax.persistence.PersistenceException;

import org.springframework.jdbc.core.JdbcTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.CachingFunction;
import innowake.mining.shared.access.AstService.AstModuleRelationshipInquiryBuilder;
import innowake.mining.shared.access.AstService.AstNodeInquiryBuilder;
import innowake.mining.shared.access.AstService.AstRelationshipInquiryBuilder;
import innowake.mining.shared.access.Comperator;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.entities.ast.AstModuleRelationshipPojo;
import innowake.mining.shared.entities.ast.AstModuleRelationshipPojoPrototype;
import innowake.mining.shared.entities.ast.AstModuleRelationshipType;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.entities.ast.AstNodePojoPrototype;
import innowake.mining.shared.entities.ast.AstRelationshipPojo;
import innowake.mining.shared.entities.ast.AstRelationshipPojoPrototype;
import innowake.mining.shared.entities.ast.AstRelationshipType;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.AstNodeLocation;
import innowake.mining.shared.model.RelationshipDirection;

/**
 * Postgres specific access methods for the {@code ast_node} and related entities.
 */
public class AstPgDao extends PgDao {
	
	/**
	 * Creates a new Client entity access for Postgres.
	 * @param jdbcTemplate Access to the Postgres database.
	 */
	public AstPgDao(final JdbcTemplate jdbcTemplate) {
		super(jdbcTemplate);
	}
	
	/**
	 * Query builder for performing queries on {@code ast_node} entities.
	 */
	public class AstNodeQueryBuilder implements AstNodeInquiryBuilder {
		protected final FilterStreamBuilder filters = new FilterStreamBuilder();
		protected final OrderStreamBuilder order = new OrderStreamBuilder();
		
		@Nullable
		protected CachingFunction<UUID, AstNodePojo> cache = null;
		
		@Nullable
		protected List<UUID> joinIDs;
		
		protected int labelLimit = 0;
		
		protected Paged.Builder<AstNodePojo> build(@Nullable final Pagination paging) {
			if (joinIDs != null ) {
				if (! order.isEmpty()) {
					throw new IllegalArgumentException("Order must not be changed if IDs are to be joined");
				}
				order.accept(q -> q.append("min(ids.row)"));
			}
			return query("SELECT an.id,"						/* 1 */
						+ " (SELECT to_jsonb(module_ids) FROM (SELECT uid, nid FROM module WHERE uid = an.module) module_ids)"
							+ " as moduleIds,"					/* 2 */
						+ " to_jsonb(an.location),"						/* 3 */
						+ " an.parent,"							/* 4 */
						+ " (SELECT to_jsonb(in_module_ids) FROM (SELECT uid, nid FROM module WHERE uid = an.included_module) in_module_ids)"
							+ " as inclusionModuleIds,"			/* 5 */
						+ " an.sibling,"						/* 6 */
						+ " (SELECT id FROM ast_node WHERE parent = an.parent AND sibling > an.sibling ORDER BY sibling LIMIT 1) next_sibling,"				/* 7 */
						+ " (SELECT id FROM ast_node WHERE parent = an.parent AND sibling < an.sibling ORDER BY sibling DESC LIMIT 1) previous_sibling,"	/* 8 */
						+ " (SELECT array_agg(id ORDER BY sibling) FROM ast_node WHERE parent = an.id) children,"	/* 9 */
						+ " an.type,"							/* 10 */
						+ " an.super_types, ")					/* 11 */
				.with(q -> {									/* 12 */
					if (labelLimit < 4) {
						q.append("an.label");
					} else {
						q.append("CASE WHEN length(an.label) > ? THEN concat(substring(an.label, 1, ?), '...') ELSE an.label END", labelLimit, labelLimit - 3);
					}
				})
				.append(" label, an.properties,"						/* 13 */
						+ " count(ar.*), array_agg(ar.id), array_agg(ar.src), array_agg(ar.dst), array_agg(ar.type), array_agg(ar.label)" /* 14-19 */
					+ " FROM ast_node an"
						+ " LEFT JOIN ast_relationship ar ON ar.src = an.id OR ar.dst = an.id")
				.when(joinIDs, (q, i) -> q.append(" INNER JOIN unnest(?) WITH ORDINALITY ids (id, row) ON an.id = ids.id").addArg(PgType.UUID, i))
				.with(filters::build)
				.append(" GROUP BY an.id")
				.with(order::build)
				.toPageable(paging, (rs, row) -> new AstNodePojo(
						(UUID) rs.getObject(1),																		/* AstNode uid */
						PgJSON.fromPGobject(rs.getObject(2), EntityId.class), null, null,							/* AstNode module ids */
						PgJSON.fromPGobject(rs.getObject(3), AstNodeLocation.class),								/* AstNode location */
						mapNullable((UUID) rs.getObject(4), k -> cachableWithKey(cache, k, AstPgDao.this::get)),	/* AstNode parent */
						PgJSON.fromPGobjectOrNull(rs.getObject(5), EntityId.class), null, null,						/* AstNode inclusion callee module ids */
						rs.getInt(6),																				/* AstNode sibling ordinal */
						mapNullable((UUID) rs.getObject(7), k -> cachableWithKey(cache, k, AstPgDao.this::get)),	/* AstNode next sibling */
						mapNullable((UUID) rs.getObject(8), k -> cachableWithKey(cache, k, AstPgDao.this::get)),	/* AstNode previous sibling */
						cachableKeyedBatch(cache, streamArray(rs.getArray(9)).map(UUID.class::cast)					/* AstNode children */
								.collect(Collectors.toUnmodifiableList()), k -> find(q -> q.joiningIds(k))),
						rs.getInt(14) > 0 ? unpivotRange(rs, 15, 5).map(o -> new AstRelationshipPojo(				/* AstNode relationships */
								(UUID) o[0],
								cachableWithKey(cache, (UUID) o[1], AstPgDao.this::get),
								cachableWithKey(cache, (UUID) o[2], AstPgDao.this::get),
								AstRelationshipType.valueOf((String) o[3]),
								(String) o[4]
							)).collect(Collectors.toList()) : Collections.emptyList(), null, null,
						rs.getString(10),																			/* AstNode type */
						PgUtil.<String>streamArray(rs.getArray(11)).collect(Collectors.toSet()),					/* resolution_flags */
						rs.getString(12),																			/* AstNode label */
						PgJSON.fromPGobject(rs.getObject(13))														/* AstNode properties */
				));
		}
		
		protected QueryBuilder buildIds() {
			return query("SELECT id FROM ast_node an")
					.with(filters::build);
		}

		protected QueryBuilder buildCount() {
			return query("SELECT count(*) FROM ast_node an")
					.with(filters::build);
		}

		protected QueryBuilder buildDelete() {
			return query("DELETE FROM ast_node an")
					.with(filters::build);
		}

		@Override
		public AstNodeInquiryBuilder byId(final UUID id) {
			filters.accept(q -> q.append("an.id = ?", id));
			return this;
		}

		@Override
		public AstNodeInquiryBuilder byIds(final Collection<UUID> ids) {
			filters.accept(q -> q.append("an.id = any(?)").addArg(PgType.UUID, ids));
			return this;
		}

		@Override
		public AstNodeInquiryBuilder joiningIds(final List<UUID> ids) {
			joinIDs = ids;
			return this;
		}

		@Override
		public AstNodeInquiryBuilder ofParent(@Nullable final UUID id) {
			filters.accept(q -> {
				q.append("an.parent ");
				if (id == null) {
					q.append("IS null");
				} else {
					q.append("= ?", id);
				}
			});
			return this;
		}

		@Override
		public AstNodeInquiryBuilder ofProject(final EntityId project) {
			filters.accept(q -> q.append("an.module IN (SELECT uid FROM module WHERE project = ").with(ProjectPgDao.referenceUidOrNid(project)).append(")"));
			return this;
		}

		@Override
		public AstNodeInquiryBuilder ofModule(final EntityId module) {
			filters.accept(q -> q.append("an.module = ").with(ModulePgDao.referenceUidOrNid(module)));
			return this;
		}

		@Override
		public AstNodeInquiryBuilder ofModules(final Collection<UUID> modules) {
			filters.accept(q -> q.append("an.module = any(?)", arrayFromCollection(PgType.UUID, modules)));
			return this;
		}

		@Override
		public AstNodeInquiryBuilder withRelationshipToModule(final EntityId module, final AstModuleRelationshipType type) {
			filters.accept(q -> q.append("an.id in (SELECT node FROM ast_module_relationship WHERE module = ").with(ModulePgDao.referenceUidOrNid(module))
					.append(" AND type = ?::ast_module_relationship_type)", type.name()));
			return this;
		}
		
		@Override
		public AstNodeInquiryBuilder withRelationshipToModules(final Collection<UUID> modules, final Collection<AstModuleRelationshipType> types) {
			filters.accept(q -> q.append("an.id in (SELECT node FROM ast_module_relationship WHERE module = any(?)").addArg(PgType.UUID, modules)
					.append(" AND type = any(?::ast_module_relationship_type[]))").addArg(PgType.STRING, types));
			return this;
		}
		
		@Override
		public AstNodeInquiryBuilder withIncludedModule(@Nullable final EntityId module) {
			if (module == null) {
				filters.accept(q -> q.append("an.included_module IS NULL"));
			} else {
				filters.accept(q -> q.append("an.included_module = ").with(ModulePgDao.referenceUidOrNid(module)));
			}
			return this;
		}

		@Override
		public AstNodeInquiryBuilder withSuperTypes(final Collection<String> types) {
			filters.accept(q -> q.append("an.super_types && ?").addArg(PgType.STRING, types));
			return this;
		}

		@Override
		public AstNodeInquiryBuilder withRelationshipTypes(
				@Nullable final Collection<AstRelationshipType> nodeRelations,
				@Nullable final Collection<AstModuleRelationshipType> moduleRelations) {
			filters.accept(q -> {
				q.append("((SELECT array_agg(DISTINCT type) FROM ast_relationship WHERE src = an.id OR dst = an.id) ");
				if (nodeRelations == null || nodeRelations.isEmpty()) {
					q.append("IS").when(nodeRelations != null, qq -> qq.append(" NOT")).append(" NULL");
				} else {
					q.append("&& ?::ast_relationship_type[]").addArg(PgType.STRING, nodeRelations);
				}
				q.append(") OR ((SELECT array_agg(DISTINCT type) FROM ast_module_relationship WHERE node = an.id) ");
				if (moduleRelations == null || moduleRelations.isEmpty()) {
					q.append("IS").when(moduleRelations != null, qq -> qq.append(" NOT")).append(" NULL");
				} else {
					q.append("&& ?::ast_module_relationship_type[]").addArg(PgType.STRING, moduleRelations);
				}
				q.append(")");
			});
			return this;
		}
		
		@Override
		public AstNodeInquiryBuilder withRetracedOffset(final Comperator comperator, final int offset) {
			filters.accept(q -> q.appendComparison("(an.location).retraced_offset", comperator, offset));
			return this;
		}

		@Override
		public AstNodeInquiryBuilder withRetracedEndOffset(final Comperator comperator, final int offset) {
			filters.accept(q -> q.appendComparison("((an.location).retraced_offset + (an.location).retraced_length)", comperator, offset));
			return this;
		}
		
		@Override
		public AstNodeInquiryBuilder withAssembledOffset(final Comperator comperator, final int offset) {
			filters.accept(q -> q.appendComparison("(an.location).assembled_offset", comperator, offset));
			return this;
		}
		
		@Override
		public AstNodeInquiryBuilder withAssembledEndOffset(final Comperator comperator, final int offset) {
			filters.accept(q -> q.appendComparison("((an.location).assembled_offset + (an.location).assembled_length)", comperator, offset));
			return this;
		}

		@Override
		public AstNodeInquiryBuilder withRelationshipCount(final AstRelationshipType type, final RelationshipDirection direction,
				final Comperator comperator, final int count) {
			filters.accept(q -> {
				q.append("(SELECT count(*) FROM ast_relationship ar WHERE ");
				if (direction == RelationshipDirection.OUT) {
					q.append("ar.src = an.id");
				} else if (direction == RelationshipDirection.IN) {
					q.append("ar.dst = an.id");
				} else {
					q.append("(ar.src = an.id OR ar.dst = an.id)");
				}
				q.append(" AND ar.type = ?::ast_relationship_type) ", type.name());
				q.append(comperator.operator()).append(" ?", count);
			});
			return this;
		}

		@Override
		public AstNodeInquiryBuilder withModuleRelationshipCount(final Collection<AstModuleRelationshipType> types, final Comperator comperator, final int count) {
			filters.accept(q -> q.append("(SELECT count(*) FROM ast_module_relationship amr"
					+ " WHERE amr.node = an.id AND amr.type = any(?::ast_module_relationship_type[])) ")
				.addArg(PgType.STRING, types.stream().map(AstModuleRelationshipType::name).collect(Collectors.toList()))
				.append(comperator.operator())
				.append(" ?", count));
			return this;
		}

		@Override
		public AstNodeInquiryBuilder withTypes(final Collection<String> types) {
			filters.accept(q -> q.append("an.type = any(?)").addArg(PgType.STRING, types));
			return this;
		}

		@Override
		public AstNodeInquiryBuilder withLabel(final String label) {
			filters.accept(q -> q.append("an.label ILIKE ?", pgPattern(label)));
			return this;
		}

		@Override
		public AstNodeInquiryBuilder withParent(final BuildingConsumer<AstNodeInquiryBuilder> parentBuilder, final boolean matches) {
			filters.accept(q -> q.append("an.parent ")
								 .when( ! matches, q2 -> q2.append("NOT "))
								 .append("IN (SELECT id from ast_node an ")
								 .with(parentBuilder.prepare(new AstNodeQueryBuilder()).filters::build)
								 .append(")"));
			return this;
		}

		@Override
		public AstNodeInquiryBuilder limitLabelLength(final int length) {
			if (length <= 3) {
				throw new IllegalArgumentException("Length must be > 3");
			}
			labelLimit = length;
			return this;
		}

		@Override
		public AstNodeInquiryBuilder usingCache(final CachingFunction<UUID, AstNodePojo> cache) {
			this.cache = cache;
			return this;
		}

		@Override
		public AstNodeInquiryBuilder sortRetracedLocation() {
			this.order.accept(q -> q.append("(an.location).retraced_offset ASC, (an.location).retraced_length DESC"));
			return this;
		}
	}

	/**
	 * Query builder for performing queries on {@code ast_node} entities.
	 */
	public class AstRelationshipQueryBuilder implements AstRelationshipInquiryBuilder {
		protected final FilterStreamBuilder filters = new FilterStreamBuilder();
		
		@Nullable
		protected CachingFunction<UUID, AstNodePojo> nodeCache = null;

		protected Paged.Builder<AstRelationshipPojo> build(@Nullable final Pagination paging) {
			return query("SELECT id,"		/* 1 */
							+ " src,"		/* 2 */
							+ " dst,"		/* 3 */
							+ " type,"		/* 4 */
							+ " label"		/* 5 */
						+ " FROM ast_relationship ar")
					.with(filters::build)
					.toPageable(paging, (rs, row) -> new AstRelationshipPojo(
							(UUID) rs.getObject(1),														/* AstRelationship uid */
							cachableWithKey(nodeCache, (UUID) rs.getObject(2), AstPgDao.this::get),		/* AstRelationship src */
							cachableWithKey(nodeCache, (UUID) rs.getObject(3), AstPgDao.this::get),		/* AstRelationship dst */
							AstRelationshipType.valueOf(rs.getString(4)),								/* AstRelationship type */
							rs.getString(5)																/* AstRelationship label */
						));
		}

		@Override
		public AstRelationshipInquiryBuilder byId(final UUID id) {
			filters.accept(q -> q.append("ar.id = ?", id));
			return this;
		}

		@Override
		public AstRelationshipInquiryBuilder ofSource(final UUID src) {
			filters.accept(q -> q.append("ar.src = ?", src));
			return this;
		}

		@Override
		public AstRelationshipInquiryBuilder ofDestination(final UUID dst) {
			filters.accept(q -> q.append("ar.dst = ?", dst));
			return this;
		}

		@Override
		public AstRelationshipInquiryBuilder withTypes(final Collection<AstRelationshipType> types) {
			filters.accept(q -> q.append("ar.type = any(?::ast_relationship_type[])").addArg(PgType.STRING, types));
			return this;
		}

		@Override
		public AstRelationshipInquiryBuilder withinModule(EntityId module) {
			filters.accept(q -> q.append("ARRAY[ar.dst, ar.src] <@ (SELECT array_agg(id) FROM ast_node WHERE module =")
					.with(ModulePgDao.referenceUidOrNid(module)).append(")"));
			return this;
		}
	}
	
	/**
	 * Query builder for AST node relationships.
	 */
	public class AstModuleRelationshipQueryBuilder implements AstModuleRelationshipInquiryBuilder {
		protected final FilterStreamBuilder filters = new FilterStreamBuilder();
		
		protected Paged.Builder<AstModuleRelationshipPojo> build(@Nullable final Pagination paging) {
			return query("SELECT node, type, module FROM ast_module_relationship am")
					.with(filters::build)
					.toPageable(paging, (rs, row) -> new AstModuleRelationshipPojo(
							(UUID) rs.getObject(1),
							AstModuleRelationshipType.valueOf(rs.getString(2)),
							(UUID) rs.getObject(3)
						));
		}
		
		@Override
		public AstModuleRelationshipInquiryBuilder ofModule(final EntityId module) {
			filters.accept(q -> q.append("am.module = ").with(ModulePgDao.referenceUidOrNid(module)));
			return this;
		}
		
		@Override
		public AstModuleRelationshipInquiryBuilder ofNode(final UUID node) {
			filters.accept(q -> q.append("am.node = ?", node));
			return this;
		}
		
		@Override
		public AstModuleRelationshipInquiryBuilder withTypes(final Collection<AstModuleRelationshipType> types) {
			filters.accept(q -> q.append("am.type = any(?::ast_module_relationship_type[])").addArg(PgType.STRING, types));
			return this;
		}
	}
	
	protected AstNodePojo get(final UUID id) {
		return findAny(q -> q.byId(id)).orElseThrow();
	}
	
	/**
	 * Creates a new {@code ast_node} entity or updates an existing one.
	 *
	 * @param astNode the {@link AstNodePojoPrototype} to create or update
	 * @param isNew {@code true} to create a new {@code ast_node} entity. {@code false} to update an existing {@code ast_node} entity
	 * @return the {@link EntityId} of the created or updated {@code ast_node}.
	 */
	public UUID put(final AstNodePojoPrototype astNode, final boolean isNew) {
		final var fields = new FieldBuilder()
				.add(astNode.module.required(isNew), "module", ModulePgDao::referenceUidOrNid)
				.add(astNode.location.required(isNew), "location", AstPgDao::appendLocation)
				.add(astNode.parent, "parent", "?")
				.add(astNode.includedModule, "included_module", ModulePgDao::referenceUidOrNid)
				.add(astNode.sibling, "sibling", "?")
				.add(astNode.type.required(isNew), "type", "?")
				.add(astNode.superTypes, "super_types", PgType.STRING, isNew)
				.add(astNode.label.required(isNew), "label", "?")
				.add(astNode.properties, "properties", "?", PgJSON::toPGobject);

		final QueryBuilder query;
		if (isNew) {
			fields.add("id", "?", astNode.id.orElseNonNull(UUID::randomUUID));
			query = query("INSERT INTO ast_node ");
			fields.buildInsert(query);
		} else {
			query = query("UPDATE ast_node SET ");
			fields.buildUpdate(query);
			query.append(" WHERE id = ?", astNode.id.getNonNull());
		}

		return query.append(" RETURNING id")
				.first(rs -> (UUID) rs.getObject(1))
				.orElseThrow(() -> new PersistenceException("Failed to " + (isNew ? "create" : "update") + " ast_node: '" + astNode));
	}

	private static Consumer<PgDao.QueryBuilder> appendLocation(@Nullable final AstNodeLocation location) {
		return location == null ? q -> q.append("?").addArg(null) : 
								  q -> q.append("(?,?,?,?,?,?,?,?)::ast_node_location", 
										  location.getRetracedOffset().orElse(null), location.getRetracedLength().orElse(null),
										  location.getAssembledOffset().orElse(null), location.getAssembledLength().orElse(null),
										  location.getRootRelativeOffset().orElse(null), location.getRootRelativeLength().orElse(null),
										  location.getRootRelativeStartLineNumber().orElse(null), location.getRootRelativeEndLineNumber().orElse(null));
	}
	
	/**
	 * Returns the first {@code ast_node} entity that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain AstNodeInquiryBuilder} containing the filter criteria and sort options.
	 * @return the {@linkplain AstNodePojo AstNodePojos}
	 */
	public Optional<AstNodePojo> findAny(final BuildingConsumer<AstNodeInquiryBuilder> builder) {
		return builder.prepare(new AstNodeQueryBuilder()).build(Pagination.FIRST).first();
	}
	
	/**
	 * Returns the only to be found {@code ast_node} entity for the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain AstNodeInquiryBuilder} containing the filter criteria and sort options.
	 * @return the {@linkplain AstNodePojo AstNodePojos}
	 * @throws IllegalStateException - If more than one entity is found.
	 */
	public Optional<AstNodePojo> findOne(final BuildingConsumer<AstNodeInquiryBuilder> builder) {
		return builder.prepare(new AstNodeQueryBuilder()).build(Pagination.SINGLE).single();
	}
	
	/**
	 * Returns all {@code ast_node} entities that match with the filters in the given {@code builder}.
	 *
	 * @param paging Pagination specification.
	 * @param builder the {@linkplain AstNodeInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain AstNodePojo AstNodePojos}
	 */
	public Paged<AstNodePojo> find(final Pagination paging, final BuildingConsumer<AstNodeInquiryBuilder> builder) {
		return builder.prepare(new AstNodeQueryBuilder()).build(paging).page();
	}

	/**
	 * Returns all {@code ast_node} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain AstNodeInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain AstNodePojo AstNodePojos}
	 */
	public List<AstNodePojo> find(final BuildingConsumer<AstNodeInquiryBuilder> builder) {
		return builder.prepare(new AstNodeQueryBuilder()).build(null).all();
	}
	
	/**
	 * Returns the ids of all {@code ast_node} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain AstNodeInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of ast {@linkplain UUID UUIDs}
	 */
	public List<UUID> findIds(final BuildingConsumer<AstNodeInquiryBuilder> builder) {
		return builder.prepare(new AstNodeQueryBuilder())
				.buildIds()
				.toList((rs, row) -> (UUID) rs.getObject(1));
	}

	/**
	 * Returns all direct and deep children {@code ast_node} entities for the given {@code parent} ast node id that have any of the given {@code superTypes}.
	 *
	 * @param parent the parent ast node id whose children are to be fetched.
	 * @param superTypes the super types
	 * @return list of {@linkplain AstNodePojo AstNodePojos}
	 */
	List<AstNodePojo> findDeep(final UUID parent, final Collection<String> superTypes) {
		/* Use this as a starting point when findDeep() is required
		 * WITH RECURSIVE ast AS (
				SELECT module, parent, id, ARRAY[coalesce(sibling, 0)] siblings, type, label, ARRAY[]::uuid[] path, false loop from ast_node WHERE id = '6b6c8ebc-5630-55bd-bf14-fc78aef4c9c4'
				UNION ALL
				SELECT a.module, a.parent, a.id, p.siblings || a.sibling, a.type, a.label, p.path || a.parent, a.id = any(p.path) FROM ast_node a INNER JOIN ast p ON a.parent = p.id AND NOT p.loop
			) SELECT * FROM ast ORDER BY siblings
		 */
		throw new UnsupportedOperationException("Method findDeep() wasn't implemented yet");
	}

	/**
	 * Returns the number of {@code ast_node} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain AstNodeInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of matching {@code ast_node} entities
	 */
	public long count(final BuildingConsumer<AstNodeInquiryBuilder> builder) {
		return builder.prepare(new AstNodeQueryBuilder())
				.buildCount()
				.first(rs -> rs.getLong(1))
				.orElse(0l);
	}

	/**
	 * Deletes all {@code ast_node} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain AstNodeInquiryBuilder} containing the filter criteria and sort options.
	 * @return number of deleted {@code ast_node} entities
	 */
	public int delete(final BuildingConsumer<AstNodeInquiryBuilder> builder) {
		return builder.prepare(new AstNodeQueryBuilder())
				.buildDelete()
				.update();
	}

	public List<AstRelationshipPojo> findRelationships(final BuildingConsumer<AstRelationshipInquiryBuilder> builder) {
		return builder.prepare(new AstRelationshipQueryBuilder())
				.build(null)
				.all();
	}
	
	public List<AstModuleRelationshipPojo> findModuleRelationships(final BuildingConsumer<AstModuleRelationshipInquiryBuilder> builder) {
		return builder.prepare(new AstModuleRelationshipQueryBuilder())
				.build(null)
				.all();
	}
	
	public UUID createRelationship(final AstRelationshipPojoPrototype relationship) {
		return query("INSERT INTO ast_relationship (id, src, dst, type, label) VALUES (gen_random_uuid(), ?, ?, ?::ast_relationship_type, ?)")
				.addArgs(relationship.src.getNonNull(), relationship.dst.getNonNull(), relationship.type.getNonNull().name(), relationship.label.orElse(null))
				.append(" RETURNING id").first(rs -> (UUID) rs.getObject(1))
				.orElseThrow(() -> new PersistenceException("Failed to store AST node relationship"));
	}
	
	public void createModuleRelationship(final AstModuleRelationshipPojoPrototype relation) {
		query("INSERT INTO ast_module_relationship (module, node, type) VALUES (").with(ModulePgDao.referenceUidOrNid(relation.module.getNonNull()))
			.append(", ? ,?::ast_module_relationship_type)", relation.node.get(), relation.type.getNonNull().name())
			.updateOrThrow(() -> new PersistenceException("Failed to store AST node to Module relationship"));
	}
	
	public int deleteModuleRelationships(EntityId moduleId, Collection<AstModuleRelationshipType> types) {
		return query("DELETE FROM ast_module_relationship WHERE module = ").with(ModulePgDao.referenceUidOrNid(moduleId))
				.append(" AND type = any(?::ast_module_relationship_type[])").addArg(PgType.STRING, types).update();
	}
	
	public int deleteRelationshipsByModule(EntityId moduleId, Collection<AstRelationshipType> types) {
		return query("DELETE FROM ast_relationship r USING ast_node n WHERE (r.src = n.id OR r.dst = n.id) AND n.module = ")
				.with(ModulePgDao.referenceUidOrNid(moduleId))
				.append(" AND r.type = any(?::ast_relationship_type[])").addArg(PgType.STRING, types)
				.update();
	}
	
}
