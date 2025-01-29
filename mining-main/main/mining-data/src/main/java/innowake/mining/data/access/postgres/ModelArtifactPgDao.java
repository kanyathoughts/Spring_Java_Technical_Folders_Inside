/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.access.postgres;

import static innowake.mining.shared.model.Identification.IDENTIFIED;
import static innowake.mining.shared.model.Identification.MISSING;

import java.sql.Timestamp;
import java.time.Instant;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.model.AstNodeLocation;
import org.springframework.jdbc.core.JdbcTemplate;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.access.ModelArtifactService.ModelArtifactInquiryBuilder;
import innowake.mining.data.io.LazyModelArtifact;
import innowake.mining.data.model.discovery.ErrorMarker;
import innowake.mining.data.model.discovery.ModelAttributeMap;
import innowake.mining.data.model.discovery.ModelDeadCode;
import innowake.mining.data.model.discovery.ModelDependency;
import innowake.mining.data.model.discovery.ModelSqlStatement;
import innowake.mining.data.model.discovery.ModelStatement;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ModuleService.ModuleInquiryBuilder;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.entities.StatementPojo;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.SourceMetrics;
import innowake.mining.shared.model.StatementType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.discovery.ErrorKey;
import innowake.mining.shared.model.discovery.ResolveTargetHelper;
import innowake.mining.shared.model.discovery.Severity;

/**
 * Postgres specific access methods for the {@code module} entity.
 * <p>This DAO should be used for the discovery only when instances of {@code ModelArtifact} or {@code ModelStatement} are required. For all other cases use
 * the {@link ModuleService} and {@link ModulePgDao} instead.</p>
 */
public class ModelArtifactPgDao extends PgDao {

	/**
	 * Creates a new module access for Postgres.
	 * 
	 * @param jdbcTemplate Access to the Postgres database
	 */
	public ModelArtifactPgDao(final JdbcTemplate jdbcTemplate) {
		super(jdbcTemplate);
	}

	/**
	 * Query builder for performing queries on {@code module} entities.
	 */
	public class ModelArtifactQueryBuilder implements ModelArtifactInquiryBuilder {

		protected final FilterStreamBuilder filters = new FilterStreamBuilder();
		protected final OrderStreamBuilder order = new OrderStreamBuilder();
		
		@SuppressWarnings("unchecked")
		protected Paged.Builder<LazyModelArtifact> build(@Nullable final Pagination paging) {
			if (paging != null) {
				order.accept(q -> q.appendOrder("m.uid", SortDirection.ASCENDING));
			}
			return query("SELECT m.uid, m.nid, m.name, m.path, to_jsonb(m.location), m.identified, m.representation, m.technology, m.type, m.info,"
						/* Parent */
						+ "p.uid as parentUid, p.nid as parentNid, p.path as parentPath "
						+ "FROM module m "
						+ "LEFT JOIN module_relationship cont ON m.uid = cont.dst and cont.type = 'CONTAINS' "
						+ "LEFT JOIN module p ON p.uid = cont.src ")
					.with(filters::build)
					.with(order::build)
					.toPageable(paging, (rs, row) -> {
						final var parentUid = (UUID) rs.getObject(11);											/* parentUid */
						final var artifact = new LazyModelArtifact(ModelArtifactPgDao.this,
								parentUid == null ? null : EntityId.of(parentUid, (Long) rs.getObject(12)));	/* parentNid */

						artifact.setModuleId(EntityId.of((UUID) rs.getObject(1), (Long) rs.getObject(2)))		/* m.uid, m.nid */
								.setName(rs.getString(3))														/* m.name */
								.setPath(rs.getString(4))														/* m.path */
								.setIdentification(rs.getBoolean(6) ? IDENTIFIED : MISSING)						/* m.identified */
								.setContainsPath(rs.getString(13));												/* parentPath */

						final var location = PgJSON.fromPGobjectOrNull(rs.getObject(5), ModuleLocation.class);	/* m.location */
						if (location != null) {
							artifact.setLocation(location);
						}

						final var representation = rs.getString(7);												/* m.representation */
						if (representation != null) {
							artifact.setRepresentation(Representation.valueOf(representation));
						}

						final var technology = Technology.fromName(rs.getString(8));							/* m.technology */
						final var type = Type.fromName(rs.getString(9));										/* m.type */
						artifact.setType(ResolveTargetHelper.fromTechnologyAndType(technology, type));

						final var info = PgJSON.fromPGobjectOrNull(rs.getObject(10), Map.class);				/* m.info */
						if (info != null) {
							artifact.setMetadata(info);
						}

						artifact.validate();
						artifact.setUnmodified(false);

						return artifact;
					});
		}

		@Override
		public ModelArtifactQueryBuilder byUid(final UUID uid) {
			filters.accept(q -> q.append("m.uid = ?", uid));
			return this;
		}

		@Override
		public ModelArtifactQueryBuilder byUids(final Collection<UUID> uids) {
			if ( ! uids.isEmpty()) {
				filters.accept(q -> q.append("m.uid = any(?)", arrayFromCollection(PgType.UUID, uids)));
			}
			return this;
		}

		@Override
		public ModelArtifactQueryBuilder byNid(final Long nid) {
			filters.accept(q -> q.append("m.nid = ?", nid));
			return this;
		}

		@Override
		public ModelArtifactQueryBuilder byNids(final Collection<Long> nids) {
			if ( ! nids.isEmpty()) {
				filters.accept(q -> q.append("m.nid = any(?)", arrayFromCollection(PgType.LONG, nids)));
			}
			return this;
		}

		@Override
		public ModelArtifactQueryBuilder byId(final EntityId module) {
			return module.hasUid() ? byUid(module.getUid()) : byNid(module.getNid());
		}

		@Override
		public ModelArtifactQueryBuilder ofProject(final EntityId project) {
			filters.accept(q -> q.append("m.project = ").with(ProjectPgDao.referenceUidOrNid(project)));
			return this;
		}

		@Override
		public ModelArtifactQueryBuilder ofSource(final EntityId source) {
			filters.accept(q -> q.append("m.source = ").with(SourcePgDao.referenceUidOrNid(source)));
			return this;
		}

		@Override
		public ModelArtifactQueryBuilder withLinkHash(final String linkHash) {
			filters.accept(q -> q.append("m.link_hash = ?", linkHash));
			return this;
		}

		@Override
		public ModelArtifactQueryBuilder withTechnology(final Technology technology) {
			filters.accept(q -> q.append("m.technology = ?", technology.name()));
			return this;
		}

		@Override
		public ModelArtifactQueryBuilder withTechnologies(final Collection<Technology> technologies) {
			if ( ! technologies.isEmpty()) {
				filters.accept(q -> q.append("m.technology = any(?)",
						arrayFromCollection(PgType.STRING, technologies.stream().map(Technology::name).collect(Collectors.toList()))));
			}
			return this;
		}

		@Override
		public ModelArtifactQueryBuilder withType(final Type type) {
			filters.accept(q -> q.append("m.type = ?", type.name()));
			return this;
		}

		@Override
		public ModelArtifactQueryBuilder withTypes(final Collection<Type> types) {
			if ( ! types.isEmpty()) {
				filters.accept(q -> q.append("m.type = any(?)", arrayFromCollection(PgType.STRING, types.stream().map(Type::name).collect(Collectors.toList()))));
			}
			return this;
		}

		@Override
		public ModelArtifactQueryBuilder withTechnologiesAndTypes(final Collection<Tuple2<Technology, Type>> technologiesAndTypes) {
			filters.accept(q -> {
				if ( ! technologiesAndTypes.isEmpty()) {
					if (technologiesAndTypes.size() == 1) {
						final Tuple2<Technology, Type> tuple = technologiesAndTypes.iterator().next();
						q.append("m.technology = ? AND m.type = ?", tuple.a.name(), tuple.b.name());
					} else {
						q.append("(");
						final Iterator<Tuple2<Technology, Type>> it = technologiesAndTypes.iterator();
						while (it.hasNext()) {
							final Tuple2<Technology, Type> tuple = it.next();
							if (it.hasNext()) {
								q.append("m.technology = ? AND m.type = ? OR ", tuple.a.name(), tuple.b.name());
							} else {
								q.append("m.technology = ? AND m.type = ?)", tuple.a.name(), tuple.b.name());
							}
						}
					}
				}
			});
			return this;
		}

		@Override
		public ModelArtifactQueryBuilder withSourceRelationshipsFrom(final EntityId moduleId, final RelationshipType type) {
			filters.accept(q -> q.append("m.uid IN (SELECT dst FROM module_relationship WHERE src = ")
								 .with(ModulePgDao.referenceUidOrNid(moduleId))
								 .append(" AND type = ?::module_relationship_type)", type.name()));
			return this;
		}

		@Override
		public ModelArtifactQueryBuilder withDestinationRelationshipsTo(final EntityId moduleId, final RelationshipType type) {
			filters.accept(q -> q.append("m.uid IN (SELECT src FROM module_relationship WHERE dst = ")
					.with(ModulePgDao.referenceUidOrNid(moduleId))
					.append(" AND type = ?::module_relationship_type)", type.name()));
			return this;
		}

		@Override
		public ModelArtifactQueryBuilder withPath(final String path) {
			filters.accept(q -> q.append("m.path = ?", path));
			return this;
		}

		@Override
		public ModelArtifactQueryBuilder withPath(final String path, final boolean caseInsensitive) {
			if (caseInsensitive) {
				filters.accept(q -> q.append("LOWER(m.path) = LOWER(?)", path));
				return this;
			}
			
			return withPath(path);
		}

		@Override
		public ModelArtifactQueryBuilder withPathsSelfOrContaining(final String path, final boolean caseInsensitive) {
			if (caseInsensitive) {
				filters.accept(q -> q.append("LOWER(m.path) = LOWER(?) OR m.uid IN (SELECT dst FROM module_relationship WHERE type = 'CONTAINS' AND src IN (SELECT uid FROM module WHERE LOWER(path) = LOWER(?)))",
						path, path));
			} else {
				filters.accept(q -> q.append("m.path = ? OR m.uid IN (SELECT dst FROM module_relationship WHERE type = 'CONTAINS' AND src IN (SELECT uid FROM module WHERE path = ?))",
							path, path));
			}

			return this;
		}

		@Override
		public ModelArtifactQueryBuilder withName(final String name) {
			filters.accept(q -> q.append("m.name = ?", name));
			return this;
		}

		@Override
		public ModelArtifactQueryBuilder withNameLike(final String name) {
			filters.accept(q -> q.append("m.name LIKE ?", name));
			return this;
		}
		
		@Override
		public ModelArtifactQueryBuilder withName(final String name, final boolean caseInsensitive) {
			if (caseInsensitive) {
				filters.accept(q -> q.append("LOWER(m.name) = LOWER(?)", name));
				return this;
			}

			return withName(name);
		}

		@Override
		public ModelArtifactQueryBuilder withNames(final Collection<String> names) {
			if ( ! names.isEmpty()) {
				filters.accept(q -> q.append("m.name = any(?)", arrayFromCollection(PgType.STRING, names)));
			}
			return this;
		}

		@Override
		public ModelArtifactQueryBuilder withDescription(final String description) {
			filters.accept(q -> q.append("m.description = ?", description));
			return this;
		}

		@Override
		public ModelArtifactQueryBuilder withIdentified(final boolean identified) {
			filters.accept(q -> q.append("m.identified = ?", Boolean.valueOf(identified)));
			return this;
		}

		@Override
		public ModelArtifactQueryBuilder withRepresentation(final Representation representation) {
			filters.accept(q -> q.append("m.representation = ?", representation.toString()));
			return this;
		}

		@Override
		public ModelArtifactQueryBuilder withMetricsDate(final Instant metricsDate) {
			filters.accept(q -> q.append("m.metrics_date = ?", Timestamp.from(metricsDate)));
			return this;
		}

		@Override
		public ModelArtifactQueryBuilder withCreator(final Creator creator) {
			filters.accept(q -> q.append("m.creator = ?", creator.toString()));
			return this;
		}
	}
	
	/**
	 * Returns all {@code module} entities that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@linkplain ModuleInquiryBuilder} containing the filter criteria and sort options.
	 * @return list of {@linkplain LazyModelArtifact LazyModelArtifacts}
	 */
	public List<LazyModelArtifact> find(final BuildingConsumer<ModelArtifactInquiryBuilder> builder) {
		return builder.prepare(new ModelArtifactQueryBuilder())
						.build(null)
						.all();
	}

	public Optional<LazyModelArtifact> findOne(final BuildingConsumer<ModelArtifactInquiryBuilder> builder) {
		return builder.prepare(new ModelArtifactQueryBuilder())
						.build(Pagination.SINGLE)
						.single();
	}

	/**
	 * Resolves all {@link ModelDependency ModelDependencies} for the the given {@code moduleUid}.
	 * 
	 * @param moduleId the {@link EntityId} of the {@code model} for which dependencies must be resolved
	 * @return list of {@linkplain ModelDependency ModelDependencies}
	 */
	@SuppressWarnings("unchecked")
	public List<ModelDependency> findDependencies(final EntityId moduleId) {
		return query("SELECT m.uid, "				/* 1 */
					+ "m.nid, "						/* 2 */
					+ "m.name, "					/* 3 */
					+ "m.path, "					/* 4 */
					+ "to_jsonb(m.location), "		/* 5 */
					+ "m.identified, "				/* 6 */
					+ "m.representation, "			/* 7 */
					+ "m.technology, "				/* 8 */
					+ "m.type, "					/* 9 */
					+ "m.info,"						/* 10 */
					/* Parent */
					+ "p.uid as parentUid, p.nid as parentNid, p.path as parentPath,"			/* 11, 12, 13 */
					/* ModelDependency properties */
					+ "r.id as dep_uid,"			/* 14 */
					+ "r.dependency_binding,"		/* 15 */
					+ "r.dependency_attributes,"	/* 16 */
					+ "to_jsonb(r.src_location) "	/* 17 */
					+ "FROM module m "
					/* Parent */
					+ "LEFT JOIN module_relationship cont ON m.uid = cont.dst and cont.type = 'CONTAINS' "
					+ "LEFT JOIN module p ON p.uid = cont.src "
					/* Referenced modules */
					+ "LEFT JOIN module_relationship r ON r.dst=m.uid "
					+ "WHERE r.src = ")
					.with(ModulePgDao.referenceUidOrNid(moduleId))
					.append(" AND r.type = any(?::module_relationship_type[])")
					.addArg(PgType.STRING, RelationshipType.DEPENDENCY_TYPES)
					.toList((rs, row) -> {
						final var parentUid = (UUID) rs.getObject(11);											/* parentUid */
						final var artifact = new LazyModelArtifact(ModelArtifactPgDao.this,
								parentUid == null ? null : EntityId.of(parentUid, (Long) rs.getObject(12)));	/* parentNid */

						artifact.setModuleId(EntityId.of((UUID) rs.getObject(1), (Long) rs.getObject(2)))		/* m.uid, m.nid */
								.setName(rs.getString(3))														/* m.name */
								.setPath(rs.getString(4))														/* m.path */
								.setIdentification(rs.getBoolean(6) ? IDENTIFIED : MISSING)						/* m.identified */
								.setContainsPath(rs.getString(13));												/* parentPath */

						final var location = PgJSON.fromPGobjectOrNull(rs.getObject(5), ModuleLocation.class);	/* m.location */
						if (location != null) {
							artifact.setLocation(location);
						}

						final var representation = rs.getString(7);												/* m.representation */
						if (representation != null) {
							artifact.setRepresentation(Representation.valueOf(representation));
						}

						final var technology = Technology.fromName(rs.getString(8));							/* m.technology */
						final var type = Type.fromName(rs.getString(9));										/* m.type */
						artifact.setType(ResolveTargetHelper.fromTechnologyAndType(technology, type));

						final var info = PgJSON.fromPGobjectOrNull(rs.getObject(10), Map.class);				/* m.info */
						if (info != null) {
							artifact.setMetadata(info);
						}

						artifact.validate();
						artifact.setUnmodified(false);

						final ModelDependency dependency = new ModelDependency()
									.setLateBinding()
									.setTarget(artifact)
									.setId((UUID) rs.getObject(14));											/* dep_uid */

						final var binding = rs.getString(15);													/* r.dependency_binding */
						if (binding != null) {
							dependency.setBinding(Binding.fromName(binding));
						}
						final var attributes = rs.getString(16);												/* r.dependency_attributes */
						if (attributes != null) {
							dependency.setAttributes(ModelAttributeMap.fromString(attributes));
						}
						final var depLocation = PgJSON.fromPGobjectOrNull(rs.getObject(17), ModuleLocation.class);	/* r.src_location */
						if (depLocation != null) {
							dependency.setLocation(depLocation);
						}

						return dependency.setModified(false);
					});
	}

	/**
	 * Resolves all {@link ErrorMarker ErrorMarkers} for the the given {@code moduleUid}.
	 * 
	 * @param moduleId the {@link EntityId} of the {@code module} for which errors must be resolved
	 * @return list of {@linkplain ErrorMarker ErrorMarkers}
	 */
	public List<ErrorMarker> findErrorMarkers(final EntityId moduleId) {
		return query("SELECT module, severity, key, cause, to_jsonb(location), line FROM error_marker WHERE module = ")
				.with(ModulePgDao.referenceUidOrNid(moduleId))
				.toList((rs, row) -> {
					final var errorMarker = new ErrorMarker()
								.setModuleId((UUID) rs.getObject(1))										/* module */
								.setSeverity(Severity.fromString(rs.getString(2)))							/* severity */
								.setKey(ErrorKey.fromString(rs.getString(3)))								/* key */
								.setCause(rs.getString(5));													/* cause */

					final var location = PgJSON.fromPGobjectOrNull(rs.getObject(5), AstNodeLocation.class);	/* m.location */
					if (location != null) {
						errorMarker.setModuleLocation(location);
					}

					return errorMarker;
				});
	}

	/**
	 * Returns all {@link ModelStatement ModelStatements} that have been created for the given {@code moduleId}.
	 * <p>Statements with {@link Technology#SQL} are not included</p>
	 *
	 * @param moduleId the module id
	 * @return list of {@linkplain ModelStatement ModelStatements}
	 * @see #findSqlStatements(EntityId)
	 */
	public List<ModelStatement> findStatements(final EntityId moduleId) {
		return query("SELECT type, text FROM statement WHERE module = ")
				.with(ModulePgDao.referenceUidOrNid(moduleId))
				.toList((rs, row) -> {
					return new ModelStatement()
									.setStatementType(StatementType.fromName(rs.getString(1)))			/* type */
									.setString(rs.getString(2))											/* text */
									.setModified(false);
		});
	}

	/**
	 * Returns all {@link ModelStatement ModelStatements} that have been created for the given {@code moduleId}.
	 * <p>Only statements with {@link Technology#SQL} are included</p>
	 *
	 * @param moduleId the module id
	 * @return list of {@linkplain ModelStatement ModelStatements}
	 */
	public List<ModelSqlStatement> findSqlStatements(final EntityId moduleId) {
		return query("SELECT type, text, properties FROM statement WHERE module = ")
				.with(ModulePgDao.referenceUidOrNid(moduleId))
				.toList((rs, row) -> {
					final var statement = new ModelSqlStatement()
							.setStatementType(StatementType.fromName(rs.getString(1)))				/* type */
							.setString(rs.getString(2));											/* text */

					final var properties = PgJSON.fromPGobjectOrNull(rs.getObject(3), Map.class);	/* properties */
					/* properties map may be null. If non null then all values must be present */
					if (properties != null) {
						statement.setLength(Objects.requireNonNull((Integer) properties.get(StatementPojo.PROPERTY_KEY_SQL_LENGTH)));
						statement.setNumberOfTables(Objects.requireNonNull((Integer) properties.get(StatementPojo.PROPERTY_KEY_TABLES)));
						statement.setNumberOfDistinctTables(Objects.requireNonNull((Integer) properties.get(StatementPojo.PROPERTY_KEY_DISTINCT_TABLES)));
						statement.setCustomComplexity(Objects.requireNonNull((Integer) properties.get(StatementPojo.PROPERTY_KEY_CUSTOM_COMPLEXITY)));
						statement.setHalsteadComplexity(Objects.requireNonNull((Double) properties.get(StatementPojo.PROPERTY_KEY_HALSTEAD_COMPLEXITY)));
						statement.setHalsteadDifficulty(Objects.requireNonNull((Double) properties.get(StatementPojo.PROPERTY_KEY_HALSTEAD_DIFFICULTY)));
					}

					statement.setModified(false);

					return statement;
				});
	}

	/**
	 * Returns all {@link ModelStatement ModelStatements} that have been created for the given {@code moduleId}.
	 * <p>Only statements with {@link Technology#SQL} are included</p>
	 *
	 * @param moduleId the module id
	 * @return list of {@linkplain ModelStatement ModelStatements}
	 */
	public List<ModelDeadCode> findDeadCodes(final EntityId moduleId) {
		return query("SELECT starting_line, number_of_lines, dead_code FROM statement WHERE module = ")
				.with(ModulePgDao.referenceUidOrNid(moduleId))
				.toList((rs, row) -> {
					return new ModelDeadCode()
								.setOffset(rs.getInt(1))		/* starting_line */
								.setLength(rs.getInt(2))		/* number_of_lines */
								.setName(rs.getString(3));		/* dead_code */
				});
	}

	/**
	 * Returns all {@link ModelStatement ModelStatements} that have been created for the given {@code moduleId}.
	 * <p>Only statements with {@link Technology#SQL} are included</p>
	 *
	 * @param moduleId the module id
	 * @return list of {@linkplain ModelStatement ModelStatements}
	 */
	public Optional<SourceMetrics> findOneSourceMetrics(final EntityId moduleId) {
		return query("SELECT module, physical_lines, code_lines, comment_lines, complexity_mc_cabe, dead_code_lines FROM source_metrics WHERE module = ")
				.with(ModulePgDao.referenceUidOrNid(moduleId))
				.first(rs -> {
					return new SourceMetrics()
								.setModuleId((UUID) rs.getObject(1))				/* module */
								.setPhysicalLines((Integer) rs.getObject(2))		/* physical_lines */
								.setCodeLines((Integer) rs.getObject(3))			/* code_lines */
								.setCommentLines((Integer) rs.getObject(4))			/* comment_lines */
								.setComplexityMcCabe((Integer) rs.getObject(5))		/* complexity_mc_cabe */
								.setDeadCodeLines((Integer) rs.getObject(6))		/* dead_code_lines */
								.setModified(false);
				});
	}
}
