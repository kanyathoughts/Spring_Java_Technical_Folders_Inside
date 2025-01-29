/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.access.postgres;

import java.sql.Connection;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.stream.Stream;

import javax.persistence.EntityNotFoundException;
import javax.persistence.PersistenceException;

import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.SingleConnectionDataSource;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.CustomPropertiesService;
import innowake.mining.shared.access.CustomPropertiesService.CustomPropertiesInquiryBuilder;
import innowake.mining.shared.access.CustomPropertiesService.CustomPropertyDefinitionInquiryBuilder;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.CustomPropertyPojo;
import innowake.mining.shared.entities.MiningPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.lang.ConditionalConsumer;
import innowake.mining.shared.model.CustomPropertyDataType;

/**
 * Postgres specific access methods for Custom Properties related data.
 */
public class CustomPropertiesPgDao extends PgDao {
	
	private static final int ENUM_UPDATE_BATCH_SIZE = 100;
	
	interface CustomPropertiesInquiryBuilderStub extends CustomPropertiesService.CustomPropertiesInquiryBuilder {
		/**
		 * Appends the reference to the {@code custom_properties} field for the current query context to the query builder.
		 * @param q Builder of the current query.
		 */
		void appendCustomPropertiesField(QueryBuilder q);
		
		/**
		 * Adds a filter clause for custom properties to the current query context.
		 * @param clause Filter clause.
		 */
		void acceptCustomPropetiesFilter(ConditionalConsumer<QueryBuilder> clause);
		
		@Override
		default CustomPropertiesInquiryBuilder withCustomProperty(final Collection<String> path, final Collection<?> values) {
			acceptCustomPropetiesFilter(q -> q.with(this::appendCustomPropertiesField).append(" #> ? = any(?)")
					.addArg(PgType.STRING, path).addArg(PgJSON.toPGobjects(values)));
			return this;
		}
		
		@Override
		default CustomPropertiesInquiryBuilder withCustomPropertyAny(final Collection<String> path, final Collection<?> values) {
			acceptCustomPropetiesFilter(q -> q.with(this::appendCustomPropertiesField).append(" #> ? @> any(?)")
					.addArg(PgType.STRING, path).addArg(PgJSON.toPGobjects(values)));
			return this;
		}
		
		@Override
		default CustomPropertiesInquiryBuilder withCustomPropertyPresent(final Collection<String> path, final boolean isPresent) {
			acceptCustomPropetiesFilter(q -> q.when(isPresent, q2 -> q2.append("NOT "))
					.append("coalesce(").with(this::appendCustomPropertiesField)
					.append(" #> ?, 'null'::jsonb) = any(ARRAY['null'::jsonb,'[]'::jsonb])").addArg(PgType.STRING, path));
			return this;
		}
		
		@Override
		default CustomPropertiesInquiryBuilder withCustomPropertyLike(final Collection<String> path, String pattern) {
			acceptCustomPropetiesFilter(q -> q.with(this::appendCustomPropertiesField).append(" #>> ? ILIKE ?").addArg(PgType.STRING, path).addArg(pattern));
			return this;
		}
	}
	
	public class CustomPropertyDefinitionQueryBuilder implements CustomPropertiesService.CustomPropertyDefinitionInquiryBuilder {
		protected final FilterStreamBuilder filters = new FilterStreamBuilder();
		
		private void queryBase(final QueryBuilder q) {
			q.append(" FROM custom_property cp LEFT JOIN custom_property cpp on cpp.id = cp.parent")
				.with(filters::build);
		}
		
		protected long buildCount() {
			return query("SELECT count(*)").with(this::queryBase)
				.first(rs -> rs.getLong(1)).orElse(0L);
		}
		
		protected List<CustomPropertyPojo> build() {
			return query("SELECT cp.id, cp.project, cp.parent, cpp.name, cp.ordinal, cp.name, cp.properties")
				.with(this::queryBase)
				.toList((rs, n) -> new CustomPropertyPojo(
						(UUID) rs.getObject(1),
						(UUID) rs.getObject(2),
						(UUID) rs.getObject(3),
						rs.getString(4),
						rs.getInt(5),
						rs.getString(6),
						PgJSON.fromPGobject(rs.getObject(7))
					));
		}
		
		protected int buildDelete() {
			return query("DELETE FROM custom_property WHERE id IN (SELECT cp.id").with(this::queryBase).append(")").update();
		}
		
		@Override
		public CustomPropertyDefinitionInquiryBuilder byId(final UUID id) {
			filters.accept(q -> q.append("cp.id = ?", id));
			return this;
		}

		@Override
		public CustomPropertyDefinitionInquiryBuilder ofProject(final EntityId projectId) {
			filters.accept(q -> q.append("cp.project = ").with(ProjectPgDao.referenceUidOrNid(projectId)));
			return this;
		}

		@Override
		public CustomPropertyDefinitionInquiryBuilder ofParent(@Nullable final UUID id) {
			if (id == null) {
				filters.accept(q -> q.append("cp.parent IS NULL"));
			} else {
				filters.accept(q -> q.append("cp.parent = ?", id));
			}
			return this;
		}

		@Override
		public CustomPropertyDefinitionInquiryBuilder withName(final String name) {
			filters.accept(q -> q.append("cp.name = ?", name));
			return this;
		}

		@Override
		public CustomPropertyDefinitionInquiryBuilder withParent(final String name) {
			filters.accept(q -> q.append("cpp.name = ?", name));
			return this;
		}
		
		private CustomPropertyDefinitionInquiryBuilder withEntity(final EntityId projectId, @Nullable final String entity, final boolean parent) {
			filters.accept(q -> q.append(parent ? "cp.parent" : "cp.id").append(" IN (SELECT property FROM custom_property_entities WHERE project = ")
				.with(ProjectPgDao.referenceUidOrNid(projectId)).when(entity, (qq, e) -> qq.append(" AND entity = ?", e).append(")")));
			return this;
		}

		@Override
		public CustomPropertyDefinitionInquiryBuilder withEntity(final EntityId projectId, @Nullable final String entity) {
			withEntity(projectId, entity, false);
			return this;
		}

		@Override
		public CustomPropertyDefinitionInquiryBuilder withParentEntity(final EntityId projectId, @Nullable final String entity) {
			withEntity(projectId, entity, true);
			return this;
		}

		@Override
		public CustomPropertyDefinitionInquiryBuilder withDataType(final CustomPropertyDataType type) {
			filters.accept(q -> q.append("cp.properties -> ? ?? ?", CustomPropertiesService.Properties.DATA_TYPE.key, type.name()));
			return this;
		}

		@Override
		public CustomPropertyDefinitionInquiryBuilder withCustomViewIndex(final int index) {
			filters.accept(q -> q.append("cp.properties ->> ? = ?", CustomPropertiesService.Properties.CUSTOM_VIEW_INDEX.key, String.valueOf(index)));
			return this;
		}
	}
	
	public CustomPropertiesPgDao(final Connection db) {
		this(new JdbcTemplate(new SingleConnectionDataSource(db, false)));
	}
	
	/**
	 * Creates a new Custom Properties data access for Postgres.
	 * @param jdbcTemplate Access to the Postgres database.
	 */
	public CustomPropertiesPgDao(final JdbcTemplate jdbcTemplate) {
		super(jdbcTemplate);
	}
	
	static Consumer<QueryBuilder> referenceEnum(final EntityId projectId, @Nullable final String name) {
		return q -> {
			q.append("(SELECT id FROM custom_enum WHERE project = ")
				.with(ProjectPgDao.referenceUidOrNid(projectId))
				.append(" AND name = ?)");
			if (name != null) {
				q.addArg(name);
			}
		};
	}
	
	/**
	 * Adds the custom properties field to a FieldBuilder.
	 * @param entity Mining entity prototype with optionally new custom property values.
	 * @param clear Whether the entity shall have only the newly defined properties.
	 * @return Consumer adding custom properties (if any defined) to a FieldBuilder.
	 */
	static Consumer<FieldBuilder> addField(final MiningPojoPrototype<?> entity, final boolean clear) {
		return fields -> fields.add(entity.customProperties, "custom_properties", 
				p -> q -> q.append(clear ? "?" : "jsonb_merge(custom_properties, ?)").addArg(PgJSON.toPGobject(p)));
	}
	
	public long countPropertyDefinitions(final BuildingConsumer<CustomPropertyDefinitionInquiryBuilder> builder) {
		return builder.prepare(new CustomPropertyDefinitionQueryBuilder()).buildCount();
	}
	
	public List<CustomPropertyPojo> findPropertyDefinitions(final BuildingConsumer<CustomPropertyDefinitionInquiryBuilder> builder) {
		return builder.prepare(new CustomPropertyDefinitionQueryBuilder()).build();
	}
	
	public UUID createProperty(final EntityId project, final String name,
			@Nullable final UUID parent, @Nullable final Integer ordinal, final Map<String, Object> properties) {
		return query("INSERT INTO custom_property (project, parent, ordinal, name, properties)"
				+ " VALUES (").with(ProjectPgDao.referenceUidOrNid(project)).append(", ?, ", parent)
			.with(ordinal == null
				? q -> q.append("coalesce((SELECT max(ordinal) FROM custom_property WHERE parent = ?) + 1, 1)", parent)
				: q -> q.append("?", ordinal))
			.append(", ?, ?) RETURNING id", name, PgJSON.toPGobject(optionalMap(properties).orElse(null)))
			.first(rs -> (UUID) rs.getObject(1))
			.orElseThrow(() -> new PersistenceException("Failed to create Custom Property '" + name +"' with parent " + parent));
	}
	
	public UUID updateProperty(final UUID id, @Nullable final String name,
			@Nullable final UUID parent, @Nullable final Integer ordinal, @Nullable final Map<String, Object> properties) {
		query("UPDATE custom_property SET ")
			.with(new FieldBuilder()
					.addNonNull(name, "name", "?")
					.addNonNull(parent, "parent", "?")
					.addNonNull(ordinal, "ordinal", "?")
					.addNonNull(PgJSON.toPGobject(properties), "properties", "jsonb_merge(properties, ?)")
				::buildUpdate)
			.append(" WHERE id = ?", id)
			.updateOrThrow(() -> new PersistenceException("Failed to change Custom Property " + id));
		return id;
	}
	
	public void assignProperty(final EntityId project, final String entity, final UUID property,
			final int ordinal, @Nullable final String name, @Nullable final Map<String, Object> properties) {
		assignProperty(project, entity, q -> q.append("?", property), ordinal, name, properties);
	}
	
	public void assignProperty(final EntityId project, final String entity, final String property,
			final int ordinal, @Nullable final String name, @Nullable final Map<String, Object> properties) {
		assignProperty(project, entity, q -> q.append("(SELECT id FROM custom_property WHERE name = ?)", property), ordinal, name, properties);
	}
	
	private void assignProperty(final EntityId project, final String entity, final Consumer<QueryBuilder> property,
			final int ordinal, @Nullable final String name, @Nullable final Map<String, Object> properties) {
		query("INSERT INTO custom_property_entities (project, entity, property, ordinal, name, properties) VALUES (")
			.with(ProjectPgDao.referenceUidOrNid(project)).append(", ?, ", entity).with(property)
			.append(", ?, ?, ?)").addArgs(ordinal, name, PgJSON.toPGobject(properties))
			.append(" ON CONFLICT (project, entity, property) DO UPDATE SET ordinal = EXCLUDED.ordinal, name = EXCLUDED.name, properties = EXCLUDED.properties")
			.updateOrThrow(() -> new PersistenceException("Failed to assign Custom Property " + property + " to " + entity + " on " + project));
	}
	
	public Map<String, List<CustomPropertyPojo>> getAssignedProperties(final EntityId project) {
		return query("SELECT cpe.entity, cpe.property, cp.project, cpe.ordinal, coalesce(cpe.name, cp.name), jsonb_merge(cp.properties, cpe.properties)"
				+ " FROM custom_property_entities cpe INNER JOIN custom_property cp on cp.id = cpe.property WHERE cpe.project = ")
			.with(ProjectPgDao.referenceUidOrNid(project))
			.toMap((rs, m) -> m.computeIfAbsent(rs.getString(1), k -> new ArrayList<CustomPropertyPojo>()).add(new CustomPropertyPojo(
					(UUID) rs.getObject(2), (UUID) rs.getObject(3), null, null,
					rs.getInt(4), rs.getString(5), PgJSON.fromPGobject(rs.getObject(6))
				)));
	}
	
	public int deleteProperties(final BuildingConsumer<CustomPropertyDefinitionInquiryBuilder> builder) {
		return builder.prepare(new CustomPropertyDefinitionQueryBuilder()).buildDelete();
	}
	
	public Optional<UUID> findEnum(final EntityId projectId, final String name) {
		return query("SELECT id FROM custom_enum WHERE project = ").with(ProjectPgDao.referenceUidOrNid(projectId))
			.append(" AND name = ?", name).first(rs -> (UUID) rs.getObject(1));
	}
	
	public UUID createEnum(final EntityId projectId, final String name) {
		return query("INSERT INTO custom_enum (project, name) VALUES (")
			.with(ProjectPgDao.referenceUidOrNid(projectId))
			.append(", ?) ON CONFLICT (project, name) DO UPDATE SET name = EXCLUDED.name RETURNING id", name)
			.first(rs -> (UUID) rs.getObject(1))
			.orElseThrow(() -> new PersistenceException("Failed to create custom Enum '" + name + "' on Project " + projectId));
	}
	
	public void deleteEnum(final EntityId projectId, final String name) {
		query("DELETE FROM custom_enum WHERE id = ")
			.with(referenceEnum(projectId, name))
			.updateOrThrow(() -> new EntityNotFoundException("Custom Enum '" + name + "' in Project " + projectId));
	}
	
	public Set<String> fetchEnumNames(final EntityId projectId) {
		return query("SELECT name FROM custom_enum WHERE project = ")
			.with(ProjectPgDao.referenceUidOrNid(projectId))
			.toSet((rs, n) -> rs.getString(1));
	}
	
	public Map<String, Set<String>> fetchEnumsAndValues(final EntityId projectId) {
		return query("SELECT a.name, b.value "
				+ "FROM custom_enum a "
				+ "INNER JOIN custom_enum_values b ON b.enum = a.id "
				+ "WHERE a.project = ")
			.with(ProjectPgDao.referenceUidOrNid(projectId))
			.append(" ORDER BY a.id, b.ordinal")
			.toMap((rs, m) -> m.computeIfAbsent(rs.getString(1), k -> new LinkedHashSet<String>()).add(rs.getString(2)));
	}
	
	public Set<String> fetchEnumValues(final EntityId projectId, final String name) {
		return query("SELECT value FROM custom_enum_values WHERE enum = ")
			.with(referenceEnum(projectId, name))
			.append(" ORDER BY ordinal")
			.toSet((rs, n) -> rs.getString(1));
	}
	
	public void renameEnumValue(final EntityId projectId, final String name, final String currentValue, final String newValue) {
		query("UPDATE custom_enum_values SET value = ? WHERE enum = ").addArg(newValue)
			.with(referenceEnum(projectId, name))
			.append(" AND value = ?", currentValue)
			.updateOrThrow(EntityNotFoundException::new);
	}

	public void putEnumValues(final EntityId projectId, final Map<String, Set<String>> enumValues) {
		query("INSERT INTO custom_enum_values (enum, value) VALUES (")
			.with(referenceEnum(projectId, null))
			.append(", ?) ON CONFLICT (enum, value) DO NOTHING")
			.updateBatch(enumValues.entrySet().stream().flatMap(entry -> entry.getValue().stream()
					.map(value -> Stream.<Object>of(entry.getKey(), value))), ENUM_UPDATE_BATCH_SIZE);
	}
	
	public void removeEnumValues(final EntityId projectId, final String name, final List<String> values) {	
		query("DELETE FROM custom_enum_values WHERE enum = ")
			.with(referenceEnum(projectId, name))
			.append(" AND VALUE = ?")
			.updateBatch(values.stream().map(Stream::<Object>of), ENUM_UPDATE_BATCH_SIZE);
	}
	
	/**
	 * Renames the auto completion value in Custom Properties where it is used.
	 * @param propertyClass Custom Property class to process.
	 * @param property Name of the Custom Property.
	 * @param currentValue Value to be replaced.
	 * @param newValue Replacement value.
	 * @return Number of entities/properties processed.
	 */
	public int renameEnumUsages(final String propertyClass, final String property, final String currentValue, final String newValue) {
		return transformCustomPropertyValueArrays(propertyClass, property, list -> list.replaceAll(s -> currentValue.equals(s) ? newValue : s));
	}
	
	/**
	 * Deletes the auto completion value from the Custom Properties where it is used.
	 * @param propertyClass Custom Property class to process.
	 * @param property Name of the Custom Property.
	 * @param values Values to be removed.
	 * @return Number of entities/properties processed.
	 */
	public int deleteEnumUsages(final String propertyClass, final String property, final List<String> values) {
		return transformCustomPropertyValueArrays(propertyClass, property, list -> list.removeIf(values::contains));
	}
	
	private int transformCustomPropertyValueArrays(final String cpClass, final String property, final Consumer<List<Object>> transform) {
		final List<Map.Entry<UUID, List<Object>>> cpValues =
			query("SELECT uid, list FROM (SELECT uid, (custom_properties #> ?) AS list FROM mining_entity) t WHERE list IS NOT NULL")
			.addArg(new String[] {cpClass, property})
			.toList((rs, n) -> new AbstractMap.SimpleEntry<>(((UUID) rs.getObject(1)), PgJSON.fromPGobject(rs.getObject(2), new ArrayList<Object>())));
		int updatedSum = 0;
		for (final var currentEntity : cpValues) {
			final var newValues = new ArrayList<>(currentEntity.getValue());
			transform.accept(newValues);
			if ( ! newValues.equals(currentEntity.getValue())) {
				updatedSum += query("UPDATE mining_entity")
					.append(" SET custom_properties = jsonb_set(custom_properties, ?, ?)", new String[] {cpClass, property}, PgJSON.toPGobject(newValues))
					.append(" WHERE uid = ?", currentEntity.getKey())
					.update();
			}
		}
		return updatedSum;
	}
	
}
