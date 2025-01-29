/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.access.postgres.CustomPropertiesPgDao;
import innowake.mining.data.access.postgres.ModulePgDao;
import innowake.mining.data.access.postgres.PgUtil;
import innowake.mining.data.error.ConstraintViolationException;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.event.CustomPropertiesModifiedEvent;
import innowake.mining.shared.access.CustomPropertiesService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.CustomPropertyPojo;
import innowake.mining.shared.entities.MiningPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.CustomPropertyFieldType;
import innowake.mining.shared.model.CustomPropertyMetadata;

/**
 * Central point for accessing and modifying Custom Properties and their metadata.
 */
@Service
public class CustomPropertiesServiceImpl implements CustomPropertiesService {
	
	private static final Pattern PATTERN_NON_ALPHA =  Pattern.compile("[^A-Za-z]");
	private static final Pattern PATTERN_NON_ALPHANUM = Pattern.compile("[^A-Za-z0-9]");
	
	private final CustomPropertiesPgDao cpDao;
	private final ModulePgDao moduleDao;
	
	private final ApplicationEventPublisher eventPublisher;
	
	@Autowired
	public CustomPropertiesServiceImpl(@Qualifier("postgres") final JdbcTemplate jdbcTemplate, final ApplicationEventPublisher eventPublisher) {
		this.cpDao = new CustomPropertiesPgDao(jdbcTemplate);
		this.moduleDao = new ModulePgDao(jdbcTemplate);
		this.eventPublisher = eventPublisher;
	}
	
	@Override
	public String getDefaultClassName(final Long projectId, final String miningEntity) {
		if (miningEntity.isEmpty() || PATTERN_NON_ALPHA.matcher(miningEntity).matches()) {
			throw new IllegalArgumentException("Entity name must consist of characters only");
		}
		return miningEntity + "CustomProperties" + projectId;
	}
	
	private void validatePropertyName(final String propertyName) {
		if (propertyName.isEmpty() || PATTERN_NON_ALPHANUM.matcher(propertyName).matches()) {
			throw new IllegalArgumentException("Property name must be alpha-numeric");
		}
	}
	
	@Override
	public UUID createEnum(EntityId projectId, String name) {
		return cpDao.createEnum(projectId, name);
	}
	
	@Override
	public void deleteEnum(EntityId projectId, String name) {
		cpDao.deleteEnum(projectId, name);
	}
	
	@Override
	public void putEnumValues(final EntityId projectId, final Map<String, Set<String>> enumValues) {
		cpDao.putEnumValues(projectId, enumValues);
	}
	
	@Override
	public Map<String, Set<String>> getEnumsAndValues(final EntityId projectId) {
		return cpDao.fetchEnumsAndValues(projectId);
	}
	
	@Override
	public Set<String> getEnumNames(final EntityId projectId) {
		return cpDao.fetchEnumNames(projectId);
	}
	
	@Override
	public Set<String> getEnumValues(final EntityId projectId, final String name) {
		return cpDao.fetchEnumValues(projectId, name);
	}
	
	private void onAllTagValues(final EntityId projectId, final String name, final Consumer<CustomPropertyPojo> action) {
		cpDao.getAssignedProperties(projectId).values().forEach(entity -> entity
			.forEach(cpClass -> cpDao.findPropertyDefinitions(q -> q.ofParent(cpClass.getId())).stream()
				.filter(cp -> name.equals(CustomPropertiesService.Properties.AUTO_COMPLETION_KEY.getFrom(cp.getProperties())))
				.forEach(action)));
	}
	
	@Override
	public void renameEnumValue(final EntityId projectId, final String name, final String currentValue, final String newValue) {
		if (! projectId.hasNid()) {
			throw new IllegalArgumentException("Renaming an Enum value currently rquires the project to be specified by numeric ID.");
		}
		cpDao.renameEnumValue(projectId, name, currentValue, newValue);
		onAllTagValues(projectId, name, cp -> cpDao.renameEnumUsages(cp.getParentName().orElseThrow(), cp.getName(), currentValue, newValue));
	}
	
	@Override
	public void removeEnumValues(final EntityId projectId, final String name, final List<String> values) {
		if (! projectId.hasNid()) {
			throw new IllegalArgumentException("Removing an Enum value currently rquires the project to be specified by numeric ID.");
		}
		if ( ! getEnumNames(projectId).contains(name)) {
			throw new IllegalArgumentException("No auto-completion list with key '" + name + "' on Project " + projectId + ".");
		}
		cpDao.removeEnumValues(projectId, name, values);
		onAllTagValues(projectId, name, cp -> cpDao.deleteEnumUsages(cp.getParentName().orElseThrow(), cp.getName(), values));
	}
	
	private void validate(final EntityId projectId, final Map<String, Object> properties) {
		validateCustomPropertyTags(projectId, properties);
	}
	
	@Override
	public void validateProjectBound(final MiningPojoPrototype<?> pojo, final Supplier<EntityId> projectId) {
		pojo.customProperties.ifDefined(p -> validate(projectId.get(), p));
	}
	
	@Override
	public void validateModuleBound(final MiningPojoPrototype<?> pojo, final Supplier<EntityId> moduleId) {
		pojo.customProperties.ifDefined(p -> validate(moduleDao.getProject(moduleId.get()).orElseThrow(), p));
	}
	
	@Override
	public long countPropertyDefinitions(final BuildingConsumer<CustomPropertyDefinitionInquiryBuilder> builder) {
		return cpDao.countPropertyDefinitions(builder);
	}
	
	@Override
	public List<CustomPropertyMetadata> findPropertyDefinitions(final BuildingConsumer<CustomPropertyDefinitionInquiryBuilder> builder) {
		return cpDao.findPropertyDefinitions(builder).stream().map(p -> {
			final var props = p.getProperties();
			final var meta = new CustomPropertyMetadata();
			meta.setName(p.getName());
			meta.setLabel(Properties.LABEL.optionalFrom(props).orElse(""));
			meta.setDataType(Properties.DATA_TYPE.getFrom(props));
			meta.setDataSource(Properties.DATA_SOURCE.getFrom(props));
			meta.setDescription(Properties.DESCRIPTION.getFrom(props));
			meta.setFieldType(Properties.FIELD_TYPE.getFrom(props));
			meta.setPluginVisible(Properties.PLUGIN_VISIBLE.optionalFrom(props).orElse(false));
			meta.setMandatory(Properties.MANDATORY.optionalFrom(props).orElse(false));
			meta.setMin(Properties.MIN.getFrom(props));
			meta.setMax(Properties.MAX.getFrom(props));
			meta.setReadOnly(Properties.READ_ONLY.optionalFrom(props).orElse(false));
			meta.setAutoCompletionKey(Properties.AUTO_COMPLETION_KEY.getFrom(props));
			meta.setCustomCategory(Properties.CUSTOM_CATEGORY.getFrom(props));
			meta.setShowWhen(Properties.SHOW_WHEN.optionalFrom(props).orElse(Collections.emptyMap()));
			meta.setCustomViewNames(Properties.CUSTOM_VIEW_NAMES.optionalFrom(props).orElse(Collections.emptyList()));
			meta.setCustomViewIndex(Properties.CUSTOM_VIEW_INDEX.getFrom(props));
			meta.setValidationRegex(Properties.VALIDATION_REGEX.getFrom(props));
			meta.setValidationErrorMessage(Properties.VALIDATION_ERROR_MESSAGE.getFrom(props));
			return meta;
		}).collect(Collectors.toList());
	}
	
	@Override
	public CustomPropertyMetadata getPropertyDefinition(final String parent, final String name) {
		return findPropertyDefinitions(q -> q.withParent(parent).withName(name)).stream().findFirst()
				.orElseThrow(() -> new MiningEntityNotFoundException(CustomPropertyMetadata.class, parent + "." + name));
	}
	
	@Override
	public UUID defineProperty(final EntityId projectId, final String miningEntity, final String propertyName, final CustomPropertyMetadata definition) {
		return definePropertyOn(projectId, miningEntity, getDefaultClassName(projectId.getNid(), miningEntity), propertyName, definition);
	}
	
	@Override
	public UUID defineProperty(final String className, final String propertyName, final CustomPropertyMetadata definition) {
		return definePropertyOn(EntityId.of(0L), null, className, propertyName, definition);
	}
	
	private UUID definePropertyOn(final EntityId projectId, @Nullable final String entity, final String className,
			final String propertyName, final CustomPropertyMetadata definition) {
		validatePropertyName(propertyName);
		final var dataType = Objects.requireNonNull(definition.getDataType(), "Data type must be defined");
		final var fieldType = Objects.requireNonNull(definition.getFieldType(), "Field type must be defined");
		
		final var props = new HashMap<String, Object>();
		Properties.DATA_TYPE.setIn(props, dataType);
		Properties.FIELD_TYPE.setIn(props, fieldType);
		Properties.LABEL.setIn(props, definition.getLabel());
		Properties.DESCRIPTION.nonNullTo(props, definition.getDescription());
		Properties.MANDATORY.nonNullTo(props, definition.isMandatory());
		Properties.MIN.nonNullTo(props, definition.getMin());
		Properties.MAX.nonNullTo(props, definition.getMax());
		Properties.READ_ONLY.nonNullTo(props, definition.isReadOnly());
		Properties.DATA_SOURCE.nonNullTo(props, definition.getDataSource());
		Properties.PLUGIN_VISIBLE.nonNullTo(props, definition.isPluginVisible());
		Properties.SHOW_WHEN.optionalTo(props, PgUtil.optionalMap(definition.getShowWhen()));
		Properties.CUSTOM_VIEW_NAMES.optionalTo(props, PgUtil.optionalCollection(definition.getCustomViewNames()));
		Properties.CUSTOM_VIEW_INDEX.optionalTo(props, Optional.of(definition.getCustomViewIndex()).filter(i -> i != 0));
		Properties.VALIDATION_REGEX.nonNullTo(props, definition.getValidationRegex());
		Properties.VALIDATION_ERROR_MESSAGE.optionalTo(props, PgUtil.optionalString(definition.getValidationErrorMessage()));
		Properties.AUTO_COMPLETION_KEY.nonNullTo(props, definition.getAutoCompletionKey());
		Properties.CUSTOM_CATEGORY.nonNullTo(props, definition.getCustomCategory());
		
		final var existingDef = cpDao.findPropertyDefinitions(q -> q.withParent(className).withName(propertyName)).stream().findAny();
		UUID definedProperty;
		if (existingDef.isEmpty()) {
			if ( ! definition.getName().equals(propertyName)) {
				throw new IllegalArgumentException("Property " + propertyName + " does not exist on " + className + ", to be renamed to " +
						definition.getName());
			}
			final var parent = cpDao.findPropertyDefinitions(q -> q.ofProject(projectId).withName(className)).stream()
				.findFirst().map(CustomPropertyPojo::getId).orElseGet(() -> {
					final UUID parentId = cpDao.createProperty(projectId, className, null, 0, Collections.emptyMap());
					if (entity != null) {
						cpDao.assignProperty(projectId, entity, parentId, 0, null, null);
					}
					return parentId;
				});
			ensureEnum(projectId, definition.getAutoCompletionKey());
			definedProperty = cpDao.createProperty(projectId, propertyName, parent, null, props);
		} else {
			if ( ! definition.getDataType().equals(CustomPropertiesService.Properties.DATA_TYPE.getFrom(existingDef.get().getProperties()))) {
				throw new IllegalArgumentException("Data type cannot be changed.");
			}
			if ( ! definition.getName().equals(existingDef.get().getName())
					&& countPropertyDefinitions(q -> q.withParent(className).withName(definition.getName())) > 0) {
				throw new IllegalArgumentException("A property named " + definition.getName() + " already exists on " + className);
			}
			ensureEnum(projectId, definition.getAutoCompletionKey());
			definedProperty = cpDao.updateProperty(existingDef.get().getId(), definition.getName(), null, null, props);
		}
		final var existingCustomPropertyOnCurrentViewIndex =
				cpDao.findPropertyDefinitions(q -> q.withParent(className).withCustomViewIndex(definition.getCustomViewIndex())).stream().findAny();
		if (existingCustomPropertyOnCurrentViewIndex.isPresent()) {
			final Map<String, Object> modifiableProperties = new HashMap<>(existingCustomPropertyOnCurrentViewIndex.get().getProperties());
			final int customViewIndex = existingDef.isEmpty() ? findPropertyDefinitions(q -> q.withParent(className)).size() :
					(Integer) existingDef.get().getProperties().get("customViewIndex");
			modifiableProperties.putAll(existingCustomPropertyOnCurrentViewIndex.get().getProperties());
			modifiableProperties.put("customViewIndex", customViewIndex);
			cpDao.updateProperty(existingCustomPropertyOnCurrentViewIndex.get().getId(), null, null, null, modifiableProperties);
		}
		return definedProperty;
	}
	
	@Nullable
	private UUID ensureEnum(final EntityId projectId, @Nullable final String name) {
		if (name == null) {
			return null;
		}
		return cpDao.findEnum(projectId, name).orElseGet(() -> createEnum(projectId, name));
	}

	@Override
	public UUID updateProperty(final UUID id, @Nullable final Integer ordinal, @Nullable final Map<String, Object> properties) {
		return cpDao.updateProperty(id, null, null, ordinal, properties);
	}
	
	@Override
	public void deleteProperty(final EntityId projectId, final String miningEntity, final String propertyName) {
		cpDao.deleteProperties(q -> q.ofProject(projectId).withParent(getDefaultClassName(projectId.getNid(), miningEntity)).withName(propertyName));
	}
	
	@Override
	public int deleteProperties(final EntityId projectId) {
		return cpDao.deleteProperties(q -> q.ofProject(projectId));
	}
	
	@Override
	public void assignProperty(EntityId project, String entity, String property) {
		cpDao.assignProperty(project, entity, property, 0, null, null);
		eventPublisher.publishEvent(new CustomPropertiesModifiedEvent(Optional.of(project)));
	}

	@Override
	public Map<String, List<CustomPropertyPojo>> getAssignedProperties(EntityId project) {
		return cpDao.getAssignedProperties(project);
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public void validateCustomPropertyTags(final EntityId projectId, final Map<String, Object> customProperties) {
		final Map<String, Set<String>> autoCompletionMap = new HashMap<>();
		
		for (final Map.Entry<String, Object> entry : customProperties.entrySet()) {
			final Object value = entry.getValue();
			if (! (value instanceof Map)) {
				/* we only care about tags, these can only be defined on classes represented as sub-Maps */
				continue;
			}
			final Map<String, Object> customPropertyValueMap = (Map<String, Object>) value;
			for (final Map.Entry<String, Object> customPropertyEntry : customPropertyValueMap.entrySet()) {
				validateCustomPropertyTagsEntry(entry.getKey(), customPropertyEntry, autoCompletionMap);
			}
		}
		
		if ( ! autoCompletionMap.isEmpty()) {
			putEnumValues(projectId, autoCompletionMap);
		}
	}
	
	private void validateCustomPropertyTagsEntry(final String customPropertyClassName, final Map.Entry<String, Object> customPropertyEntry,
			final Map<String, Set<String>> autoCompletionMap) {
		final String propertyName = customPropertyEntry.getKey();
		final CustomPropertyMetadata meta = getPropertyDefinition(customPropertyClassName, propertyName);
		if (CustomPropertyFieldType.TAG != meta.getFieldType()) {
			return;
		} else if (meta.getAutoCompletionKey() == null) {
			throw new ConstraintViolationException("Auto completion key must not be null");
		}
		
		final Object propertyValue = customPropertyEntry.getValue();
		if (propertyValue != null) {
			if ( ! (propertyValue instanceof List)) {
				throw new ConstraintViolationException("The Tag property should be a List of Strings, but was: " + propertyValue);
			}
			final Set<String> autoCompletions = autoCompletionMap.computeIfAbsent(meta.getAutoCompletionKey(), key -> new TreeSet<>());
			@SuppressWarnings("unchecked") final Set<String> trimmedValues = trimValues((List<String>) propertyValue);
			autoCompletions.addAll(trimmedValues);
			customPropertyEntry.setValue(trimmedValues);
		}
	}
	
	private Set<String> trimValues(final List<String> propertyValues) {
		return propertyValues.stream()
			.map(String::trim)
			.filter(Predicate.not(String::isEmpty))
			.collect(Collectors.toCollection(LinkedHashSet::new));
	}
	
}
