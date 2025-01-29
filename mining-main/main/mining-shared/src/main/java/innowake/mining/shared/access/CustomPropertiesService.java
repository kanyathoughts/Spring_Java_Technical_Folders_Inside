/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.function.Supplier;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.entities.CustomPropertyPojo;
import innowake.mining.shared.entities.MiningPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.lang.MapPropertyAccessor;
import innowake.mining.shared.lang.MapPropertyConverter;
import innowake.mining.shared.model.CustomPropertyDataType;
import innowake.mining.shared.model.CustomPropertyFieldType;
import innowake.mining.shared.model.CustomPropertyMetadata;

/**
 * Specifies functions for accessing the Custom Properties and their metadata in the database.
 */
public interface CustomPropertiesService {
	
	public interface CustomPropertiesInquiryBuilder {
		/**
		 * Include entries if any of the provided values matches the exact property value.
		 * For matching arbitrary elements of array properties see {@link #withCustomPropertyAny(Collection, Collection)}.
		 * @param path Path of keys identifying the custom property.
		 * @param values Values, any of which must match the property.
		 * @return This builder.
		 */
		CustomPropertiesInquiryBuilder withCustomProperty(Collection<String> path, Collection<?> values);
		/**
		 * Include entries if any of the provided values matches the property value or any of its values in case of an array.
		 * @param path Path of keys identifying the custom property.
		 * @param values Values, any of which must be found in the property.
		 * @return This builder.
		 */
		CustomPropertiesInquiryBuilder withCustomPropertyAny(Collection<String> path, Collection<?> values);
		/**
		 * Exclude entries if any of the provided values matches the property value or any of its values in case of an array.
		 * @param path Path of keys identifying the custom property.
		 * @param isPresent {@code true} if the the property value must be not null or in case of an array, the array must not be empty. Otherwise {@code false}
		 * @return This builder.
		 */
		CustomPropertiesInquiryBuilder withCustomPropertyPresent(Collection<String> path, boolean isPresent);
		/**
		 * Filter by a custom property using a string pattern.
		 * @param path Path of keys identifying the custom property.
		 * @param pattern Pattern to match.
		 * @return This builder.
		 */
		CustomPropertiesInquiryBuilder withCustomPropertyLike(Collection<String> path, String pattern);
	}
	
	public interface CustomPropertyDefinitionInquiryBuilder {
		CustomPropertyDefinitionInquiryBuilder byId(UUID id);
		CustomPropertyDefinitionInquiryBuilder ofProject(EntityId projectId);
		CustomPropertyDefinitionInquiryBuilder ofParent(@Nullable UUID id);
		CustomPropertyDefinitionInquiryBuilder withParent(String name);
		CustomPropertyDefinitionInquiryBuilder withName(String name);
		CustomPropertyDefinitionInquiryBuilder withEntity(EntityId projectId, @Nullable String entity);
		CustomPropertyDefinitionInquiryBuilder withParentEntity(EntityId projectId, @Nullable String entity);
		CustomPropertyDefinitionInquiryBuilder withDataType(CustomPropertyDataType type);
		CustomPropertyDefinitionInquiryBuilder withCustomViewIndex(int index);
	}
	
	public class Properties {
		private Properties() { }
		
		public static final MapPropertyAccessor<String>
			LABEL = new MapPropertyAccessor<>("label");
		public static final MapPropertyConverter<CustomPropertyDataType, String>
			DATA_TYPE = new MapPropertyConverter<>("dataType", CustomPropertyDataType::valueOf, CustomPropertyDataType::name);
		public static final MapPropertyConverter<CustomPropertyDataType, String>
			COLLECTION_TYPE = new MapPropertyConverter<>("collectionType", CustomPropertyDataType::valueOf, CustomPropertyDataType::name);
		public static final MapPropertyConverter<CustomPropertyFieldType, String>
			FIELD_TYPE = new MapPropertyConverter<>("fieldType", CustomPropertyFieldType::valueOf, CustomPropertyFieldType::name);
		public static final MapPropertyAccessor<String>
			DATA_SOURCE = new MapPropertyAccessor<>("dataSource");
		public static final MapPropertyAccessor<String>
			DESCRIPTION = new MapPropertyAccessor<>("description");
		public static final MapPropertyAccessor<String>
			CUSTOM_CATEGORY = new MapPropertyAccessor<>("customCategory");
		public static final MapPropertyAccessor<Boolean>
			PLUGIN_VISIBLE = new MapPropertyAccessor<>("pluginVisible");
		public static final MapPropertyAccessor<Boolean>
			MANDATORY = new MapPropertyAccessor<>("mandatory");
		public static final MapPropertyAccessor<Object>
			MIN = new MapPropertyAccessor<>("min");
		public static final MapPropertyAccessor<Object>
			MAX = new MapPropertyAccessor<>("max");
		public static final MapPropertyAccessor<Boolean>
			READ_ONLY = new MapPropertyAccessor<>("readOnly");
		public static final MapPropertyAccessor<Map<String, Object>>
			SHOW_WHEN = new MapPropertyAccessor<>("showWhen");
		public static final MapPropertyAccessor<List<String>>
			CUSTOM_VIEW_NAMES = new MapPropertyAccessor<>("customViewNames");
		public static final MapPropertyAccessor<Integer>
			CUSTOM_VIEW_INDEX = new MapPropertyAccessor<>("customViewIndex");
		public static final MapPropertyAccessor<String>
			VALIDATION_REGEX = new MapPropertyAccessor<>("validationRegex");
		public static final MapPropertyAccessor<String>
			VALIDATION_ERROR_MESSAGE = new MapPropertyAccessor<>("validationErrorMessage");
		public static final MapPropertyAccessor<String>
			AUTO_COMPLETION_KEY = new MapPropertyAccessor<>("autoCompletionKey");
	}
	
	long countPropertyDefinitions(BuildingConsumer<CustomPropertyDefinitionInquiryBuilder> builder);
	
	List<CustomPropertyMetadata> findPropertyDefinitions(BuildingConsumer<CustomPropertyDefinitionInquiryBuilder> builder);
	
	CustomPropertyMetadata getPropertyDefinition(String parent, String name);
	
	UUID defineProperty(EntityId projectId, String miningEntity, String propertyName, CustomPropertyMetadata definition);
	
	UUID defineProperty(String className, String propertyName, CustomPropertyMetadata definition);
	
	UUID updateProperty(UUID id, @Nullable Integer ordinal, @Nullable Map<String, Object> properties);
	
	void deleteProperty(EntityId projectId, String miningEntity, String propertyName);
	
	int deleteProperties(EntityId projectId);
	
	void assignProperty(EntityId project, String entity, String property);
	
	public Map<String, List<CustomPropertyPojo>> getAssignedProperties(EntityId project);
	
	String getDefaultClassName(Long projectId, String miningEntity);
	
	/**
	 * Defines a new Enum.
	 * @param projectId ID of the Project the Enum shall belong to.
	 * @param name Name for the Enum.
	 * @return ID of the created Enum.
	 */
	UUID createEnum(final EntityId projectId, final String name);
	
	/**
	 * Removes an Enum definition and its values.
	 * @param projectId ID of the Project the Enum belongs to.
	 * @param name Name of the Enum.
	 */
	void deleteEnum(final EntityId projectId, final String name);
	
	/**
	 * Gets the names of Enums defined on a Project.
	 * @param projectId ID of the Project.
	 * @return List of Enum names.
	 */
	Set<String> getEnumNames(EntityId projectId);
	
	/**
	 * Gets the Enums defined on a Project and their values.
	 * @param projectId ID of the Project.
	 * @return Map of Enum names to values.
	 */
	Map<String, Set<String>> getEnumsAndValues(final EntityId projectId);
	
	/**
	 * Gets the values defined in a certain Enum.
	 * @param projectId ID of the Project the Enum is defined on.
	 * @param name Name of the Enum.
	 * @return List of values.
	 */
	Set<String> getEnumValues(EntityId projectId, String name);
	
	/**
	 * Adds new values to Enums if they are not yet present. 
	 * @param projectId ID of the project the Enum is defined for.
	 * @param map Map of Enums and associated values.
	 */
	void putEnumValues(EntityId projectId, Map<String, Set<String>> map);
	
	/**
	 * Replaces an existing Enum values with a different one.
	 * @param projectId ID of the project the Enum is defined for.
	 * @param name Name of the Enum.
	 * @param currentValue Value to be replaced.
	 * @param newValue Value to be defined instead.
	 */
	void renameEnumValue(EntityId projectId, String name, String currentValue, String newValue);
	
	/**
	 * Removes values form an Enum.
	 * @param projectId ID of the project the Enum is defined for.
	 * @param name Name of the Enum.
	 * @param values Values to be removed.
	 */
	void removeEnumValues(EntityId projectId, String name, List<String> values);
	
	/**
	 * Validate custom properties of an entity bound to a Project.
	 * @param pojo MiningPojo optionally carrying custom properties.
	 * @param projectId ID of the project the Pojo belongs to. 
	 */
	void validateProjectBound(MiningPojoPrototype<?> pojo, Supplier<EntityId> projectId);
	
	/**
	 * Validate custom properties of an entity bound to a Module.
	 * @param pojo A MiningPojo optionally carrying custom properties.
	 * @param moduleId ID of the module the Pojo belongs to.
	 */
	void validateModuleBound(MiningPojoPrototype<?> pojo, Supplier<EntityId> moduleId);
	
	void validateCustomPropertyTags(EntityId projectId, Map<String, Object> customProperties);
	
}
