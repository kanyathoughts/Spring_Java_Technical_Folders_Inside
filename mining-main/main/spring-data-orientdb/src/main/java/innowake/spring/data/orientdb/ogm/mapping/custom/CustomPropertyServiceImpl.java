/*
 * Copyright (c) 2021 Deloitte. All rights reserved.
 */
package innowake.spring.data.orientdb.ogm.mapping.custom;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import com.orientechnologies.orient.core.db.ODatabaseSession;
import com.orientechnologies.orient.core.exception.OQueryParsingException;
import com.orientechnologies.orient.core.exception.OValidationException;
import com.orientechnologies.orient.core.id.ORecordId;
import com.orientechnologies.orient.core.metadata.schema.OClass;
import com.orientechnologies.orient.core.metadata.schema.OProperty;
import com.orientechnologies.orient.core.record.OEdge;
import com.orientechnologies.orient.core.record.OElement;
import com.orientechnologies.orient.core.record.OVertex;
import com.orientechnologies.orient.core.record.impl.ODocument;
import com.orientechnologies.orient.core.sql.executor.OResultSet;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.shared.model.CustomPropertyDataType;
import innowake.spring.data.orientdb.commons.exception.EntityProxyMappingException;

/**
 * Provides support for manipulating CustomProperty.
 */
public class CustomPropertyServiceImpl implements CustomPropertyService {
	private static final Logger LOG = LoggerFactory.getLogger(CustomPropertyServiceImpl.class);
	
	private static final String CUSTOM_PROPERTIES_QUERY = "SELECT EXPAND(OUT('HasAdditionalInfo')[@this INSTANCEOF CustomProperties]) FROM ?";
	
	private static final String HAS_ADDITIONAL_INFO = "HasAdditionalInfo";

	@Override
	public Map<String, List<CustomProperty>> readCustomProperties(final String recordId, final ODatabaseSession session) {
		final Map<String, List<CustomProperty>> customProperties = new HashMap<>();
		try (final OResultSet result = session.query(CUSTOM_PROPERTIES_QUERY, new ORecordId(recordId))) {
			result.elementStream().forEach(element -> {
				final Optional<OClass> schemaType = element.getSchemaType();
				if ( ! schemaType.isPresent()) {
					/* custom property records must have a schema class */
					LOG.error("Unable to read custom properties from " + element.getIdentity() + " for entity " + recordId + " - missing schema information");
					return;
				}
				OClass schema = schemaType.get();
				final String className = schema.getName();
				final List<CustomProperty> customProps = customProperties.computeIfAbsent(className, key -> new ArrayList<>());
				
				/* If element properties are not contained schema declared properties, the session is reloaded to get the fresh schema details */
				final Set<String> nonEdgeElementProperties = element.getPropertyNames().stream()
						.filter(name -> ! (name.startsWith("in_") || name.startsWith("out_"))).collect(Collectors.toSet());
				final Set<String> schemaProperties = schema.declaredProperties().stream().map(OProperty::getName).collect(Collectors.toSet());
				
				if ( ! schemaProperties.containsAll(nonEdgeElementProperties)) {
					LOG.debug("Schema Properties {} doesn't contain all Element Properties {}. Schema will be reloaded.",
							schemaProperties, nonEdgeElementProperties);
					session.reload();
					schema = element.getSchemaType().get();
				}
				schema.declaredProperties().forEach(property -> {
					final String propertyName = property.getName();
					/* Schema can sometimes contain stale data but an element however always have properties that are actually present.
					 * To avoid querying for an absent property we are only retrieving the properties available in the element. */
					if (nonEdgeElementProperties.contains(propertyName)) {
						/* In case the schema of the custom property class is not strict, the record can contain properties
						 * for which there is no schema information. In absence of schema information, we treat those properties as strings.
						 */
						final CustomPropertyDataType type = Optional.ofNullable(property)
								.map(prop -> CustomPropertyDataType.fromOrientType(Integer.valueOf(prop.getType().getId())))
								.orElse(CustomPropertyDataType.STRING);
						Object value = element.getProperty(propertyName);
						if (CustomPropertyDataType.REFERENCE == type && value instanceof ODocument) {
							value = ((ODocument) value).getProperty("@rid");
						}
						final CustomProperty customProperty = new CustomProperty(propertyName, value, type);
						customProps.add(customProperty);
					}
				});
			});
		}
		return customProperties;

	}

	@Override
	public void saveOrUpdate(final OElement oElement,final Map<String, List<CustomProperty>> customProperties,
			final ODatabaseSession session) {
		if (customProperties.isEmpty()) {
			return;
		}

		final String recordId = oElement.getIdentity().toString();
		final Set<String> keySet = customProperties.keySet();
		
		/* check if rid is already saved.*/
		if (oElement.getIdentity().isPersistent()) {
			try (final OResultSet result = session.query(CUSTOM_PROPERTIES_QUERY, new ORecordId(recordId))) {
				result.elementStream().forEach(element -> {
					final OClass oClass = element.getSchemaType().get();

					final String propertyClass = oClass.getName();
					final List<CustomProperty> existingProps = customProperties.get(propertyClass);

					if (existingProps != null) {
						/* already custom properties are in.*/
						for (final CustomProperty property : existingProps) {
							addProperty(element, property, propertyClass, session);
						}
						keySet.remove(propertyClass);
						element.save();
					}
				});
			}
		}
		for (final String customPropertyType : keySet) {
			if ( ! session.getMetadata().getSchema().existsClass(customPropertyType)) {
				session.reload();
			}

			if (session.getMetadata().getSchema().existsClass(customPropertyType)) {
				final OVertex customPropertyVertex = session.newVertex(customPropertyType);
				final Optional<OVertex> miningEntityVertex = oElement.asVertex();
				if (miningEntityVertex.isPresent()) {
					final OEdge newEdge = session.newEdge(miningEntityVertex.get(), customPropertyVertex, HAS_ADDITIONAL_INFO);
					customProperties.get(customPropertyType).forEach(
							customProperty -> addProperty(customPropertyVertex, customProperty, customPropertyType, session));
					customPropertyVertex.save();
					newEdge.save();
				}
			} else {
				final String errorMessage = String.format("Custom property class %s doesn't exist in database", customPropertyType);
				LOG.error(errorMessage);
				throw new OValidationException(errorMessage);
			}
		}
	}
	
	private static void addProperty(final OElement element, final CustomProperty property, final String propertyClass, final ODatabaseSession session) {
		final Optional<OClass> schemaType = element.getSchemaType();
		if (schemaType.isPresent()) {
			if ( ! schemaType.get().existsProperty(property.getName())) {
				session.reload();
			} else {
				persistProperty(element, property);
				return;
			}
			if (schemaType.get().existsProperty(property.getName())) { /* To check if the property is actually not present in the db */
				persistProperty(element, property);
				return;
			}
		}
		throw new OValidationException(String.format("Custom property %s doesn't exist in %s class", property.getName(), propertyClass));
	}
	
	private static void persistProperty(final OElement element, final CustomProperty property) {
		try {
			element.setProperty(property.getName(), CustomPropertyConverter.getPropertyValue(property));
		} catch (final IllegalArgumentException | OQueryParsingException | EntityProxyMappingException e) {
			final String errorMessage = String.format("Invalid value %s for custom property : %s", property.getValue(), property.getName());
			LOG.error(errorMessage, e);
			throw new OValidationException(errorMessage);
		}
	}
}
