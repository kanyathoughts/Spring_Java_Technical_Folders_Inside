/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.ogm.mapping.util;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.function.BinaryOperator;

import org.apache.commons.lang.StringUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.springdata.annotations.Relationship;

/**
 * Utility for Relationship annotation processing.
 */
public final class RelationshipUtils {
	
	private static final Map<Field, String> relationShipCache = new WeakHashMap<>();
	
	private static final BinaryOperator<String> GET_GRAPH_RELATION_NAME = (a, b) -> a + "_" + b;

	private RelationshipUtils() {}
	
	/**
	 * Get the graphRelationShipName which returns edge name with fieldName_entityName if @Relationship is not present
	 * and if @Relationship annotation is present then it returns the value passed in annotation.
	 *
	 * @param field field
	 * @return relationship name
	 */
	public static synchronized String getGraphRelationshipName(final Field field) {
		if( ! relationShipCache.containsKey(field)) {
			@Nullable final Relationship edge = field.getAnnotation(Relationship.class);
	        if (edge != null && StringUtils.isNotEmpty(edge.toString())) {
	        	relationShipCache.put(field, edge.name());
	        } else {
	        	relationShipCache.put(field, GET_GRAPH_RELATION_NAME.apply(field.getDeclaringClass().getSimpleName(), field.getName()));
	        }
		}
		return relationShipCache.get(field);
	}
	
}
