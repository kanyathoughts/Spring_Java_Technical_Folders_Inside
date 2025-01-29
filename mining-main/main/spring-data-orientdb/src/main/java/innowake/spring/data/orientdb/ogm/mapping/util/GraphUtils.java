/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.spring.data.orientdb.ogm.mapping.util;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.apache.commons.collections4.IteratorUtils;

import com.orientechnologies.orient.core.metadata.schema.OClass;
import com.orientechnologies.orient.core.record.ODirection;
import com.orientechnologies.orient.core.record.OEdge;
import com.orientechnologies.orient.core.record.OElement;
import com.orientechnologies.orient.core.record.OVertex;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.spring.data.orientdb.ogm.mapping.ClassDefinition;
import innowake.spring.data.orientdb.ogm.mapping.ObjectStruct;

/**
 * Utility class to operate on vertex and edges.
 */
public final class GraphUtils {
	
	private static final Logger LOGGER = LoggerFactory.getLogger(GraphUtils.class);
	
	private GraphUtils() {}

	/**
	 * Saves the vertex and edges in OrientDb.
	 *
	 * @param vertexNode node that needs to be saved
	 * @param oStruct object structure of an entity class
	 */
	public static void saveVertexAndEdges(final OVertex vertexNode, final ObjectStruct oStruct) {
		vertexNode.save();
		if ( ! oStruct.getEdgesToRemove().isEmpty()) {
			oStruct.getEdgesToSave().stream().forEach(edge -> {
				if (oStruct.getEdgesToRemove().contains(edge)) {
					oStruct.getEdgesToRemove().remove(edge);
				}
			});
			oStruct.getEdgesToRemove().forEach(OElement::delete);
		}
		oStruct.getEdgesToSave().forEach(OElement::save);
		LOGGER.debug(() -> "saving entity type "  + vertexNode.getSchemaType().map(OClass::getName).orElse("schema not found") 
				+ " identity " + vertexNode.getIdentity());
	}

	/**
	 * Collect all the edges to be removed. If the edge present is not attached to any vertex it's marked to be removed.
	 *
	 * @param vertexNode {@link OVertex} instance of a given entity
	 * @param classDef class definition of an entity class
	 * @param oStruct holds the values of the entity as per class definition
	 */
	public static void updateOStructWithEdgesToRemove(final OVertex vertexNode, final ClassDefinition classDef, final ObjectStruct oStruct) {
		updateOStructWithEdgesToRemove(vertexNode, classDef.getEntityEdges().entrySet(), oStruct);
		updateOStructWithEdgesToRemove(vertexNode, classDef.getCollectionOfEntityEdges().entrySet(), oStruct);
	}

	/**
	 * Collects all the vertex nodes which has no relationship in the graph and deletes them.
	 *
	 * @param oStruct object structure of an entity class
	 */
	public static void collectAndDeleteOrphanNodes(final ObjectStruct oStruct) {
		oStruct.getAllVerticesConnected().stream()
		.filter(vertex -> IteratorUtils.toList(vertex.getEdges(ODirection.BOTH).iterator()).isEmpty())
		.forEach(OVertex::delete);
	}

	private static void updateOStructWithEdgesToRemove(final OVertex vertexNode, final Set<Map.Entry<String, Field>> links,
			final ObjectStruct oStruct) {
		for (final Map.Entry<String, Field> link : links) {
			/* Get the relationship field in entity object, which is linked by an edge in graph. 
			 * If the vertex node is deleted then the edge should be removed */
			final String graphRelationName = RelationshipUtils.getGraphRelationshipName(link.getValue());
			/* Remove all the outward edges in case of update and create new edges based on the entities, this ensures us to clear orphan nodes */
			final Stream<OEdge> edgeStream = StreamSupport.stream(vertexNode.getEdges(ODirection.OUT, graphRelationName).spliterator(), false);
			edgeStream.forEach(edge -> oStruct.getEdgesToRemove().add(edge));
		}
	}
}
