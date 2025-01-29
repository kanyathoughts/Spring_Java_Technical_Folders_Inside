/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.spring.data.orientdb.ogm.mapping;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import com.orientechnologies.orient.core.record.OElement;
import com.orientechnologies.orient.core.record.OVertex;
/**
 * Structure to support the values of an entity object.
 * It maps the name of a field with its value.
 */
public class ObjectStruct {

	private final List<OElement> edgesToSave = new ArrayList<>();
	private final List<OElement> edgesToRemove = new ArrayList<>();
	private List<OVertex> orphanVerticesManaged = new ArrayList<>();

	/**
	 * Map of basic attributes.
	 */
	private Map<String, Object> fields = new HashMap<>();

	/**
	 * Map of links to enum vertices.
	 */
	private Map<String, Object> enumLinks = new HashMap<>();
	
	/**
	 * Map of links to collection of enum vertices.
	 */
	private Map<String, Object> enumListLinks = new HashMap<>();
	
	/**
	 * Map of links to other objects.
	 */
	private Map<String, Object> entityLinks = new HashMap<>();
	
	/**
	 * Map of links to other objects related via edges.
	 */
	private Map<String, Object> entityEdgeLinks = new HashMap<>();

	/** Map of embedded fields whose class type is annotated with @Embedded.
	 */
	private Map<String, Object> embeddedFields =  new HashMap<>();

	/**
	 * Map  of links of type collection to other entity objects.
	 */
	private Map<String, Object> collectionOfEntityLinks = new HashMap<>();
	
	/**
	 * Map  of links of type collection to other entity objects via edges.
	 */
	private Map<String, Object> collectionOfEdgeEntityLinks = new HashMap<>();

	/**
	 * Returns a map of primitive and enum fields with its value in entity object.
	 * 
	 * @return the map of primitive fields with its value.
	 */
	public Map<String, Object> getFields() {
		return Collections.unmodifiableMap(fields);
	}

	/**
	 * Sets the map of primitive field of entity class with its value.
	 * 
	 * @param objectFields map of all the object fields with key being the field name
	 */
	public void setFields(final Map<String, Object> objectFields) {
		fields = objectFields;
	}
	
	/**
	 * Returns a map of enum field names in entity class with its value.
	 * 
	 * @return a map of enum field names in entity class with its value
	 */
	public Map<String, Object> getEnumLinks() {
		return Collections.unmodifiableMap(enumLinks);
	}

	/**
	 * Sets the {@link Map} which contains the enum field name and its value in entity object.
	 * 
	 * @param enumLinks map of field names with entity class
	 */
	public void setEnumLinks(final Map<String, Object> enumLinks) {
		this.enumLinks =  enumLinks;
	}

	/**
	 * Returns a map of collection of enum field names in entity class with its value.
	 * 
	 * @return a map of enum field names in entity class with its value
	 */
	public Map<String, Object> getEnumCollectionLinks() {
		return Collections.unmodifiableMap(enumListLinks);
	}

	/**
	 * Sets the {@link Map} which contains the enum field name and its value in entity object.
	 * 
	 * @param enumListLinks map of field names with entity class
	 */
	public void setEnumCollectionLinks(final Map<String, Object> enumListLinks) {
		this.enumListLinks = enumListLinks;
	}
	
	/**
	 * Returns a map of field names with "has-a" relationship in entity class with its value.
	 * 
	 * @return a map of field names with "has-a" relationship in entity class with its value
	 */
	public Map<String, Object> getLinks() {
		return Collections.unmodifiableMap(entityLinks);
	}

	/**
	 * Sets the link Map which contains the fieldName and its value with "has-a" relationship in entity class.
	 * 
	 * @param links map of field names with has-a relationship in entity class
	 */
	public void setLinks(final Map<String, Object> links) {
		this.entityLinks = links;
	}
	
	/**
	 * Returns a map of field names with "has-a" relationship in entity class with its value linked by an edge.
	 * 
	 * @return a map of field names with "has-a" relationship in entity class with its value
	 */
	public Map<String, Object> getEdgeLinks() {
		return Collections.unmodifiableMap(entityEdgeLinks);
	}

	/**
	 * Sets the link Map which contains the fieldName and its value with "has-a" relationship in entity class liked by edge.
	 * 
	 * @param links map of field names with has-a relationship in entity class
	 */
	public void setEdgeLinks(final Map<String, Object> links) {
		this.entityEdgeLinks = links;
	}

	/**
	 * Returns a map of field names with embedded relationship in entity class with its value.
	 *
	 * @return map consisting of embedded data fields values.
	 */
	public Map<String, Object> getEmbeddedFields() {
		return Collections.unmodifiableMap(embeddedFields);
	}

	/**
	 * 
	 * Sets the embedded links map which contains the fieldName and its value with "has-a" relationship in entity class.
	 *
	 * @param embeddedFields map of field names with has-a relationship in entity class
	 */
	public void setEmbeddedFields(final Map<String, Object> embeddedFields) {
		this.embeddedFields = embeddedFields;
	}

	/**
	 * Returns a map of field names with "has-a" relationship in entity class, with its corresponding value in entity object.
	 * 
	 * @return map of field names with "has-a" relationship in entity class, with its corresponding value in entity object
	 */
	public Map<String, Object> getLinkLists() {
		return Collections.unmodifiableMap(collectionOfEntityLinks);
	}

	/**
	 * Adds the field name and value of collection field with "has-a" relationship in entity class.
	 * 
	 * @param linkLists map of field link lists with has-a relationship in the entity class
	 *
	 */
	public void setLinkList(final Map<String, Object> linkLists) {
		this.collectionOfEntityLinks = linkLists;
	}
	
	/**
	 * Returns a map of field names with "has-a" relationship in entity class linked by edge, with its corresponding value in entity object.
	 * 
	 * @return map of field names with "has-a" relationship in entity class linked by edge, with its corresponding value in entity object
	 */
	public Map<String, Object> getEdgeLinkLists() {
		return Collections.unmodifiableMap(collectionOfEdgeEntityLinks);
	}

	/**
	 * Adds the field name and value of collection field with "has-a" relationship in entity class linked by edges.
	 * 
	 * @param linkLists map of field link lists with has-a relationship in the entity class linked by edges
	 *
	 */
	public void setEdgeLinkList(final Map<String, Object> linkLists) {
		this.collectionOfEdgeEntityLinks = linkLists;
	}
	
	/**
	 * Collection of {@link OElement} elements that needs to saved in DB.
	 * 
	 * @return edges that needs to be persisted in DB connecting two vertices
	 */
	public List<OElement> getEdgesToSave() {
		return edgesToSave;
	}

	/**
	 * Collection of orphan {@link OElement} elements that needs to be deleted from DB.
	 * 
	 * @return edges that needs to be deleted from DB 
	 */
	public List<OElement> getEdgesToRemove() {
		return edgesToRemove;
	}

	/**
	 * All the vertices that are connected with the entity that was mapped to this {@link ObjectStruct}
	 *
	 * @return all the vertices connected to the current vertex
	 */
	public List<OVertex> getAllVerticesConnected() {
		return Collections.unmodifiableList(orphanVerticesManaged);
	}

	/**
	 * Sets All the vertices that are connected with the entity that was mapped to this {@link ObjectStruct}
	 *
	 *@param orphanVerticesManaged the list of vertices connected to the current vertex
	 */
	public void setAllVerticesConnected(final List<OVertex> orphanVerticesManaged) {
		this.orphanVerticesManaged = orphanVerticesManaged;
	}

}
