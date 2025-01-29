/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model.dependency.graph;

import org.apache.commons.collections4.map.MultiKeyMap;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.TypeIdentifier;

/**
 * A combination of a module's {@link Technology} and {@link Type}
 * to aid in Dependency Graph filtering.
 */
public class NodeType {
	
	private static final String TECHNOLOGY_KEY = "technology";
	private static final String TYPE_KEY = "type";
	private static final MultiKeyMap<TypeIdentifier, NodeType> NODE_TYPE_MAP = new MultiKeyMap<>();
	private static final ObjectMapper objectMapper = new ObjectMapper();
	private final Type type;
	private final Technology technology;
	
	/**
	 * Method to return an object of {@link NodeType} to aid in graph filtering.
	 *
	 * @param type {@link Type} value for a module
	 * @param technology {@link Technology} for a module
	 * @return an instance of {@link NodeType} based on the values provided
	 */
	public static NodeType of(final Technology technology, final Type type) {
		if ( ! NODE_TYPE_MAP.containsKey(technology, type)) {
			NODE_TYPE_MAP.put(technology, type, new NodeType(type, technology));
		}
		return NODE_TYPE_MAP.get(technology, type);
	}
	
	/**
	 * Method to return an object of {@link NodeType} to aid in graph filtering
	 * after obtaining their values from the {@link String} parameters.
	 *
	 * @param nodeType {@link String} value that represents {@link NodeType}.
	 * 					Expected Structure: "{@link Technology}_{@link Type}" 
	 * @return corresponding {@link NodeType} value
	 */
	public static NodeType of(final String nodeType) {
		final String[] argSplit = nodeType.split("_", 2);
		return of(Technology.fromName(argSplit[0]), Type.fromName(argSplit[1]));
	}

	/**
	 * Creates the JSON value using Object Mapper.
	 *
	 * @return JSON value of NodeType.
	 */
	@JsonValue
	public JsonNode toValue() {
		final ObjectNode jsonNode = objectMapper.createObjectNode();
		jsonNode.put(TYPE_KEY, type.toString());
		jsonNode.put(TECHNOLOGY_KEY, technology.toString());
		return jsonNode;
	}
	
	/**
	 * Gets the NodeType object from the JSON value.
	 *
	 * @param jsonNode JSON value of NodeType
	 * @return NodeType object created from JSON value
	 */
	@JsonCreator
	public static NodeType getNodeTypeFromJson(final JsonNode jsonNode) {
		return NodeType.of(Technology.valueOf(jsonNode.get(TECHNOLOGY_KEY).asText())
				, Type.valueOf(jsonNode.get(TYPE_KEY).asText()));
	}
	
	/**
	 * Getter to fetch the {@link Type} value.
	 *
	 * @return the {@link Type} value
	 */
	public Type getType() {
		return type;
	}

	/**
	 * Getter to fetch the {@link Technology} value.
	 *
	 * @return the {@link Technology} value
	 */
	public Technology getTechnology() {
		return technology;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result  = 1;
		result = prime * result + this.technology.hashCode();
		result = prime * result + this.type.hashCode();
		return result;
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj instanceof NodeType) {
			final NodeType nodeTypeObj = (NodeType) obj;
			if (this.technology.equals(nodeTypeObj.getTechnology())
				&& this.type.equals(nodeTypeObj.getType())) {
				return true;
			}
		}
		return false;
	}

	private NodeType(final Type type, final Technology technology) {
		this.type = type;
		this.technology = technology;
	}
}
