/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller.module.filter;

import java.util.List;

import cz.jirutka.rsql.parser.ast.ComparisonOperator;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.dependency.graph.NodeType;

/**
 * Domain that represents the individual filter option.
 */
public class DependencyGraphFilterInputParameter implements IDependencyGraphFilterParameter {
	
	private final GraphProperty property;
	private final ComparisonOperator operator;
	private final List<Object> value;

	/**
	 * Constructor to instantiate the parameters needed to filter the graph.
	 * These parameters can be either {@link NodeType} or {@link RelationshipType}
	 * 
	 * @param property the {@link GraphProperty} to filter against
	 * @param operator the {@link ComparisonOperator} to use for filtering
	 * @param value {@code List} of value to compare against
	 */
	public DependencyGraphFilterInputParameter(final GraphProperty property, final ComparisonOperator operator, final List<Object> value) {
		this.property = property;
		this.operator = operator;
		this.value = value;
	}

	/**
	 * Getter method to fetch the value of {@link GraphProperty}.
	 * 
	 * @return the {@link GraphProperty}
	 */
	public GraphProperty getProperty() {
		return property;
	}

	/**
	 * Getter method to fetch the value of {@link ComparisonOperator}.
	 * 
	 * @return the {@link ComparisonOperator}
	 */
	public ComparisonOperator getOperator() {
		return operator;
	}

	/**
	 * Getter method to fetch the value to compare against.
	 * 
	 * @return the value to compare against
	 */
	public List<Object> getValue() {
		return value;
	}

	/**
	 * Graph Property to be used to filter Dependency Graph
	 */
	public enum GraphProperty {
		/** The types of modules */
		MODULES,
		/** The types of relationships between the nodes */
		RELATIONSHIPS;
		
		/**
		 * Method to return {@link GraphProperty} from the string value.
		 *
		 * @param name String value
		 * @return {@link GraphProperty} value from the enum
		 */
		public static GraphProperty fromName(final String name) {
			for (final GraphProperty value : values()) {
				if (value.name().equalsIgnoreCase(name)) {
					return value;
				}
			}
			throw new IllegalArgumentException("Invalid property name: " + name);
		}
	}
}
