/*
\ * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller.module.filter;

import cz.jirutka.rsql.parser.ast.ComparisonOperator;

/**
 * Domain that represents the individual filter option for browse modules.
 */
public class BrowseModuleFilterInputParameter {
	private final BrowseModuleProperty property;
	private final ComparisonOperator operator;
	private final Object value;
	
	/**
	 * Constructor to instantiate the parameters needed to filter the modules.
	 * 
	 * @param property the {@link BrowseModuleProperty} to filter against
	 * @param operator the operator to use for filtering
	 * @param value {@code Object} of value to compare against
	 */
	public BrowseModuleFilterInputParameter(final BrowseModuleProperty property, final ComparisonOperator operator, final Object value) {
		this.property = property;
		this.operator = operator;
		this.value = value;
	}

	/**
	 * Getter method to fetch the value of {@link BrowseModuleProperty}.
	 * 
	 * @return the {@link BrowseModuleProperty}
	 */
	public BrowseModuleProperty getProperty() {
		return property;
	}

	/**
	 * Getter method to fetch the value of operator.
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
	public Object getValue() {
		return value;
	}
	
	/**
	 * Browse Module Property to be used to filter Browse Module.
	 */
	public enum BrowseModuleProperty {
		/** The name of modules */
		NAME("name"),
		/** The technology of modules */
		TECHNOLOGY("objectTypeLink.technologyLink.name"),
		/** The type of module*/
		TYPE("objectTypeLink.typeLink.name"),
		/** The calls edge. */
		CALLS("Calls"),
		/** The Includes edge. */
		INCLUDES("Includes"),
		/** The References edge. */
		REFERENCES("References"),
		/** The ReadsWrites edge. */
		ACCESSES("ACCESSES");
		
		private final String name;
		

		private BrowseModuleProperty() {
			this.name = "";
		}

		private BrowseModuleProperty(final String name) {
			this.name = name;
		}
		
		public String getName() {
			return name;
		}
		
		/**
		 * Method to return {@link BrowseModuleProperty} from the string value.
		 *
		 * @param name String value
		 * @return {@link BrowseModuleProperty} value from the enum
		 * @throws IllegalArgumentException in case of an invalid property name
		 */
		public static BrowseModuleProperty fromName(final String name) {
			for (final BrowseModuleProperty value : values()) {
				if (value.name.equalsIgnoreCase(name)) {
					return value;
				}
			}
			throw new IllegalArgumentException("Invalid property name: " + name);
		}
	}
}
