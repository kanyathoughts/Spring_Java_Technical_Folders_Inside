/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.datalineage;

import innowake.mining.shared.Definable;
import innowake.mining.shared.entities.dataflow.ProxyContainerPojo;
import innowake.mining.shared.model.ModuleLocation;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Prototype used to update the definition of {@link ProxyContainerPojo}
 */
public class ProxyContainerPrototype {

	public final DataFlowId dataFlowId;

	public final Definable<ProxyContainerPojo.Type> type = new Definable<>(false, "ProxyContainer.type");
	public final Definable<Map<ProxyContainerPojo.Property, Object>> properties = new Definable<>(false, "ProxyContainer.properties");
	public final Definable<Set<DataFlowNodePrototype>> fields = new Definable<>(false, "ProxyContainer.fields");
	public final Definable<ModuleLocation> statementLocation = new Definable<>(true, "ProxyContainer.statementLocation");

	/**
	 * Constructor.
	 * @param dataFlowId Data Flow ID
	 */
	public ProxyContainerPrototype(final DataFlowId dataFlowId) {
		this.dataFlowId = dataFlowId;
	}

	/**
	 * Returns ID of DataFlow
	 *
	 * @return ID of DataFlow
	 */
	public DataFlowId getDataFlowId() {
		return dataFlowId;
	}

	/**
	 * Sets a type describing the type of this proxy container. The description is specific to the language of the parent module.
	 *
	 * @param type a type describing the type of this proxy container
	 * @return {@link ProxyContainerPrototype}
	 */
	public ProxyContainerPrototype setType(final ProxyContainerPojo.Type type) {
		this.type.set(type);
		return this;
	}

	/**
	 * Sets a map of properties of this proxy container.
	 *
	 * @param properties map of properties of this proxy container
	 * @return {@link ProxyContainerPrototype}
	 */
	public ProxyContainerPrototype setProperties(final Map<ProxyContainerPojo.Property, Object> properties) {
		this.properties.set(properties);
		return this;
	}

	/**
	 * Sets the list of fields in this container. Depending on the type of container, the order of the fields can be relevant.
	 *
	 * @param fields the list of fields in this container
	 * @return {@link ProxyContainerPrototype}
	 */
	public ProxyContainerPrototype setFields(final List<DataFlowNodePrototype> fields) {
		this.fields.set(new LinkedHashSet<>(fields));
		return this;
	}

	/**
	 * Adds a field into the fields list
	 * 
	 * @param field The field to add
	 * @return {@link ProxyContainerPrototype}
	 */
	public ProxyContainerPrototype addField(final DataFlowNodePrototype field) {
		if ( ! this.fields.isDefined()) {
			this.fields.set(new LinkedHashSet<>());
		}
		this.fields.getNonNull().add(field);
		return this;
	}

	/**
	 * Sets the location of the statement that created or is represented by this proxy container.
	 *
	 * @param statementLocation the statement location
	 * @return {@link ProxyContainerPrototype}
	 */
	public ProxyContainerPrototype setStatementLocation(final ModuleLocation statementLocation) {
		this.statementLocation.set(statementLocation);
		return this;
	}
}
