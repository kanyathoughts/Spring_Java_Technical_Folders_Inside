/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.shared.model.datalineage;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Objects;
import java.util.HashSet;
import java.util.Set;

import innowake.mining.shared.Definable;
import innowake.mining.shared.entities.dataflow.DataFlowErrorPojoPrototype;
import innowake.mining.shared.entities.dataflow.DataFlowNodePojo;
import innowake.mining.shared.model.ModuleLocation;

/**
 * Prototype used to update the definition of {@link DataFlowNodePojo}
 */
public class DataFlowNodePrototype {

	public final DataFlowId dataFlowId;

	public final Definable<String> name = new Definable<>(false, "DataFlowNode.name");
	public final Definable<Collection<DataFlowErrorPojoPrototype>> errors = new Definable<>(false, "DataFlowNode.errors");
	public final Definable<Set<DataFlowId>> relatedFields = new Definable<>(false, "DataFlowNode.relatedFields");
	public final Definable<Set<DataFlow>> dataFlows = new Definable<>(false, "DataFlowNode.dataFlows");
	public final Definable<ModuleLocation> location = new Definable<>(true, "DataFlowNode.location");

	/**
	 * Constructor.
	 * @param dataFlowId Data Flow ID
	 */
	public DataFlowNodePrototype(final DataFlowId dataFlowId) {
		this.dataFlowId = dataFlowId;
	}

	/**
	 * Returns a {@link DataFlowId}
	 *
	 * @return {@link DataFlowId}
	 */
	public DataFlowId getDataFlowId() {
		return dataFlowId;
	}

	/**
	 * Sets the name of the data flow node. This should describe the field or statement that the node represents.
	 *
	 * @param name the name of the node
	 * @return {@link DataFlowNodePrototype}
	 */
	public DataFlowNodePrototype setName(final String name) {
		this.name.set(name);
		return this;
	}

	/**
	 * Sets the {@link DataFlowErrorPojoPrototype}
	 *
	 * @param errors collection of {@link DataFlowErrorPojoPrototype}
	 * @return {@link DataFlowNodePrototype}
	 */
	public DataFlowNodePrototype setErrors(final Collection<DataFlowErrorPojoPrototype> errors) {
		this.errors.set(errors);
		return this;
	}

	/**
	 * Adds an {@link DataFlowErrorPojoPrototype} to the collection
	 *
	 * @param error {@link DataFlowErrorPojoPrototype} object
	 * @return {@link DataFlowNodePrototype}
	 */
	public DataFlowNodePrototype addError(final DataFlowErrorPojoPrototype error) {
		if ( ! this.errors.isDefined()) {
			this.errors.set(new ArrayList<>());
		}
		this.errors.getNonNull().add(error);
		return this;
	}

	/**
	 * Sets a collection of related fields
	 *
	 * @param relatedFields the new related field
	 * @return {@link DataFlowNodePrototype}
	 */
	public DataFlowNodePrototype setRelatedFields(final Set<DataFlowId> relatedFields) {
		this.relatedFields.set(relatedFields);
		return this;
	}

	/**
	 * Adds another node as a related field. The other node should also represent a field.
	 *
	 * @param relatedField the new related field
	 * @return {@link DataFlowNodePrototype}
	 */
	public DataFlowNodePrototype addRelatedField(final DataFlowId relatedField) {
		if ( ! this.relatedFields.isDefined()) {
			this.relatedFields.set(new HashSet<>());
		}
		this.relatedFields.getNonNull().add(relatedField);
		return this;
	}

	/**
	 * Sets a collection of {@link DataFlow}
	 *
	 * @param dataFlows Set of {@link DataFlow}
	 * @return {@link DataFlowNodePrototype}
	 */
	public DataFlowNodePrototype setDataFlows(final Set<DataFlow> dataFlows) {
		this.dataFlows.set(dataFlows);
		return this;
	}

	/**
	 * Adds a {@link DataFlow} to the collection
	 *
	 * @param dataFlow the new {@link DataFlow}
	 * @return {@link DataFlowNodePrototype}
	 */
	public DataFlowNodePrototype addDataFlow(final DataFlow dataFlow) {
		if ( ! this.dataFlows.isDefined()) {
			this.dataFlows.set(new HashSet<>());
		}
		this.dataFlows.getNonNull().add(dataFlow);
		return this;
	}

	/**
	 * Sets the location of the field or statement inside the module.
	 *
	 * @param location the location of the field or statement inside the module
	 * @return {@link DataFlowNodePrototype}
	 */
	public DataFlowNodePrototype setLocation(final ModuleLocation location) {
		this.location.set(location);
		return this;
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o)
			return true;
		if (o == null || getClass() != o.getClass())
			return false;
		final DataFlowNodePrototype that = (DataFlowNodePrototype) o;
		return Objects.equals(dataFlowId, that.dataFlowId) &&
				Objects.equals(name.get(), that.name.get()) &&
				( ! errors.isDefined() || Objects.equals(errors.get(), that.errors.get())) &&
				( ! relatedFields.isDefined() || Objects.equals(relatedFields.get(), that.relatedFields.get())) &&
				( ! dataFlows.isDefined() || Objects.equals(dataFlows.get(), that.dataFlows.get())) &&
				( ! location.isDefined() || Objects.equals(location.get(), that.location.get()));
	}

	@Override
	public int hashCode() {
		return Objects.hash(dataFlowId, name.get(),
				errors.isDefined() ? errors.get() : null,
				relatedFields.isDefined() ? relatedFields.get() : null,
				dataFlows.isDefined() ? dataFlows.get() : null,
				location.isDefined() ? location.get() : null);
	}
}
