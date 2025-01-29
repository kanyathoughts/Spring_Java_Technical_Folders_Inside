/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.model.discovery;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import com.google.common.base.MoreObjects;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.data.model.discovery.attribute.ModelAttributeKey;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleLocation;

/**
 *
 * This class is used to represent a dependency that belongs to a model artifact.
 */
public class ModelDependency implements Serializable {

	@Nullable private ModelArtifact target;
	@Nullable private Binding binding;
	private ModelAttributeMap<Object> attributeMap = new ModelAttributeMap<>();

	private transient boolean checkForDuplicates;

	@Nullable protected ModuleLocation fromLocation;
	private int fromLocationHash;

	private boolean isModified;

	@Nullable
	private UUID id;

	private final List<ModelArtifact> conditionalDependencies = new ArrayList<>();

	/**
	 * Sets checks for duplicates while creating dependencies.
	 *
	 * @param checkForDuplicates {@code true} to check if the reference already exists. Otherwise {@code false}
	 * @return modelDependency {@link ModelDependency}
	 */
	public ModelDependency setCheckForDuplicates(final boolean checkForDuplicates) {
		this.checkForDuplicates = checkForDuplicates;
		return this;
	}

	/**
	 * Duplicates check while creating dependencies
	 *
	 * @return boolean {@code true} to check if the reference already exists. Otherwise {@code false}
	 */
	public boolean isCheckForDuplicates() {
		return checkForDuplicates;
	}

	/**
	 * @return {@code true} if this {@link ModelDependency} was modified. Otherwise {@code false}.
	 */
	public boolean isModified() {
		return isModified || fromLocationHash != (fromLocation == null ? -1 : Assert.assertNotNull(fromLocation).hashCode());
	}

	/**
	 * Sets the modified state of this {@link ModelDependency} to the given {@code modified} value.
	 *
	 * @param modified the modified value
	 * @return this {@link ModelDependency} instance for method chaining
	 */
	public ModelDependency setModified(final boolean modified) {
		isModified = modified;
		fromLocationHash = fromLocation == null ? -1 : Assert.assertNotNull(fromLocation).hashCode();
		return this;
	}

	public ModelDependency() {
		isModified = true;
	}

	public ModelDependency(final ModelDependency dependency) {
		setFromDependency(dependency);
		id = dependency.id;
	}

	@Override
	public String toString() {
		return MoreObjects.toStringHelper(this)
				   .omitNullValues()
				   .add("id", id)
				   .add("target", target)
				   .add("binding", binding)
				   .add("attributes", attributeMap)
				   .add("conditionalDependency", conditionalDependencies)
				   .toString();
	}

	@Override
	public boolean equals(@Nullable final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}

		final ModelDependency other = (ModelDependency) obj;
		return binding == other.binding && Objects.equals(target, other.target) && equalsCollections(conditionalDependencies, other.conditionalDependencies)
				&& equalsAttributes(other.attributeMap);
	}

	@Override
	public int hashCode() {
		return Objects.hash(target, binding);
	}

	/**
	 *
	 * Return the target (i.e. the dependency).
	 *
	 * @return the dependency target.
	 */
	public ModelArtifact getTarget() {
		validate();
		return Assert.assertNotNull(target, "Target must not be NULL");
	}

	/**
	 *
	 * Return the binding type {@link Binding}.
	 *
	 * @return the binding type of the dependency.
	 */
	public Binding getBinding() {
		validate();
		return Assert.assertNotNull(binding, "Binding must not be NULL");
	}

	/*--- Set Methods ---*/
	/**
	 * Set the dependency artifact.
	 * <p>When setting a {@ode target} then this {@link ModelDependency} is considered as modified without checking if the {@link ModelArtifact} has actually
	 * changed or not. This results in the deletion of the existing reference edge and the creation of a new one. You should call this method on a persisted
	 * {@link ModelDependency} only when necessary for performance reasons.</p>
	 *
	 * @param target the Artifact that we bind as dependency
	 * @return the ModelDependency instance
	 */
	public ModelDependency setTarget(final ModelArtifact target) {
		this.target = target;
		isModified = true;
		return this;
	}

	/**
	 * Set the binding type from the enum {@code Binding}.
	 * Mandatory argument.
	 *
	 * @param binding The binding type.
	 * @return The ModelDependency instance.
	 */
	public ModelDependency setBinding(final Binding binding) {
		if (this.binding != binding) {
			this.binding = binding;
			isModified = true;
		}
		return this;
	}

	/**
	 * Set the binding type to {@code Binding#LATE}.
	 *
	 * @return The ModelDependency instance.
	 */
	public ModelDependency setLateBinding() {
		return setBinding(Binding.LATE);
	}

	/**
	 * Set the binding to {@code Binding#EARLY}.
	 *
	 * @return The ModelDependency instance.
	 */
	public ModelDependency setEarlyBinding() {
		return setBinding(Binding.EARLY);
	}

	public ModelDependency setFromDependency(final ModelDependency dependency) {
		dependency.validate();
		this.target = dependency.getTarget();
		this.binding = dependency.binding;
		this.attributeMap = dependency.attributeMap;
		isModified = true;
		return this;
	}

	/**
	 * Adds an attribute to the model's attribute map.
	 *
	 * @param key The key for the attribute.
	 * @param value The value for the attribute.
	 * @return This instance.
	 */
	public ModelDependency addAttribute(final ModelAttributeKey key, final Object value) {
		final Object oldValue = attributeMap.put(key, value);
		if (isModified || ! Objects.equals(value, oldValue) && ! equalsCollections(value, oldValue)) {
			isModified = true;
		}

		return this;
	}

	static boolean equalsCollections(final Object newValue, final Object oldValue) {
		if (newValue instanceof Collection && oldValue instanceof Collection) {
			final Collection<?> oldCollection = (Collection<?>) oldValue;
			final Collection<?> newCollection = (Collection<?>) newValue;
			if (oldCollection.size() != newCollection.size()) {
				return false;
			}

			/* Do not consider the order. Test that for each element in oldCollection is one finding in newCollection
			 * Remove finding from new Collection to track if duplicates are used */
			final Set<?> newSet = new HashSet<>(newCollection);
			return oldCollection.stream().allMatch(oldElem -> oldElem != null && newSet.remove(oldElem.toString()) || newSet.remove(oldElem))
						&& newSet.isEmpty();
		}

		return false;
	}

	/**
	 * Retrieves an attribute of the model's attribute map if the key exists.
	 *
	 * @param key The key for the attribute.
	 * @return The value to which the specified key is mapped, or null if this map contains no mapping for the key
	 */
	@Nullable
	public Object getAttribute(final ModelAttributeKey key) {
		return attributeMap.get(key);
	}

	/**
	 * @param attributeMap The attributes for the dependency.
	 * @return The ModelDependency after setting the attributes.
	 */
	public ModelDependency setAttributes(final ModelAttributeMap<Object> attributeMap) {
		if (isModified || ! this.attributeMap.entrySet().equals(attributeMap.entrySet())) {
			this.attributeMap = attributeMap;
			isModified = true;
		}
		return this;
	}

	/**
	 * Returns an unmodifiable map containing all attributes for the dependency.
	 *
	 * @return The attributes for the dependency.
	 */
	public Map<ModelAttributeKey, Object> getAttributes() {
		return Collections.unmodifiableMap(attributeMap);
	}

	/**
	 * Sets the from location of the model dependency.
	 *
	 * @param moduleLocation the from location of the model dependency
	 * @return the Model Dependency instance
	 */
	public ModelDependency setLocation(final ModuleLocation moduleLocation) {
		if (isModified || ! moduleLocation.equals(this.fromLocation)) {
			this.fromLocation = moduleLocation;
			isModified = true;
		}
		return this;
	}

	/**
	 * Returns the from location of the model dependency.
	 *
	 * @return The from location of the model dependency
	 */
	public  Optional<ModuleLocation> getLocation() {
		return Optional.ofNullable(fromLocation);
	}

	/**
	 * @return the id of the module reference.
	 */
	@Nullable
	public UUID getId() {
		return id;
	}

	/**
	 * Sets the id of this {@link ModelDependency}
	 *
	 * @param id the dependency id
	 * @return the Model Dependency instance
	 */
	public ModelDependency setId(final UUID id) {
		this.id = id;
		return this;
	}

	/**
	 * Adds the conditional dependency to the model dependency.
	 *
	 * @param  modelArtifact the conditional dependency.
	 * @return The ModelDependency after adding the conditional dependency.
	 */
	public ModelDependency addConditionalDependency(final ModelArtifact modelArtifact) {
		conditionalDependencies.add(modelArtifact);
		return this;
	}

	/**
	 * Returns all the conditional dependencies.
	 *
	 * @return the conditional dependencies.
	 */
	public List<ModelArtifact> getConditionalDependencies() {
		return conditionalDependencies;
	}

	/**
	 *
	 * Used in conjunction with builder pattern, this validate method can
	 * be used to ensure the object has all the necessary members set and
	 * in addition will assign appropriate default values as needed.
	 * <br>
	 * Should only be used when all builder methods have been called and object
	 * is appropriately "built".
	 *
	 * @return a reference to this object
	 */
	public ModelDependency validate() {
		if( target == null ) {
			throw new IllegalArgumentException("The argument 'target' must be set.");
		}

		if ( binding == null ) {
			throw new IllegalArgumentException("The argument 'binding' must be set.");
		}

		return this;

	}

	@SuppressWarnings("null")
	private boolean equalsAttributes(final ModelAttributeMap<Object> otherAttributeMap) {
		if (attributeMap.size() != otherAttributeMap.size()) {
			return false;
		}

		for (final Entry<ModelAttributeKey, Object> entry : attributeMap.entrySet()) {
			final ModelAttributeKey key = entry.getKey();
			final Object value = entry.getValue();
			final Object otherValue = otherAttributeMap.get(key);
			if (value == null) {
				if (otherValue != null || ! otherAttributeMap.containsKey(key)) {
					return false;
				}
			/* To support different "Collections" in the Map#value */
			} else if ( ! value.equals(otherValue) &&  ! equalsCollections(value, otherValue)) {
				return false;
			}
		}

		return true;
	}
}
