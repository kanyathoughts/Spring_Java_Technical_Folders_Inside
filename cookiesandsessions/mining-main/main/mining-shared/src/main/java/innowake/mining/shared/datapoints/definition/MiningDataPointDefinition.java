/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.datapoints.definition;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import innowake.lib.core.api.lang.Nullable;

/**
 * Definition of a mining data point. Contains schema information such as the name, type and enclosing parent type of the data point,
 * as well as additional meta data.
 */
public class MiningDataPointDefinition extends MiningSchemaElement {
	
	public enum ScalarType {
		INT("Int"),
		LONG("Long"),
		FLOAT("Float"),
		STRING("String"),
		BOOLEAN("Boolean"),
		DATETIME("DateTime"),
		TIMESTAMP("Timestamp"),
		UUID("UUID"),
		JSON("JSON"),
		ENTITY_ID("EntityId"),
		UNDEFINED("");
		
		private final String name;
		
		private ScalarType(final String name) {
			this.name = name;
		}

		@Override
		public String toString() {
			return name;
		}
	}
	
	protected final String parentTypeName;
	@Nullable
	protected ScalarType scalarType;
	@Nullable
	protected String referenceTypeName;
	protected boolean isArray;
	protected boolean isNullable;
	
	@Nullable
	protected final AliasDefinition aliasDefinition;
	
	protected final List<MiningDataPointDefinition> parameters = new ArrayList<>();
	protected final Set<String> usages = new HashSet<>();
	protected final Map<String, Map<String, String>> usageAttributes = new HashMap<>();
	
	protected String displayName = "";
	protected String description = "";

	protected final Map<String, MiningDataPointFilterCallbacks> filterCallbacks = new HashMap<>();
	protected final Map<String, MiningDataPointSortCallback<?>> sortCallbacks = new HashMap<>();
	
	/**
	 * Creates a new MiningDataPointDefinition representing a data point with scalar type.
	 *
	 * @param name the name of the data point
	 * @param parentTypeName the name of the type on which the data point is defined
	 * @param scalarType the type of the data point
	 * @param isArray whether the data point represents an array
	 * @param isNullable whether the data point may contain a null value
	 */
	public MiningDataPointDefinition(final String name, final String parentTypeName,
									 final ScalarType scalarType, final boolean isArray, final boolean isNullable) {
		this(name, parentTypeName, scalarType, isArray, isNullable, Collections.emptyMap(), Collections.emptyMap());
	}

	/**
	 * Creates a new MiningDataPointDefinition representing a data point with scalar type.
	 *
	 * @param name the name of the data point
	 * @param parentTypeName the name of the type on which the data point is defined
	 * @param scalarType the type of the data point
	 * @param isArray whether the data point represents an array
	 * @param isNullable whether the data point may contain a null value
	 * @param filterCallbacks map of query names and filter callbacks collection for making the data point filterable on certain GraphQL queries
	 * @param sortCallbacks map of query names and sort callbacks for making the data point sortable on certain GraphQL queries
	 */
	public MiningDataPointDefinition(final String name, final String parentTypeName,
			final ScalarType scalarType, final boolean isArray, final boolean isNullable,
			final Map<String, MiningDataPointFilterCallbacks> filterCallbacks, final Map<String, MiningDataPointSortCallback<?>> sortCallbacks) {
		super(name);
		this.parentTypeName = parentTypeName;
		this.scalarType = scalarType;
		this.referenceTypeName = null;
		this.isArray = isArray;
		this.isNullable = isNullable;
		this.aliasDefinition = null;
		this.filterCallbacks.putAll(filterCallbacks);
		this.sortCallbacks.putAll(sortCallbacks);
	}
	
	/**
	 * Creates a new MiningDataPointDefinition representing a data point of a complex type.
	 *
	 * @param name the name of the data point
	 * @param parentTypeName the name of the type on which the data point is defined
	 * @param referenceTypeName the name of the type of this data point
	 * @param isArray whether the data point represents an array
	 * @param isNullable whether the data point may contain a null value
	 */
	public MiningDataPointDefinition(final String name, final String parentTypeName, final String referenceTypeName,
									 final boolean isArray, final boolean isNullable) {
		this(name, parentTypeName, referenceTypeName, isArray, isNullable, Collections.emptyMap(), Collections.emptyMap());
	}

	/**
	 * Creates a new MiningDataPointDefinition representing a data point of a complex type.
	 *
	 * @param name the name of the data point
	 * @param parentTypeName the name of the type on which the data point is defined
	 * @param referenceTypeName the name of the type of this data point
	 * @param isArray whether the data point represents an array
	 * @param isNullable whether the data point may contain a null value
	 * @param filterCallbacks map of query names and filter callbacks collection for making the data point filterable on certain GraphQL queries
	 * @param sortCallbacks map of query names and sort callbacks for making the data point sortable on certain GraphQL queries
	 */
	public MiningDataPointDefinition(final String name, final String parentTypeName, final String referenceTypeName,
			final boolean isArray, final boolean isNullable,
			final Map<String, MiningDataPointFilterCallbacks> filterCallbacks, final Map<String, MiningDataPointSortCallback<?>> sortCallbacks) {
		super(name);
		this.parentTypeName = parentTypeName;
		this.scalarType = null;
		this.referenceTypeName = referenceTypeName;
		this.isArray = isArray;
		this.isNullable = isNullable;
		this.aliasDefinition = null;
		this.filterCallbacks.putAll(filterCallbacks);
		this.sortCallbacks.putAll(sortCallbacks);
	}
	
	/**
	 * Creates a new MiningDataPointDefinition representing an alias for another data point.
	 * 
	 * @param name the name of the alias
	 * @param parentTypeName the name of the type on which the alias is defined
	 * @param aliasDefinition information about the actual data point represented by this alias
	 */
	public MiningDataPointDefinition(final String name, final String parentTypeName, final AliasDefinition aliasDefinition) {
		this(name, parentTypeName, aliasDefinition, Collections.emptyMap(), Collections.emptyMap());
	}

	/**
	 * Creates a new MiningDataPointDefinition representing an alias for another data point.
	 *
	 * @param name the name of the alias
	 * @param parentTypeName the name of the type on which the alias is defined
	 * @param aliasDefinition information about the actual data point represented by this alias
	 * @param filterCallbacks map of query names and filter callbacks collection for making the data point filterable on certain GraphQL queries
	 * @param sortCallbacks map of query names and sort callbacks for making the data point sortable on certain GraphQL queries
	 */
	public MiningDataPointDefinition(final String name, final String parentTypeName, final AliasDefinition aliasDefinition,
			final Map<String, MiningDataPointFilterCallbacks> filterCallbacks, final Map<String, MiningDataPointSortCallback<?>> sortCallbacks) {
		super(name);
		this.parentTypeName = parentTypeName;
		this.scalarType = null;
		this.referenceTypeName = null;
		this.isArray = false;
		this.isNullable = false;
		this.aliasDefinition = aliasDefinition;
		this.filterCallbacks.putAll(filterCallbacks);
		this.sortCallbacks.putAll(sortCallbacks);
	}

	/**
	 * Copy constructor for subclass (decorator pattern).
	 * @param other the other definition to copy
	 */
	protected MiningDataPointDefinition(final MiningDataPointDefinition other) {
		super(other.name);
		this.projectIds = other.projectIds;
		this.parentTypeName = other.parentTypeName;
		this.scalarType = other.scalarType;
		this.referenceTypeName = other.referenceTypeName;
		this.isArray = other.isArray;
		this.isNullable = other.isNullable;
		this.parameters.addAll(other.parameters);
		this.usages.addAll(other.usages);
		this.usageAttributes.putAll(other.usageAttributes);
		this.providedBy.addAll(other.providedBy);
		this.displayName = other.displayName;
		this.description = other.description;
		this.aliasDefinition = other.aliasDefinition;
		this.filterCallbacks.putAll(other.filterCallbacks);
		this.sortCallbacks.putAll(other.sortCallbacks);
	}

	/**
	 * Constructor for JSON de-serialization. Do not use this constructor when creating a MiningDataPointDefinition programmatically,
	 * prefer one of the other constructors instead.
	 * @param name the name of the data point
	 * @param parentTypeName the name of the type on which the data point is defined
	 * @param scalarType the type of the data point (if data point is of scalar type)
	 * @param referenceTypeName the name of the type of this data point (if the data point is of complex type)
	 * @param isArray whether the data point represents an array
	 * @param isNullable whether the data point may contain a null value
	 * @param aliasDefinition the alias definition (if this data point is an alias)
	 */
	@JsonCreator
	public MiningDataPointDefinition(@JsonProperty("name") final String name,
									 @JsonProperty("parentTypeName") final String parentTypeName,
									 @JsonProperty("scalarType") final ScalarType scalarType,
									 @JsonProperty("referenceTypeName") final String referenceTypeName,
									 @JsonProperty("isArray")final boolean isArray,
									 @JsonProperty("isNullable") final boolean isNullable,
									 @JsonProperty("aliasDefinition") final AliasDefinition aliasDefinition) {
		super(name);
		this.parentTypeName = parentTypeName;
		this.scalarType = scalarType;
		this.referenceTypeName = referenceTypeName;
		this.isArray = isArray;
		this.isNullable = isNullable;
		this.aliasDefinition = aliasDefinition;
	}
	
	public String getId() {
		return this.parentTypeName + "." + this.name;
	}

	/**
	 * Gets the name of the enclosing parent type (a {@link MiningDataTypeDefinition}).
	 *
	 * @return the name of the parent type
	 */
	public String getParentTypeName() {
		return parentTypeName;
	}

	/**
	 * Gets the scalar type of this data point. Returns {@code null} if this data point is not of scalar type.
	 *
	 * @return the scalar type or {@code null}
	 */
	@Nullable
	public ScalarType getScalarType() {
		return scalarType;
	}
	
	/**
	 * Sets the scalar type of this data point. Set it to {@code null} if this data point is not of scalar type.
	 *
	 * @param scalarType the scalar type or {@code null}
	 */
	public void setScalarType(@Nullable final ScalarType scalarType) {
		this.scalarType = scalarType;
	}
	
	/**
	 * Gets the name of the type of this data point. Returns {@code null} if this data point is of scalar type.
	 *
	 * @return the type name or {@code null}
	 */
	@Nullable
	public String getReferenceTypeName() {
		return referenceTypeName;
	}
	
	/**
	 * Sets the name of the type of this data point. Set to {@code null} if this data point is of scalar type.
	 *
	 * @param referenceTypeName the type name or {@code null}
	 */
	public void setReferenceTypeName(@Nullable final String referenceTypeName) {
		this.referenceTypeName = referenceTypeName;
	}
	
	/**
	 * Returns whether this data point represents and array of its type.
	 *
	 * @return whether data point is array
	 */
	public boolean isArray() {
		return isArray;
	}
	
	/**
	 * Sets whether this data point represents and array of its type.
	 *
	 * @param isArray whether data point is array
	 */
	public void setArray(final boolean isArray) {
		this.isArray = isArray;
	}

	/**
	 * Returns whether this data point may contain {@code null}.
	 * 
	 * @return whether the data point is nullable
	 */
	public boolean isNullable() {
		return isNullable;
	}
	
	/**
	 * Sets whether this data point may contain {@code null}.
	 * @param isNullable whether the data point is nullable
	 */
	public void setNullable(final boolean isNullable) {
		this.isNullable = isNullable;
	}
	
	/**
	 * Returns a list of the parameters of this data point. Each parameter is returned as a {@link MiningDataPointDefinition}.
	 * <p>
	 * The list of parameters can be, and usually is, empty.
	 * 
	 * @return the list of parameters
	 */
	public List<MiningDataPointDefinition> getParameters() {
		return parameters;
	}
	
	/**
	 * Adds a parameter for this data point. A parameter is used when querying the value of the data point. The parameter is itself expressed
	 * as another {@code MiningDataPointDefinition}.
	 *
	 * @param param the parameter to add
	 */
	public void addParameter(final MiningDataPointDefinition param) {
		parameters.add(param);
	}
	
	/**
	 * Convenience method to add multiple parameters.
	 *
	 * @param params the parameters to add
	 * @see #addParameter(MiningDataPointDefinition)
	 */
	public void addParameters(final Collection<MiningDataPointDefinition> params) {
		parameters.addAll(params);
	}
	
	/**
	 * Gets the usages of this data point.
	 * 
	 * @return the data point usages
	 */
	public Set<String> getUsages() {
		return usages;
	}
	
	/**
	 * Adds a data point usage.
	 *
	 * @param usage the data point usage
	 */
	public void addUsage(final String usage) {
		usages.add(usage);
	}
	
	/**
	 * Convenience method to add multiple usages.
	 *
	 * @param newUsages the data point usages
	 * @see #addUsage(String)
	 */
	public void addUsages(final Collection<String> newUsages) {
		usages.addAll(newUsages);
	}
	
	
	/**
	 * Gets the usage attributes
	 *
	 * @return the map of usage attributes
	 */
	public Map<String, Map<String, String>> getUsageAttributes() {
		return usageAttributes;
	}
	
	/**
	 * Adds a usage attribute
	 *
	 * @param usage the usage
	 * @param key the key of the attribute
	 * @param value the value of the attribute
	 */
	public void addUsageAttribute(final String usage, final String key, final String value) {
		usageAttributes.computeIfAbsent(usage, k -> new HashMap<>()).put(key, value);
	}
	
	/**
	 * Convenience method to add multiple usage attributes.
	 *
	 * @param usage the usage
	 * @param attributes the attributes
	 * @see #addUsageAttribute(String, String, String)
	 */
	public void addUsageAttributes(final String usage, final Map<String, String> attributes) {
		usageAttributes.computeIfAbsent(usage, k -> new HashMap<>()).putAll(attributes);
	}
	
	/**
	 * Gets the name of the data point to be displayed on the UI.
	 *
	 * @return display name of the data point
	 */
	public String getDisplayName() {
		return displayName;
	}

	
	/**
	 * Sets the name of the data point to be displayed on the UI.
	 *
	 * @param displayName display name of the data point
	 */
	public void setDisplayName(final String displayName) {
		this.displayName = displayName;
	}

	
	/**
	 * Gets the description of the data point to be displayed on the UI.
	 *
	 * @return description of the data point
	 */
	public String getDescription() {
		return description;
	}

	
	/**
	 * Sets the description of the data point to be displayed on the UI.
	 *
	 * @param description description of the data point
	 */
	public void setDescription(final String description) {
		this.description = description;
	}
	
	/**
	 * Returns whether this data point is an alias for another data point.
	 *
	 * @return {@code true} if this data point is an alias
	 * @see #getAliasFor()
	 */
	public boolean isAlias() {
		return aliasDefinition != null;
	}
	
	/**
	 * Returns information about the data point for which this data point is an alias,
	 * or {@code null} if this data point is not an alias.
	 *
	 * @return the alias definition or {@code null}
	 */
	@Nullable
	public AliasDefinition getAliasFor() {
		return aliasDefinition;
	}

	@JsonIgnore
	public MiningDataPointFilterCallbacks getFilterCallbacks(final String queryName) {
		return Optional.ofNullable(filterCallbacks.get(queryName)).orElse(MiningDataPointFilterCallbacks.EMPTY);
	}

	@JsonIgnore
	@SuppressWarnings("unchecked")
	public <B> Optional<MiningDataPointSortCallback<B>> getSortCallback(final String queryName) {
		return Optional.ofNullable((MiningDataPointSortCallback<B>) sortCallbacks.get(queryName));
	}

	@Override
	public String toString() {
		final String type = scalarType != null ? scalarType.toString() : referenceTypeName;
		return "MiningDataPointDefinition [name=" + name + ", parentTypeName=" + parentTypeName + ", aliasDefinition=" + aliasDefinition + ", type=" + type
				+ ", isArray=" + isArray + ", displayName=" + displayName + ", description=" + description + "]";
	}
}
