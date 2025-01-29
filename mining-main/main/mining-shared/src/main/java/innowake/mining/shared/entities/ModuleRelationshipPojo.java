/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.model.Binding;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.Collections;


/**
 * {@code module_relationship} entity class for relation ships between {@code module} entities.
 */
@MiningDataType(name = MiningEnitityNames.MODULE_RELATIONSHIP)
public class ModuleRelationshipPojo extends ModuleRelationshipBasePojo {

	private final Optional<RelationshipDirection> direction;
	private final Optional<Binding> dependencyBinding;
	private final Optional<String> dependencyAttributes;
	private final List<UUID> validIfReachedFrom;
	private final Optional<ModuleBasePojo> srcModuleDetails;
	private final Optional<ModuleBasePojo> dstModuleDetails;
	private final Optional<UUID> dependencyDefinition;

	/**
	 * Constructor.
	 * 
	 * @param id the id of the module relationship
	 * @param srcModule the {@link UUID} of the source module
	 * @param srcLocation the {@link ModuleLocation} of the source module if available
	 * @param dstModule the {@link UUID} of the destination module
	 * @param dstLocation the {@link ModuleLocation} of the destination module if available
	 * @param type the name of the {@link RelationshipType} of the module relationship
	 * @param direction context dependent direction of the relationship
	 * @param properties the properties of the module relationship if available
	 * @param dependencyBinding the dependency binding of the module relationship if available
	 * @param dependencyAttributes the dependency attributes of the module relationship if available
	 * @param validIfReachedFrom List of Modules limiting the applicability of the relation.
	 * @param srcModuleDetails details of the source module
	 * @param dstModuleDetails details of the destination module
	 * @param dependencyDefinition  the {@link UUID} of the dependencyDefinition
	 */
	@JsonCreator
	public ModuleRelationshipPojo(
			@JsonProperty("id") final UUID id,
			@JsonProperty("srcModule") final UUID srcModule,
			@JsonProperty("srcLocation") @Nullable final ModuleLocation srcLocation,
			@JsonProperty("dstModule") final UUID dstModule,
			@JsonProperty("dstLocation") @Nullable final ModuleLocation dstLocation,
			@JsonProperty("type") @JsonAlias("relationship") final RelationshipType type,
			@JsonProperty("direction") @Nullable final RelationshipDirection direction,
			@JsonProperty("properties") @Nullable final Map<String, Object> properties,
			@JsonProperty("dependencyBinding") @Nullable final Binding dependencyBinding,
			@JsonProperty("dependencyAttributes") @Nullable final String dependencyAttributes,
			@JsonProperty("validIfReachedFrom") final List<UUID> validIfReachedFrom,
			@JsonProperty("srcModuleDetails") @Nullable final ModuleBasePojo srcModuleDetails,
			@JsonProperty("dstModuleDetails") @Nullable final ModuleBasePojo dstModuleDetails,
			@JsonProperty("dependencyDefinition") @Nullable final UUID dependencyDefinition) {
		super(id, srcModule, srcLocation, dstModule, dstLocation, type, properties);
		this.direction = Optional.ofNullable(direction);
		this.dependencyBinding = Optional.ofNullable(dependencyBinding);
		this.dependencyAttributes = Optional.ofNullable(dependencyAttributes);
		this.validIfReachedFrom = validIfReachedFrom;
		this.srcModuleDetails = Optional.ofNullable(srcModuleDetails);
		this.dstModuleDetails = Optional.ofNullable(dstModuleDetails);
		this.dependencyDefinition = Optional.ofNullable(dependencyDefinition);
	}

	/**
	 * Constructor.
	 * 
	 * @param srcLocation the {@link ModuleLocation} of the source module if available
	 * @param type the name of the {@link RelationshipType} of the module relationship
	 * @param dependencyBinding the dependency binding of the module relationship if available
	 * @param dstModuleDetails details of the destination module
	 */
	public ModuleRelationshipPojo(
			@Nullable final ModuleLocation srcLocation,
			final RelationshipType type,
			@Nullable final Binding dependencyBinding,
			@Nullable final ModuleBasePojo dstModuleDetails) {
		super(UUID.randomUUID(), UUID.randomUUID(), srcLocation, UUID.randomUUID(), null, type, null);
		this.direction = Optional.empty();
		this.dependencyBinding = Optional.ofNullable(dependencyBinding);
		this.dependencyAttributes = Optional.empty();
		this.validIfReachedFrom = Collections.emptyList();
		this.srcModuleDetails = Optional.empty();
		this.dstModuleDetails = Optional.ofNullable(dstModuleDetails);
		this.dependencyDefinition = Optional.empty();
	}

	/**
	 * @return Direction of the relationship, either incoming or outgoing.
	 *         This value and whether it is defined depends on the base Module specified when querying relationships.
	 */
	public Optional<RelationshipDirection> getDirection() {
		return direction;
	}

	/**
	 * @return the dependency binding of the module relationship if available
	 */
	public Optional<Binding> getDependencyBinding() {
		return dependencyBinding;
	}

	/**
	 * @return the dependency attributes of the module relationship if available
	 */
	public Optional<String> getDependencyAttributes() {
		return dependencyAttributes;
	}
	
	/**
	 * @return List of Modules limiting the applicability of the relation. If this is empty the relationship is generally valid.
	 */
	public List<UUID> getValidIfReachedFrom() {
		return validIfReachedFrom;
	}

	/**
	 * @return details of the source module
	 */
	public Optional<ModuleBasePojo> getSrcModuleDetails() {
		return srcModuleDetails;
	}

	/**
	 * @return details of the destination module
	 */
	public Optional<ModuleBasePojo> getDstModuleDetails() {
		return dstModuleDetails;
	}

	/**
	 * Optionally, returns the id of the {@link DependencyDefinitionPojo} from which this relationship was created. Set only if the relationship
	 * was created by Discovery.
	 * @return the id of the {@link DependencyDefinitionPojo}
	 */
	public Optional<UUID> getDependencyDefinition() {
		return dependencyDefinition;
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this).appendSuper(super.toString())
				.append("dependencyBinding", dependencyBinding)
				.append("dependencyAttributes", dependencyAttributes)
				.append("onlyIfReachedFrom", validIfReachedFrom)
				.toString();
	}

	@Override
	public boolean equals(@Nullable final Object o) {
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass()) {
			return false;
		}

		final ModuleRelationshipPojo that = (ModuleRelationshipPojo) o;
		return Objects.equals(id, that.id) && Objects.equals(srcModule, that.srcModule) && Objects.equals(srcLocation, that.srcLocation) && Objects.equals(dstModule, that.dstModule) && Objects.equals(dstLocation, that.dstLocation) && type == that.type && Objects.equals(properties, that.properties) && Objects.equals(dependencyBinding, that.dependencyBinding) && Objects.equals(dependencyAttributes, that.dependencyAttributes) && Objects.equals(validIfReachedFrom, that.validIfReachedFrom);
	}

	@Override
	public int hashCode() {
		return Objects.hash(id, srcModule, srcLocation, dstModule, dstLocation, type, properties, dependencyBinding, dependencyAttributes, validIfReachedFrom);
	}
}
