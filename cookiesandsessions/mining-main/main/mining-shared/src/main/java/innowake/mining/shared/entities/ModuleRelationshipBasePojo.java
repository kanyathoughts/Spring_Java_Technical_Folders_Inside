/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition.ScalarType;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.RelationshipType;
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.Map;
import java.util.Optional;
import java.util.UUID;

/**
 * Basic description of a relation between two modules.
 */
public class ModuleRelationshipBasePojo {
	
	protected final UUID id;
	protected final UUID srcModule;
	protected final Optional<ModuleLocation> srcLocation;
	protected final UUID dstModule;
	protected final Optional<ModuleLocation> dstLocation;
	protected final RelationshipType type;
	protected final Optional<Map<String, Object>> properties;
	
	/**
	 * @param id the id of the module relationship
	 * @param srcModule the {@link UUID} of the source module
	 * @param srcLocation the {@link ModuleLocation} of the source module if available
	 * @param dstModule the {@link UUID} of the destination module
	 * @param dstLocation the {@link ModuleLocation} of the destination module if available
	 * @param type the name of the {@link RelationshipType} of the module relationship
	 * @param properties the properties of the module relationship if available
	 */
	@JsonCreator
	public ModuleRelationshipBasePojo(
			@JsonProperty("id") final UUID id,
			@JsonProperty("srcModule") final UUID srcModule,
			@JsonProperty("srcLocation") @Nullable final ModuleLocation srcLocation,
			@JsonProperty("dstModule") final UUID dstModule,
			@JsonProperty("dstLocation") @Nullable final ModuleLocation dstLocation,
			@JsonProperty("type") @JsonAlias("relationship") final RelationshipType type,
			@JsonProperty("properties") @Nullable final Map<String, Object> properties) {
		this.id = id;
		this.srcModule = srcModule;
		this.srcLocation = Optional.ofNullable(srcLocation);
		this.dstModule = dstModule;
		this.dstLocation = Optional.ofNullable(dstLocation);
		this.type = type;
		this.properties = Optional.ofNullable(properties);
	}
	
	/**
	 * @return the id of the module relationship.
	 */
	public UUID getId() {
		return id;
	}
	
	/**
	 * @return the {@link UUID} of the source module
	 */
	public UUID getSrcModule() {
		return srcModule;
	}
	
	/**
	 * @return the {@link ModuleLocation} of the source module if available
	 */
	public Optional<ModuleLocation> getSrcLocation() {
		return srcLocation;
	}
	
	/**
	 * @return the {@link UUID} of the destination module
	 */
	public UUID getDstModule() {
		return dstModule;
	}
	
	/**
	 * @return the {@link ModuleLocation} of the destination module if available
	 */
	public Optional<ModuleLocation> getDstLocation() {
		return dstLocation;
	}
	
	/**
	 * @return the {@link RelationshipType} of the module relationship
	 */
	public RelationshipType getRelationship() {
		return type;
	}
	
	/**
	 * @return the properties of the module relationship if available
	 */
	@MiningDataPoint(scalarType = ScalarType.JSON)
	public Optional<Map<String, Object>> getProperties() {
		return properties;
	}
	
	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("srcModule", srcModule)
				.append("srcLocation", srcLocation)
				.append("dstModule", dstModule)
				.append("dstLocation", dstLocation)
				.append("type", type)
				.append("properties", properties)
				.toString();
	}
	
}
