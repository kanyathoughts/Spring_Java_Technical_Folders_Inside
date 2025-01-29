/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.UUID;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataType;

/**
 * Client entity class.
 */
@MiningDataType(name = MiningEnitityNames.CLIENT)
public final class ClientPojo extends MiningPojo {
	
	private final String name;
	private final Boolean hasLogo;
	private final Boolean toBeDeleted;
	
	@JsonCreator
	public ClientPojo(
			@JsonProperty("uid") final UUID uid,
			@JsonProperty("customProperties") final CustomPropertiesMap customProperties,
			@JsonProperty("id") final Long nid,
			@JsonProperty("name") final String name,
			@JsonProperty("hasLogo") final Boolean hasLogo,
			@JsonProperty("markedDeleted") final Boolean toBeDeleted) {
		super(EntityId.of(uid, nid), customProperties);
		this.name = name;
		this.hasLogo = hasLogo;
		this.toBeDeleted = toBeDeleted;
	}
	
	/**
	 * Gets the name of the Client.
	 * @return Client name.
	 */
	public String getName() {
		return name;
	}
	
	/**
	 * Returns if the Client was marked for deletion.
	 * @return Whether the client is to be deleted.
	 */
	public Boolean isMarkedDeleted() {
		return toBeDeleted;
	}
	
	/**
	 * Returns if the Client has a custom logo defined.
	 * @return Whether the Client record contains a logo image.
	 */
	public Boolean getHasLogo() {
		return hasLogo;
	}
	
	@Override
	public String toString() {
		final ToStringBuilder builder = new ToStringBuilder(this);
		builder.appendSuper(super.toString());
		builder.append("name", name);
		builder.append("hasLogo", hasLogo);
		builder.append("toBeDeleted", toBeDeleted);
		return builder.toString();
	}
	
}
