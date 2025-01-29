/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import java.util.UUID;
import java.util.function.Supplier;

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.BinaryValue;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataType;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Source entity class.
 */
@MiningDataType(name = MiningEnitityNames.SOURCE)
public class SourcePojo extends MiningPojo {
	
	private final EntityId project;
	private final String name;
	private final String path;
	private final Technology technology;
	private final Type type;
	private final Long metaDataRevision;
	private final Long contentRevision;
	private final BinaryValue contentHash;
	private final Supplier<BinaryString> content;
	
	@JsonCreator
	public SourcePojo(
			@JsonProperty("uid") final UUID uid,
			@JsonProperty("nid") @JsonAlias("id") final Long nid,
			@JsonProperty("projectEntity") @Nullable final EntityId project,
			@JsonProperty("project") @Nullable final UUID projectUid,
			@JsonProperty("projectId") @Nullable final Long projectNid,
			@JsonProperty("name") final String name,
			@JsonProperty("path") final String path,
			@JsonProperty("technology") final Technology technology,
			@JsonProperty("type") final Type type,
			@JsonProperty("metaDataRevision") final Long metaDataRevision,
			@JsonProperty("contentRevision") final Long contentRevision,
			@JsonProperty("contentHash") final BinaryValue contentHash,
			@JsonProperty("content") final BinaryString content,
			@JsonProperty("customProperties") final CustomPropertiesMap customProperties) {
		this(uid, nid, project != null ? project : EntityId.of(projectUid, projectNid),
				name, path, technology, type, metaDataRevision, contentRevision, contentHash, () -> content, customProperties);
	}
	
	public SourcePojo(
			final UUID uid,
			final Long nid,
			final EntityId project,
			final String name,
			final String path,
			final Technology technology,
			final Type type,
			final Long metaDataRevision,
			final Long contentRevision,
			final BinaryValue contentHash,
			final Supplier<BinaryString> content,
			final CustomPropertiesMap customProperties) {
		super(EntityId.of(uid, nid), customProperties);
		this.project = project;
		this.name = name;
		this.path = path;
		this.technology = technology;
		this.type = type;
		this.metaDataRevision = metaDataRevision;
		this.contentRevision = contentRevision;
		this.contentHash = contentHash;
		this.content = content;
	}
	
	@JsonIgnore
	public EntityId getProject() {
		return project;
	}
	
	@JsonProperty("project")
	public UUID getProjectUid() {
		return project.getUid();
	}
	
	@JsonProperty("projectId")
	public Long getProjectNid() {
		return project.getNid();
	}
	
	public String getName() {
		return name;
	}
	
	public String getPath() {
		return path;
	}
	
	public Technology getTechnology() {
		return technology;
	}
	
	public Type getType() {
		return type;
	}
	
	public Long getMetaDataRevision() {
		return metaDataRevision;
	}
	
	public Long getContentRevision() {
		return contentRevision;
	}
	
	public BinaryValue getContentHash() {
		return contentHash;
	}
	
	public BinaryString getContent() {
		return content.get();
	}
	
}
