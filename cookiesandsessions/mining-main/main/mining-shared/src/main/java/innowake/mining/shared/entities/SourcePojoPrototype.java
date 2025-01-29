/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities;

import com.fasterxml.jackson.annotation.JsonAlias;

import innowake.mining.shared.Definable;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.BinaryValue;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/*
	Source
	+	EntityId		project				project,projectId
	+	String			name
	+	String			path
	+	Technology		technology
	+	Type			type
	+	Long			metaDataRevision
	+	Long			contentRevision
	+	BinaryValue		contentHash
	*	BinaryString	content
 */

/**
 * Source entity request class.
 */
public class SourcePojoPrototype extends MiningPojoPrototype<SourcePojoPrototype> {
	
	public final Definable<EntityId> project = new Definable<>(false, "Source.project");
	public final Definable<String> name = new Definable<>(false, "Source.name");
	public final Definable<String> path = new Definable<>(false, "Source.path");
	public final Definable<Technology> technology = new Definable<>(false, "Source.technology");
	public final Definable<Type> type = new Definable<>(false, "Source.type");
	public final Definable<Long> metaDataRevision = new Definable<>(false, "Source.metaDataRevision");
	public final Definable<Long> contentRevision = new Definable<>(false, "Source.contentRevision");
	public final Definable<BinaryValue> contentHash = new Definable<>(false, "Source.contentHash");
	public final Definable<BinaryString> content = new Definable<>(false, "Source.content");
	
	public SourcePojoPrototype() {
		super("Source");
	}
	
	@JsonAlias("projectId")
	public SourcePojoPrototype setProject(final EntityId project) {
		this.project.set(this.project.orElseNonNull(EntityId.VOID).merge(project));
		return this;
	}
	
	public SourcePojoPrototype setName(final String name) {
		this.name.set(name);
		return this;
	}
	
	public SourcePojoPrototype setPath(final String path) {
		this.path.set(path);
		return this;
	}
	
	public SourcePojoPrototype setTechnology(final Technology technology) {
		this.technology.set(technology);
		return this;
	}
	
	public SourcePojoPrototype setType(final Type type) {
		this.type.set(type);
		return this;
	}
	
	public SourcePojoPrototype setMetaDataRevision(final Long metaDataRevision) {
		this.metaDataRevision.set(metaDataRevision);
		return this;
	}
	
	public SourcePojoPrototype setContentRevision(final Long contentRevision) {
		this.contentRevision.set(contentRevision);
		return this;
	}
	
	public SourcePojoPrototype setContentHash(final BinaryValue contentHash) {
		this.contentHash.set(contentHash);
		return this;
	}
	
	public SourcePojoPrototype setContent(final BinaryString content) {
		this.content.set(content);
		return this;
	}
	
}
