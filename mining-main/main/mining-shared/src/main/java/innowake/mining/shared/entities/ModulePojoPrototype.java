/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.shared.entities;

import java.time.Instant;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonAlias;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.Definable;
import innowake.mining.shared.access.BinaryValue;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * {@code module} entity request class.
 */
public class ModulePojoPrototype extends MiningSequentialPojoPrototype<ModulePojoPrototype> {

	public final Definable<EntityId> project = new Definable<>(false, "Module.project");
	public final Definable<String> name = new Definable<>(false, "Module.name");
	public final Definable<String> path = new Definable<>(true, "Module.path");
	public final Definable<Technology> technology = new Definable<>(false, "Module.technology");
	public final Definable<Type> type = new Definable<>(false, "Module.type");
	public final Definable<Storage> storage = new Definable<>(false, "Module.storage");
	public final Definable<Origin> origin = new Definable<>(false, "Module.origin");
	public final Definable<Creator> creator = new Definable<>(false, "Module.creator");
	public final Definable<Identification> identification = new Definable<>(false, "Module.identification");
	public final Definable<Map<String, Object>> info = new Definable<>(true, "Module.info");
	public final Definable<String> description = new Definable<>(true, "Module.description");
	public final Definable<EntityId> source = new Definable<>(true, "Module.source");
	public final Definable<BinaryValue> contentHash = new Definable<>(true, "Module.contentHash");
	public final Definable<String> linkHash = new Definable<>(false, "Module.linkHash");
	public final Definable<ModuleLocation> location = new Definable<>(true, "Module.location");
	public final Definable<Representation> representation = new Definable<>(true, "Module.representation");
	public final Definable<Boolean> requiresReview = new Definable<>(false, "Module.requiresReview");
	public final Definable<Instant> modifiedDate = new Definable<>(true, "Module.modifiedDate");
	public final Definable<Instant> metricsDate = new Definable<>(true, "Module.metricsDate");
	public final Definable<SourceMetricsPojoPrototype> sourceMetrics = new Definable<>(true, "Module.sourceMetrics");
	public final Definable<String> content = new Definable<>(true, "Module.content");
	public final Definable<EntityId> parent = new Definable<>(true, "Module.parent");
	public final Definable<String> parentPath = new Definable<>(true, "Module.parentPath");
	public final Definable<String> dependencyHash = new Definable<>(true, "Module.dependencyHash");

	public ModulePojoPrototype() {
		super("Module");
	}

	@JsonAlias("projectId")
	public ModulePojoPrototype setProject(final EntityId project) {
		this.project.set(this.project.orElseNonNull(EntityId.VOID).merge(project));
		return this;
	}

	public ModulePojoPrototype setName(final String name) {
		this.name.set(name);
		return this;
	}

	public ModulePojoPrototype setPath(@Nullable final String path) {
		this.path.set(path);
		return this;
	}

	public ModulePojoPrototype setTechnology(final Technology technology) {
		this.technology.set(technology);
		return this;
	}

	public ModulePojoPrototype setType(final Type type) {
		this.type.set(type);
		return this;
	}

	public ModulePojoPrototype setStorage(final Storage storage) {
		this.storage.set(storage);
		return this;
	}

	public ModulePojoPrototype setOrigin(final Origin origin) {
		this.origin.set(origin);
		return this;
	}

	public ModulePojoPrototype setCreator(final Creator creator) {
		this.creator.set(creator);
		return this;
	}

	public ModulePojoPrototype setIdentification(final Identification identification) {
		this.identification.set(identification);
		return this;
	}

	public ModulePojoPrototype setInfo(final Map<String, Object> info) {
		this.info.set(info);
		return this;
	}

	public ModulePojoPrototype setDescription(final String description) {
		this.description.set(description);
		return this;
	}

	public ModulePojoPrototype setSource(final EntityId source) {
		this.source.set(source);
		return this;
	}

	public ModulePojoPrototype setContentHash(final BinaryValue contentHash) {
		this.contentHash.set(contentHash);
		return this;
	}

	public ModulePojoPrototype setLinkHash(final String linkHash) {
		this.linkHash.set(linkHash);
		return this;
	}

	public ModulePojoPrototype setLocation(@Nullable final ModuleLocation location) {
		this.location.set(location);
		return this;
	}

	public ModulePojoPrototype setRepresentation(final Representation representation) {
		this.representation.set(representation);
		return this;
	}

	public ModulePojoPrototype setRequiresReview(final boolean requiresReview) {
		this.requiresReview.set(Boolean.valueOf(requiresReview));
		return this;
	}

	public ModulePojoPrototype setModifiedDate(final Instant modifiedDate) {
		this.modifiedDate.set(modifiedDate);
		return this;
	}

	public ModulePojoPrototype setMetricsDate(final Instant metricsDate) {
		this.metricsDate.set(metricsDate);
		return this;
	}

	public ModulePojoPrototype setSourceMetrics(@Nullable final SourceMetricsPojoPrototype sourceMetrics) {
		this.sourceMetrics.set(sourceMetrics);
		return this;
	}

	public ModulePojoPrototype setContent(final String content) {
		this.content.set(content);
		return this;
	}

	public ModulePojoPrototype setParent(final EntityId parent) {
		this.parent.set(parent);
		return this;
	}
	
	public ModulePojoPrototype setParentPath(final String parentPath) {
		this.parentPath.set(parentPath);
		return this;
	}

	public ModulePojoPrototype setDependencyHash(final String dependencyHash) {
		this.dependencyHash.set(dependencyHash);
		return this;
	}
}
