/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.entities.testing;

import java.util.Collection;
import java.util.Collections;
import java.util.Optional;
import java.util.UUID;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.shared.access.BinaryValue;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.SourceMetricsPojo;
import innowake.mining.shared.entities.ModulePojo.Representation;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.AdditionalInfo;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.ModuleType;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;

/**
 * Module for testing purposes.
 */
public class ModulePojoDummy extends ModulePojoPrototype {

	public static ModulePojo VOID = ModulePojoDummy.build(new ModulePojoPrototype());

	public ModulePojoDummy prepare(final BuildingConsumer<ModulePojoDummy> builder) {
		return builder.prepare(this);
	}
	
	public ModulePojo build() {
		return build(this);
	}
	
	public static ModulePojo build(final ModulePojoPrototype proto) {
		return new ModulePojo(
			proto.uid.orElseNonNull(UUID::randomUUID),											/* uid */
			proto.nid.orElseNonNull(-1L), 														/* nid */
			new CustomPropertiesMap(proto.customProperties.orElse(null)),						/* customProperties */
			proto.project.orElseNonNull(() -> EntityId.of(-1l)), null, null,					/* projectId */
			proto.name.orElseNonNull("DUMMY"),													/* name */
			proto.path.orElseNonNull("DUMMY.UNKNOWN"),											/* path */
			proto.technology.orElseNonNull(Technology.UNKNOWN),									/* technology */
			proto.type.orElseNonNull(Type.UNKNOWN),												/* type */
			proto.storage.orElseNonNull(Storage.UNDEFINED),										/* storage */
			proto.origin.orElseNonNull(Origin.CUSTOM),											/* origin */
			proto.creator.orElseNonNull(Creator.API),											/* creator */
			proto.identification.orElseNonNull(Identification.IDENTIFIED),						/* identification */
			proto.info.orElseNonNull(Collections.emptyMap()),									/* info */
			proto.description.orElse(null),														/* description */
			proto.source.optional().map(EntityId::getUid).orElse(null),						/* source */
			proto.contentHash.optional().orElse(null),										/* contentHash */
			proto.linkHash.orElseNonNull(""),													/* linkHash */
			proto.location.orElse(null),														/* location */
			proto.representation.orElse(null),													/* representation */
			proto.requiresReview.orElseNonNull(false),											/* requiresReview */
			proto.modifiedDate.orElse(null),													/* modifiedDate */
			proto.metricsDate.orElse(null),														/* metricsDate */
			proto.sourceMetrics.optional().map(SourceMetricsPojoDummy::build).orElse(null),	/* sourceMetrics */
			proto.content.orElse(null),															/* content */
			0,																					/* errors */
			0,																					/* statements */
			0,										 											/* sqlStatements */
			false,																				/* sourceCodeAvailable */
			null, null, null,																				/* parent */
			null,																				/* parentPath */
			proto.dependencyHash.orElse(null)																				/* dependency hash */
		);
	}

	public static ModulePojoPrototype build(final ModulePojo module) {
		final ModulePojoPrototype proto = new ModulePojoPrototype()
				.setCustomProperties(module.getCustomProperties())
				.setProject(module.getProject())
				.setName(module.getName())
				.setPath(module.getPath().orElse(null))
				.setTechnology(module.getTechnology())
				.setType(module.getType())
				.setStorage(module.getStorage())
				.setOrigin(module.getOrigin())
				.setCreator(module.getCreator())
				.setIdentification(module.getIdentification())
				.setInfo(module.getInfo().orElse(null))
				.setDescription(module.getDescription().orElse(null))
				.setSource(module.getSource().map(EntityId::of).orElse(null))
				.setLinkHash(module.getLinkHash())
				.setLocation(module.getLocation().orElse(null))
				.setRepresentation(module.getRepresentation().orElse(null))
				.setRequiresReview(module.isRequiresReview())
				.setModifiedDate(module.getModifiedDate().orElse(null))
				.setMetricsDate(module.getMetricsDate().orElse(null))
				.setContent(module.getContent().orElse(null));

		final Optional<BinaryValue> contentHash = module.getContentHash();
		if (contentHash.isPresent()) {
			proto.setContentHash(contentHash.get());
		}

		final Optional<SourceMetricsPojo> sourceMetrics = module.getSourceMetrics();
		if (sourceMetrics.isPresent()) {
			proto.setSourceMetrics(SourceMetricsPojoDummy.build(sourceMetrics.get()));
		}

		return proto;
	}

	public static ModulePojoPrototype newModuleDefinition(@Nullable final String name, @Nullable final ModuleType moduleType, @Nullable final ModuleLocation location,
			@Nullable final String path, @Nullable final Storage storage, @Nullable final Representation representation,
			final Collection<AdditionalInfo> additionalInfos, @Nullable final Identification identification, @Nullable final Origin origin) {

		if ( ! additionalInfos.isEmpty()) {
			throw new IllegalStateException("additional infos are not supported");
		}

		final ModulePojoPrototype module = new ModulePojoPrototype();
		if (name != null) {
			module.setName(name);
		}
		if (moduleType != null) {
			module.setTechnology(moduleType.getTechnology())
					.setType(moduleType.getType());
		}
		module.setLocation(location);
		if (path != null) {
			module.setPath(path);
		}
		if (storage != null) {
			module.setStorage(storage);
		}
		if (identification != null) {
			module.setIdentification(identification);
		}
		if (origin != null) {
			module.setOrigin(origin);
		}
		if (representation != null) {
			module.setRepresentation(representation);
		}

		return module;
	}
}
