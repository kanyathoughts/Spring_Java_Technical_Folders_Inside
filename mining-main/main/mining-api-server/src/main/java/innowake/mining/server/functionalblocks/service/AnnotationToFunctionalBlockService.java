/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.service;

import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationContext;
import innowake.mining.server.functionalblocks.generation.ModuleBlockGeneration;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.MiningPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import java.util.AbstractMap;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * Service for bridging between the Annotation and FunctionalBlock features.
 * <p>
 * Annotations are represented as FunctionalBlocks of {@linkplain FunctionalBlockFlag#TYPE type} {@link FunctionalBlockType#FUNCTIONAL_UNIT}.
 * The main purpose of this service is to find the functional block corresponding to the Annotation and to synchronize the Annotation
 * with the functional block.
 */
@Service
public class AnnotationToFunctionalBlockService {

	private final AnnotationService annotationService;
	private final FunctionalBlockService functionalBlockService;
	private final FunctionalBlockGenerationService functionalBlockGenerationService;

	public AnnotationToFunctionalBlockService(final AnnotationService annotationService, final FunctionalBlockService functionalBlockService,
			@Lazy final FunctionalBlockGenerationService functionalBlockGenerationService) {
		this.annotationService = annotationService;
		this.functionalBlockService = functionalBlockService;
		this.functionalBlockGenerationService = functionalBlockGenerationService;
	}

	/**
	 * Returns the UUIDs of the functional units (functional blocks of type "functional unit") that represent the given Annotations,
	 * invoking the block generation to ensure that the functional units were created and are up-to-date.
	 * @param projectId id of the project containing the Annotations
	 * @param annotationIds ids of the Annotations
	 * @return a map of Annotation id to uid of the functional block representing the Annotation
	 */
	public Map<Long, UUID> getFunctionalUnitsForAnnotations(final EntityId projectId, final Collection<EntityId> annotationIds) {
		if ( annotationIds.isEmpty() ) {
			return Collections.emptyMap();
		}

		final List<AnnotationPojo> annotations = annotationService.find(q -> q.ofProject(projectId).byIds(annotationIds));
		if ( annotations.size() < annotationIds.size() ) {
			/* using list instead of set cause have to use EntityId.equals to find missing annotation */
			final List<EntityId> ids = annotations.stream().map(MiningPojo :: identity).toList();
			for (var annotationId : annotationIds) {
				if ( ! ids.contains(annotationId) ) {
					throw new MiningEntityNotFoundException(AnnotationPojo.class, "annotation id: " + annotationId + ", project: " + projectId);
				}
			}
		}

		final Set<EntityId> moduleIds = annotations.stream().map(AnnotationPojo :: getModule).collect(Collectors.toSet());

		/* execute the functional block generation on the Annotations' modules to ensure that a functional block for each Annotation has been created */
		for (final EntityId moduleId : moduleIds) {
			functionalBlockGenerationService.generate(ModuleBlockGeneration.class, new FunctionalBlockGenerationContext(projectId), moduleId);
		}

		return functionalBlockService.findGeneratedFromAnnotations(annotationIds);
	}

	/**
	 * Returns the functional groups (functional blocks of type "functional group") that contain the given Annotations. These are the parents
	 * of the functional blocks returned by {@link #getFunctionalUnitsForAnnotations(EntityId, Collection)}.
	 * @param projectId id of the project containing the Annotations
	 * @param annotationIds ids of the Annotations
	 * @return a map of Annotation id to list of functional groups containing the Annotation
	 */
	public Map<Long, List<FunctionalBlockPojo>> getFunctionalGroupsForAnnotations(final EntityId projectId, final Collection<EntityId> annotationIds) {
		return functionalBlockService.findGeneratedFromAnnotations(annotationIds).entrySet().stream().collect(Collectors.toMap(Map.Entry :: getKey, entry -> {
			final FunctionalBlockPojo functionalUnit = functionalBlockService.find(entry.getValue())
					.orElseThrow(() -> new MiningEntityNotFoundException(FunctionalBlockPojo.class, entry.getValue().toString()));
			return functionalBlockService.find(q -> q.ofProject(projectId).byUids(functionalUnit.getParents()).withType(FunctionalBlockType.FUNCTIONAL_GROUP));
		}));
	}

	/**
	 * Returns the functional group names that contain the given Annotation as children.
	 * @param annotationId id of the Annotations
	 * @return a Set of functional group names
	 */
	public Set<String> getFunctionalBlockNamesByAnnotationId(final Long annotationId) {
		return functionalBlockService.findFunctionalBlockNamesByAnnotationId(annotationId);
	}

	/**
	 * Returns Map of UUID of Functional Unit to the Annotation for the given functional units.
	 * @param projectId id of the project containing the Annotations
	 * @param functionalUnitIds ids of the functional units
	 *
	 * @return a map of functional unit id to the Annotation
	 */
	public Map<UUID, AnnotationPojo> getAnnotationForFunctionalUnits(final EntityId projectId, final Collection<UUID> functionalUnitIds) {

		final Map<UUID, EntityId> result = functionalBlockService.getGeneratedFrom(functionalUnitIds).entrySet().stream()
				.filter(entry -> entry.getValue() != null && entry.getValue().getAnnotationId().isPresent())
				.collect(Collectors.toMap(Map.Entry :: getKey, entry -> entry.getValue().getAnnotationId().get()));

		final List<AnnotationPojo> annotations = annotationService.find(q -> q.ofProject(projectId).byIds(result.values()));

		return result.entrySet().stream().map(entry -> new AbstractMap.SimpleEntry<>(entry.getKey(),
						annotations.stream().filter(annotation -> annotation.getId().equals(entry.getValue().getNid())).findFirst()))
				.filter(entry -> entry.getValue().isPresent()).collect(Collectors.toMap(Map.Entry :: getKey, entry -> entry.getValue().get()));
	}
}
