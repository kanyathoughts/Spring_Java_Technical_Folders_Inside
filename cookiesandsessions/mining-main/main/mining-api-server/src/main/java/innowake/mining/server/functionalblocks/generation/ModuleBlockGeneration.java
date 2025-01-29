/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.functionalblocks.generation;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import innowake.mining.shared.access.AstService;
import innowake.mining.shared.entities.ast.AstNodePojo;
import innowake.mining.shared.model.AstNodeLocation;
import org.springframework.stereotype.Component;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.core.api.AstNodeUtils;
import innowake.mining.server.functionalblocks.generation.FunctionalBlockGenerationResult.Operation;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.BinaryValue;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import innowake.mining.shared.entities.functionalblocks.ModulePart;
import innowake.mining.shared.model.ModuleLocation;

/**
 * Generates a functional block that represents a Module.
 */
@Component
public class ModuleBlockGeneration implements FunctionalBlockGeneration<EntityId, GeneratedFrom> {

	public static final String MODULE_BLOCK_GENERATION_ID = "ModuleBlockGeneration";
	public static final String PARAGRAPH_BLOCK_GENERATION_ID = "ParagraphBlockGeneration";
	public static final String ANNOTATION_BLOCK_GENERATION_ID = "AnnotationBlockGeneration";
	public static final String NO_DESCRIPTION_AVAILABLE = "No Automatic Description Available";

	private final FunctionalBlockService functionalBlockService;
	private final ModuleService moduleService;
	private final AnnotationService annotationService;
	private final AstService astService;
	
	public ModuleBlockGeneration(final FunctionalBlockService functionalBlockService, final ModuleService moduleService,
			final AnnotationService annotationService, final AstService astService) {
		this.functionalBlockService = functionalBlockService;
		this.annotationService = annotationService;
		this.moduleService = moduleService;
		this.astService = astService;
	}

	@Override
	public Collection<FunctionalBlockGenerationResult<GeneratedFrom>> generate(final FunctionalBlockGenerationContext context,
			@Nullable final EntityId moduleId) {
		EntityId projectId = context.getProjectId();
		if (moduleId == null) {
			throw new IllegalArgumentException("Cannot execute module block generation because the 'moduleId' is null");
		}
		final var module = moduleService.findAnyModule(q -> q.byId(moduleId))
				.orElseThrow(() -> new IllegalArgumentException("Cannot execute module block generation because the 'module' is null"));
		
		final Optional<FunctionalBlockPojo> existingBlock = findExistingModuleBlock(module, projectId);
		FunctionalBlockPojoPrototype moduleBlock = new FunctionalBlockPojoPrototype();
		GeneratedFrom moduleGeneratedFrom = null;
		if (existingBlock.isEmpty()) {
			/* Module block generation */
			final Optional<BinaryValue> contentHash = module.getContentHash();
			final Optional<String> dependencyHash = module.getDependencyHash();
			moduleGeneratedFrom = GeneratedFrom.fromModule(module.getLinkHash(),
					contentHash.map(BinaryValue::toString).orElse(null), dependencyHash.orElse(null));
			moduleBlock = generateModuleBlock(module, projectId);
		}
		final List<FunctionalBlockGenerationResult<GeneratedFrom>> moduleBlockResults = new ArrayList<>();
		/* Paragraph block generation */
		final List<UUID> moduleBlockChildrenUuidList = generateParagraphBlock(moduleId, module, projectId, moduleBlockResults);
		moduleBlock = setFieldsToModuleBlock(module, existingBlock, moduleBlock, moduleBlockResults, moduleBlockChildrenUuidList);
		if (existingBlock.isEmpty()) {
			final FunctionalBlockGenerationResult<GeneratedFrom> finalResult = new FunctionalBlockGenerationResult<>(Operation.CREATE,
					moduleBlock, moduleGeneratedFrom);
			moduleBlockResults.add(finalResult);
		}

		return moduleBlockResults;
	}

	private List<UUID> generateParagraphBlock(final EntityId moduleId, final ModulePojo module,
			final EntityId projectId, final List<FunctionalBlockGenerationResult<GeneratedFrom>> moduleBlockResults) {
		final List<UUID> moduleBlockChildrenUuidList = new ArrayList<>();
		final Map<FunctionalBlockPojo, List<ModulePart>> existingParagraghAndParts = getExistingParagraphAndModuleParts(module, projectId);

		final List<AnnotationPojo> annotations = annotationService.find(q -> q.ofProject(projectId).ofModule(moduleId));
		final Map<UUID, Boolean> annotationBlockMapped = new HashMap<>();
		final Map<Long, UUID> annotationMap = generateAnnotationBlock(module, annotations, projectId, moduleBlockResults, annotationBlockMapped);
		final List<FunctionalBlockGenerationResult<GeneratedFrom>> updatedResult = moduleBlockResults;
		final List<AstNodePojo> paragraphs = findParagraphs(moduleId);
		for (final AstNodePojo paragraph : paragraphs) {
			final AstNodeLocation advancedModuleLocationV2 = paragraph.getLocation();
			if (advancedModuleLocationV2 != null) {
				final Optional<Integer> offset = advancedModuleLocationV2.getRetracedOffset();
				final Optional<Integer> length = advancedModuleLocationV2.getRetracedLength();
				if (offset.isPresent() && length.isPresent()) {
					final Integer modulePartLength = calculateProperLength(paragraph, offset.get() + length.get()) - offset.get();
					final ModulePart modulePart = new ModulePart(module.getLinkHash(), new ModuleLocation(offset.get(), modulePartLength));
					final List<FunctionalBlockPojo> matchingFunctionalBlock = getMatchingFunctionalBlock(existingParagraghAndParts, modulePart);
					
					FunctionalBlockPojoPrototype paragraphBlock = new FunctionalBlockPojoPrototype();
					if (matchingFunctionalBlock.isEmpty()) {
						paragraphBlock = createParagraphBlock(projectId, paragraph, modulePart);
						paragraphBlock.setChildren(searchForMatchingParagraphAnnotationBlock(modulePart, annotations, annotationMap, annotationBlockMapped));

						final Optional<BinaryValue> contentHash = module.getContentHash();
						final Optional<String> dependencyHash = module.getDependencyHash();
						final GeneratedFrom paragraphGeneratedFrom = GeneratedFrom.fromModule(module.getLinkHash(),
								contentHash.map(BinaryValue::toString).orElse(null), dependencyHash.orElse(null));
						final UUID paragraphBlockId = paragraphBlock.uid.required(false).orElseNonNull(UUID::randomUUID);
						paragraphBlock.setUid(paragraphBlockId);
						final FunctionalBlockGenerationResult<GeneratedFrom> result = new FunctionalBlockGenerationResult<>(Operation.CREATE, paragraphBlock,
								 paragraphGeneratedFrom);
						moduleBlockChildrenUuidList.add(paragraphBlockId);
						moduleBlockResults.add(result);
					} else {
						final List<FunctionalBlockGenerationResult<GeneratedFrom>> annotationBlocks = getUpdatedAnnotationBlock(updatedResult);
						if ( ! annotationBlocks.isEmpty()) {
							final List<FunctionalBlockPojoPrototype> annotationFunctionalBlocks = new ArrayList<>();
							final List<UUID> uuids = getAnnotationFunctionalBlocks(annotations, annotationBlockMapped, annotationMap, modulePart,
									matchingFunctionalBlock, annotationBlocks, annotationFunctionalBlocks);
							updateParagraphBlockChildren(moduleBlockResults, moduleBlockChildrenUuidList, matchingFunctionalBlock, paragraphBlock,
									annotationFunctionalBlocks, uuids);
						} else {
							moduleBlockChildrenUuidList.add(matchingFunctionalBlock.get(0).getUid());
						}
					}
				}
			}
		}
		final List<UUID> annotationBlockMappedToModule = annotationBlockMapped.entrySet().stream().filter(entry -> !entry.getValue().booleanValue())
				.map(Map.Entry::getKey)
				.collect(Collectors.toList());
		moduleBlockChildrenUuidList.addAll(annotationBlockMappedToModule);
		return moduleBlockChildrenUuidList;
	}
	
	
	private FunctionalBlockPojoPrototype setFieldsToModuleBlock(final ModulePojo module, final Optional<FunctionalBlockPojo> existingBlock,
			FunctionalBlockPojoPrototype moduleBlock, final List<FunctionalBlockGenerationResult<GeneratedFrom>> moduleBlockResults,
			final List<UUID> moduleBlockChildrenUuidList) {
		if ( ! moduleBlockChildrenUuidList.isEmpty()) {
			if (existingBlock.isPresent()) {
				moduleBlock = new FunctionalBlockPojoPrototype();
				moduleBlock.setDescription(module.getDescription().orElse(NO_DESCRIPTION_AVAILABLE));
				moduleBlock.setUid(existingBlock.get().getUid());
				moduleBlock.setChildren(moduleBlockChildrenUuidList);
				final FunctionalBlockGenerationResult<GeneratedFrom> result = new FunctionalBlockGenerationResult<>(Operation.UPDATE, moduleBlock);
				moduleBlockResults.add(result);
			} else {
				/* Add paragraph block / annotation block as a child to module block */
				moduleBlock.setChildren(moduleBlockChildrenUuidList);
			}
		}
		return moduleBlock;
	}

	private Optional<FunctionalBlockPojo> findExistingModuleBlock(final ModulePojo module, final EntityId projectId) {
		return functionalBlockService
				.find(q -> q.ofProject(projectId).generatedFromModule(module.getLinkHash()).withType(FunctionalBlockType.MODULE).withType(FunctionalBlockType.STRUCTURAL)
						.ofProject(module.getProject())).stream()
				.findAny();
	}
	
	private static FunctionalBlockPojoPrototype generateModuleBlock(final ModulePojo module, final EntityId projectId) {
		final FunctionalBlockPojoPrototype moduleBlock = new FunctionalBlockPojoPrototype();
		moduleBlock.setModuleParts(Arrays.asList(new ModulePart(module.getLinkHash())));
		moduleBlock.setName(module.getName());
		moduleBlock.setDescription(module.getDescription().orElse(NO_DESCRIPTION_AVAILABLE));
		moduleBlock.setFlags(generateBlockMap(MODULE_BLOCK_GENERATION_ID, Set.of(FunctionalBlockType.MODULE, FunctionalBlockType.STRUCTURAL), Boolean.TRUE));
		moduleBlock.setProject(projectId);
		moduleBlock.setUid(UUID.randomUUID());
		return moduleBlock;
	}
	
	private List<AstNodePojo> findParagraphs(final EntityId moduleId) {
		return astService.find(q -> q.ofModule(moduleId)
					.withSuperTypes(AstNodeUtils.CFG_COLLAPSIBLE_NODE, AstNodeUtils.INVOCABLE));
	}

	private List<UUID> getAnnotationFunctionalBlocks(final List<AnnotationPojo> annotations, final Map<UUID, Boolean> annotationBlockMapped,
			final Map<Long, UUID> annotationMap, final ModulePart modulePart, final List<FunctionalBlockPojo> matchingFunctionalBlock,
			final List<FunctionalBlockGenerationResult<GeneratedFrom>> annotationBlocks, final List<FunctionalBlockPojoPrototype> annotationFunctionalBlocks) {
		for (final FunctionalBlockGenerationResult<GeneratedFrom> generationForm : annotationBlocks) {
			if (matchingFunctionalBlock.get(0).getChildren().contains(generationForm.getFunctionalBlock().uid.getNonNull())) {
				annotationFunctionalBlocks.add(generationForm.getFunctionalBlock());
			}
		}
		return searchForMatchingParagraphAnnotationBlock(modulePart, annotations, annotationMap, annotationBlockMapped);
	}

	private List<FunctionalBlockPojo> getMatchingFunctionalBlock(final Map<FunctionalBlockPojo, List<ModulePart>> existingParagraghAndParts,
			final ModulePart modulePart) {
		return existingParagraghAndParts.entrySet().parallelStream().filter(entry -> entry.getValue().stream().anyMatch(
				part -> part.getLocation().equals(modulePart.getLocation()) && part.getModuleLinkHash().equals(modulePart.getModuleLinkHash())))
				.map(Map.Entry::getKey).collect(Collectors.toList());
	}

	private Map<FunctionalBlockPojo, List<ModulePart>> getExistingParagraphAndModuleParts(final ModulePojo module, final EntityId projectId) {
		final List<FunctionalBlockPojo> existingParagraphBlocks = functionalBlockService.find(q -> q.ofProject(projectId)
				.generatedFromModule(module.getLinkHash()).withType(FunctionalBlockType.STRUCTURAL));
		
		final Map<FunctionalBlockPojo, List<ModulePart>> existingParagraghAndParts = new HashMap<>();
		for (final FunctionalBlockPojo existingBlock : existingParagraphBlocks) {
			existingParagraghAndParts.put(existingBlock, existingBlock.getModuleParts());
		}
		return existingParagraghAndParts;
	}

	private List<FunctionalBlockGenerationResult<GeneratedFrom>> getUpdatedAnnotationBlock(
			final List<FunctionalBlockGenerationResult<GeneratedFrom>> updatedResult) {
		return updatedResult.stream()
				.filter(entry -> entry.getFunctionalBlock().flags.isDefined() && entry.getFunctionalBlock().flags.get() != null
						&& entry.getFunctionalBlock().flags.getNonNull().get(FunctionalBlockFlag.TYPE.name())
						.equals(Set.of(FunctionalBlockType.FUNCTIONAL_UNIT)))
						.collect(Collectors.toList());
	}

	private void updateParagraphBlockChildren(final List<FunctionalBlockGenerationResult<GeneratedFrom>> moduleBlockResults,
			final List<UUID> moduleBlockChildrenUuidList, final List<FunctionalBlockPojo> matchingKeys, final FunctionalBlockPojoPrototype paragraphBlock,
			final List<FunctionalBlockPojoPrototype> annotationFunctionalBlocks, final List<UUID> uuids) {
		if (! annotationFunctionalBlocks.isEmpty() || matchingKeys.get(0).getChildren().isEmpty() && !uuids.isEmpty()) {
			paragraphBlock.setUid(matchingKeys.get(0).getUid());
			paragraphBlock.setChildren(uuids);
			final FunctionalBlockGenerationResult<GeneratedFrom> result = new FunctionalBlockGenerationResult<>(Operation.UPDATE,
					paragraphBlock, null);
			moduleBlockResults.add(result);
		} else {
			moduleBlockChildrenUuidList.add(matchingKeys.get(0).getUid());
		}
	}


	private int calculateProperLength(final AstNodePojo paragraph, final int endOffset) {
		final List<AstNodePojo> childs = paragraph.getChildren();
		int finalOffset = endOffset;
		for(final AstNodePojo child: childs) {
			final AstNodeLocation advancedModuleLocationV2 = child.getLocation();
			if (advancedModuleLocationV2 != null) {
				final Optional<Integer> offset = advancedModuleLocationV2.getRetracedOffset();
				final Optional<Integer> length = advancedModuleLocationV2.getRetracedLength();
				if (offset.isPresent() && length.isPresent()) {
					finalOffset =  Math.max(finalOffset, offset.get().intValue() + length.get().intValue());
				}
			}
		}
		return finalOffset;
		
	}

	private FunctionalBlockPojoPrototype createParagraphBlock(final EntityId projectId, final AstNodePojo paragraph,
			final ModulePart modulePart) {
		final FunctionalBlockPojoPrototype paragraphBlock = new FunctionalBlockPojoPrototype();
		paragraphBlock.setModuleParts(Arrays.asList(modulePart));
		paragraphBlock.setName(paragraph.getLabel());
		paragraphBlock.setDescription(NO_DESCRIPTION_AVAILABLE);
		paragraphBlock.setFlags(generateBlockMap(PARAGRAPH_BLOCK_GENERATION_ID, Set.of(FunctionalBlockType.STRUCTURAL), Boolean.TRUE));
		paragraphBlock.setProject(projectId);
		return paragraphBlock;
	}

	private Map<Long, UUID> generateAnnotationBlock(final ModulePojo module, final List<AnnotationPojo> annotations, final EntityId projectId,
			final List<FunctionalBlockGenerationResult<GeneratedFrom>> moduleBlockResults, final Map<UUID, Boolean> annotationBlockMapped) {
		final Map<Long, UUID> annotationMap = new HashMap<>();
		annotations.forEach(annotation -> {
				final Optional<ModuleLocation> location = annotation.getLocation();
				final ModulePart annotationModulePart;
				final Optional<FunctionalBlockPojo> existingAnnotationBlock = annotationBlockAlreadyExist(annotation, moduleBlockResults, annotationMap,
						annotationBlockMapped, projectId);
				if (location.isPresent() && existingAnnotationBlock.isEmpty()) {
					annotationModulePart = new ModulePart(module.getLinkHash(), new ModuleLocation(location.get().getOffset(), location.get().getLength()));
					final FunctionalBlockPojoPrototype annotationBlock = createAnnotationBlock(annotationModulePart, annotation);
					final GeneratedFrom annotationGeneratedFrom = GeneratedFrom.fromAnnotation(EntityId.of(annotation.getId()));
					final UUID annotationBlockId = annotationBlock.uid.required(false).orElseNonNull(UUID::randomUUID);
					annotationBlock.setUid(annotationBlockId);
					annotationBlock.setProject(projectId);
					annotationMap.put(annotation.getId(), annotationBlockId);
					annotationBlockMapped.put(annotationBlockId, Boolean.FALSE);
					final FunctionalBlockGenerationResult<GeneratedFrom> annotationResult = new FunctionalBlockGenerationResult<>(Operation.CREATE,
							annotationBlock, annotationGeneratedFrom);
					moduleBlockResults.add(annotationResult);
				}
		});
		return annotationMap;
	}

	private FunctionalBlockPojoPrototype createAnnotationBlock(final ModulePart annotationModulePart, final AnnotationPojo annotation) {
		final FunctionalBlockPojoPrototype annotationBlock = new FunctionalBlockPojoPrototype();
		annotationBlock.setModuleParts(Arrays.asList(annotationModulePart));
		annotationBlock.setName(annotation.getName());
		annotationBlock.setDescription(annotation.getName());
		annotationBlock.setFlags(generateBlockMap(ANNOTATION_BLOCK_GENERATION_ID, Set.of(FunctionalBlockType.FUNCTIONAL_UNIT), Boolean.TRUE));
		return annotationBlock;
	}

	private Optional<FunctionalBlockPojo> annotationBlockAlreadyExist(final AnnotationPojo annotation,
			final List<FunctionalBlockGenerationResult<GeneratedFrom>> moduleBlockResults, final Map<Long, UUID> annotationMap,
			final Map<UUID, Boolean> annotationBlockMapped, final EntityId projectId) {

		final Optional<FunctionalBlockPojo> existingAnnotationBlock = functionalBlockService
				.find(q -> q.ofProject(projectId).generatedFromAnnotation(EntityId.of(annotation.getId())).withType(FunctionalBlockType.FUNCTIONAL_UNIT)).stream().findAny();

		if (existingAnnotationBlock.isPresent()) {
			annotationMap.put(annotation.getId(), existingAnnotationBlock.get().getUid());
			annotationBlockMapped.put(existingAnnotationBlock.get().getUid(), Boolean.FALSE);
			if (existingAnnotationBlock.get().getDescription() != null && ( ! existingAnnotationBlock.get().getDescription().equals(annotation.getName())
						|| ! existingAnnotationBlock.get().getName().equals(annotation.getName()))) {
				final FunctionalBlockGenerationResult<GeneratedFrom> result = new FunctionalBlockGenerationResult<>(Operation.UPDATE,
						updateAnnotationBlock(existingAnnotationBlock.get().getUid(), annotation.getName()), null);
				moduleBlockResults.add(result);
			}
		}
		return existingAnnotationBlock;
	}

	private FunctionalBlockPojoPrototype updateAnnotationBlock(final UUID uuid, final String description) {
		final FunctionalBlockPojoPrototype annotationBlock = new FunctionalBlockPojoPrototype();
		annotationBlock.setName(description);
		annotationBlock.setDescription(description);
		annotationBlock.setUid(uuid);
		return annotationBlock;
	}
	
	private List<UUID> searchForMatchingParagraphAnnotationBlock(final ModulePart paragraphModulePart, final List<AnnotationPojo> annotations,
			final Map<Long, UUID> annotationMap, final Map<UUID, Boolean> annotationBlockMapped) {
		final ModuleLocation paragraphModuleLocation = paragraphModulePart.getLocation().orElse(null);
		final List<UUID> matchingParagraphAnnotationUuidList = new ArrayList<>();
		annotations.forEach(annotation -> {
			final ModuleLocation annotationModuleLocation = annotation.getLocation().orElse(null);
			if (paragraphModuleLocation != null && annotationModuleLocation != null &&
                    annotationModuleLocation.getOffset() >= paragraphModuleLocation.getOffset() &&
                    annotationModuleLocation.getOffset() + annotationModuleLocation.getLength()
					<= paragraphModuleLocation.getOffset() + paragraphModuleLocation.getLength()) {
				matchingParagraphAnnotationUuidList.add(annotationMap.get(annotation.getId()));
				annotationBlockMapped.put(annotationMap.get(annotation.getId()), Boolean.TRUE);
			} 
		});
		return matchingParagraphAnnotationUuidList;
	}
	
	private static Map<String, Object> generateBlockMap(final String generationId, final Set<FunctionalBlockType> blockType, final Boolean readOnly) {
		final Map<String, Object> map = new HashMap<>();
		map.put(FunctionalBlockFlag.READ_ONLY.name(), readOnly);
		map.put(FunctionalBlockFlag.GENERATED_BY.name(), generationId);
		map.put(FunctionalBlockFlag.TYPE.name(), blockType);
		map.put(FunctionalBlockFlag.GENERATED_AT.name(), Instant.now().toEpochMilli());
		return map;
	}

	@Override
	public void persistAdditionalData(final FunctionalBlockGenerationContext context, final UUID functionalBlockId, final GeneratedFrom data) {
		functionalBlockService.setGeneratedFrom(functionalBlockId, data);
	}

	@Override
	public Optional<String> getLockCategory(final FunctionalBlockGenerationContext context, @Nullable final EntityId inputData) {
		return Optional.of(MODULE_BLOCK_GENERATION_ID);
	}

	@Override
	public List<Long> getModuleIdsToLock(final FunctionalBlockGenerationContext context, @Nullable final EntityId moduleId) {
		if (moduleId == null) {
			throw new IllegalArgumentException("Cannot execute module block generation because the 'moduleId' is null");
		}
		return List.of(moduleId.getNid());
	}
}
