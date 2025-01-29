/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.functionalblocks.generation;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.functionalblocks.FunctionalBlockUtil;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import org.springframework.stereotype.Component;

import java.util.*;

/**
 * Generates a ({@link FunctionalBlockType#FUNCTIONAL_GROUP} containing all the Annotations of a module, with subgroups for each
 * structural unit of the module (e.g. method, paragraph).
 */
@Component
public class StructuralFunctionalBlockGeneration implements FunctionalBlockGeneration<EntityId, Void> {

	public static final String STRUCTURAL_FB_GENERATION_ID = "StructuralFunctionalBlockGeneration";

	private final FunctionalBlockService functionalBlockService;
	private final ModuleService moduleService;

	public StructuralFunctionalBlockGeneration(final FunctionalBlockService functionalBlockService, final ModuleService moduleService) {
		this.functionalBlockService = functionalBlockService;
		this.moduleService = moduleService;
	}

	@Override
	public Collection<FunctionalBlockGenerationResult<Void>> generate(final FunctionalBlockGenerationContext context, @Nullable final EntityId moduleId) {
		if (moduleId == null) {
			throw new IllegalArgumentException("Cannot execute module block generation because the 'moduleId' is null");
		}
		final ModulePojo module = moduleService.findAnyModule(q -> q.byId(moduleId))
				.orElseThrow(() -> new IllegalArgumentException("Cannot execute module block generation because the 'module' is null"));

		final Optional<FunctionalBlockPojo> moduleBlock = findModuleBlock(context, module);

		if (moduleBlock.isEmpty()) {
			return Collections.emptyList();
		}

		final List<FunctionalBlockPojo> existingStructuralFunctionalBlocks = findExistingStructuralFunctionalBlocks(context, module);

		if ( ! existingStructuralFunctionalBlocks.isEmpty()) {
			//TODO: support updating the existing blocks!
			return Collections.emptyList();
		}

		return generateStructuralBlock(context, moduleBlock.get());
	}

	private Optional<FunctionalBlockPojo> findModuleBlock(final FunctionalBlockGenerationContext context, final ModulePojo module) {
		return functionalBlockService.find(q -> q
				.ofProject(context.getProjectId())
				.withType(FunctionalBlockType.MODULE)
				.generatedFromModule(module.getLinkHash()))
				.stream()
				.findAny();
	}

	private List<FunctionalBlockPojo> findExistingStructuralFunctionalBlocks(final FunctionalBlockGenerationContext context, final ModulePojo module) {
		return functionalBlockService.find(q -> q
				.ofProject(context.getProjectId())
				.withType(FunctionalBlockType.FUNCTIONAL_GROUP)
				.withFlag(FunctionalBlockFlag.GENERATED_BY, STRUCTURAL_FB_GENERATION_ID)
				.withResolvedModulePart(module.identity()));
	}

	private List<FunctionalBlockGenerationResult<Void>> generateStructuralBlock(final FunctionalBlockGenerationContext context,
					final FunctionalBlockPojo block) {

		final List<FunctionalBlockGenerationResult<Void>> result = new ArrayList<>();

		final List<UUID> children = new ArrayList<>(block.getChildren().size());
		for (final FunctionalBlockPojo childBlock : functionalBlockService.get(block.getChildren())) {
			if (FunctionalBlockUtil.hasType(childBlock, FunctionalBlockType.STRUCTURAL)) {
				final List<FunctionalBlockGenerationResult<Void>> childResult = generateStructuralBlock(context, childBlock);
				if ( ! childResult.isEmpty()) {
					result.addAll(childResult);
					children.add(childResult.get(childResult.size() - 1).getFunctionalBlock().uid.getNonNull());
				}
			} else {
				children.add(childBlock.getUid());
			}
		}

		if ( ! children.isEmpty()) {
			final FunctionalBlockPojoPrototype prototype = new FunctionalBlockPojoPrototype();
			prototype.setUid(UUID.randomUUID());
			prototype.setProject(context.getProjectId());
			prototype.setFlags(Map.of(
					FunctionalBlockFlag.TYPE.name(), List.of(FunctionalBlockType.FUNCTIONAL_GROUP),
					FunctionalBlockFlag.GENERATED_BY.name(), STRUCTURAL_FB_GENERATION_ID
			));
			prototype.setName(block.getName());
			prototype.setDescription("");
			prototype.setChildren(children);
			result.add(new FunctionalBlockGenerationResult<>(FunctionalBlockGenerationResult.Operation.CREATE, prototype));
		}

		return result;
	}

	@Override
	public Optional<String> getLockCategory(final FunctionalBlockGenerationContext context, @Nullable final EntityId inputData) {
		return Optional.of(STRUCTURAL_FB_GENERATION_ID);
	}

	@Override
	public List<Long> getModuleIdsToLock(final FunctionalBlockGenerationContext context, @Nullable final EntityId moduleId) {
		if (moduleId == null) {
			throw new IllegalArgumentException("Cannot execute module block generation because the 'moduleId' is null");
		}
		return List.of(moduleId.getNid());
	}
}
