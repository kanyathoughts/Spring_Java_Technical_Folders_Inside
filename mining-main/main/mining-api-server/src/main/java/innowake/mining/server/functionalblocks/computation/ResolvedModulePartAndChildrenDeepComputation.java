/*
*Copyright (c) 2023 Deloitte. All rights reserved.
*/
package innowake.mining.server.functionalblocks.computation;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.ModulePart;
import innowake.mining.shared.entities.functionalblocks.ResolvedModulePart;
import innowake.mining.shared.model.ModuleLocation;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Computation that calculates the union of all module parts of the given block and its children (recursively) and also resolves the
 * module link hash of module parts to the actual module ids.
 * <p>
 * The union of module parts is calculated as follows:
 * <ol>
 *     <li>make a collection of all module parts of the given block and all child blocks, recursively</li>
 *     <li>group the module parts by linkHash</li>
 *     <li>for each group, check if it contains a module part where {@code location} is {@code null}. If that is the case, remove all other module parts from the group</li>
 *     <li>for groups that contain only non-null locations, merge adjacent and overlapping regions by following this algorithm:
 *          <ol>
 *              <li>sort the module parts by {@code location.offset}</li>
 *              <li>for each part, check if the {@code offset} of the next part is less than or equal to {@code offset + length + 1} of the current part
 *                  (in other words, check if the next part starts within or directly after the current part)</li>
*               <li>if that is true, then merge the current part with the next part by removing the current and next part from the list and replacing it with
 *                  a new part where {@code offset=current.offset} and {@code length=max(current.offset + current.length, next.offset + next.length) - offset}</li>
*               <li>if a merge was done, then continue with the newly created (merged) part as current, otherwise continue with next in the list</li>
 *          </ol>
 *     </li>
 *     <li>iterate over all module parts from all groups, resolve the linkHash to module id and collect the result as {@code ResolvedModulePartComputation} objects</li>
 * </ol>
 * <p>
 * Since this computation needs to retrieve the recursive list of children for each block, it also persists this list (as cached information) via
 * {@link FunctionalBlockService#setChildrenDeep(UUID, List)}.
 */
@Component
@Order(Ordered.HIGHEST_PRECEDENCE) /* runs before any other computation */
public class ResolvedModulePartAndChildrenDeepComputation implements FunctionalBlockComputation<Pair<List<UUID>, List<ResolvedModulePart>>> {

	private final FunctionalBlockService functionalBlockService;
	private final ModuleService moduleService;

	public ResolvedModulePartAndChildrenDeepComputation(final FunctionalBlockService functionalBlockService, final ModuleService moduleService) {
		this.functionalBlockService = functionalBlockService;
		this.moduleService = moduleService;
	}

	@Override
	public boolean accept(final FunctionalBlockPojo functionalBlock) {
		/* REACHABILITY_NETWORK aggregates all RA_TOP_DOWN blocks, so can in worst-case contain the entire project, transitively.
		 * We should not attempt to calculate its childrenDeep and ResolvedModuleParts.
		 * The same would apply to all other blocks of that sort, which aggregate together a huge number of other blocks.
		 */
		return ! functionalBlock.isOfType(FunctionalBlockType.REACHABILITY_NETWORK);
	}


	@Override
	public Map<FunctionalBlockPojo,Pair<List<UUID>, List<ResolvedModulePart>>> computeBatched(final List<FunctionalBlockPojo> functionalBlocks,
			final ProgressMonitor progressMonitor) {

		final Map<FunctionalBlockPojo,Pair<List<UUID>, List<ResolvedModulePart>>> finalMap = new HashMap<>();
		final EntityId projectId = functionalBlocks.get(0).getProject();
		final Map<UUID, List<UUID>> childrenDeepOfParents = functionalBlockService.findChildrenIdsDeep
				     (functionalBlocks.stream().map(FunctionalBlockPojo::getUid).toList(), -1);

		final Map<FunctionalBlockPojo, List<UUID>> functionalBlockPojoMap =
				generateAMapOfBlockAndChildren(childrenDeepOfParents, functionalBlocks);

		final Map<UUID, List<ModulePart>> moduleParts = functionalBlockService.getModuleParts(childrenDeepOfParents.values().stream()
				.flatMap(List::stream).toList());

		final Map<String, EntityId> allModuleIds = getAllModuleIds(projectId, moduleParts.values().stream().flatMap(List::stream));

		for (final var entry : functionalBlockPojoMap.entrySet()) {
			final List<ResolvedModulePart> resolved = new ArrayList<>();
			final List<UUID> childrenDeep = entry.getValue();
			if (childrenDeep != null && ! childrenDeep.isEmpty()) {
				final Map<String, List<ModulePart>> groupedData = groupByLinkHashCheckNullLocations(getAllModuleParts(entry.getKey(), childrenDeep, moduleParts));
				for (final Map.Entry<String, List<ModulePart>> input : groupedData.entrySet()) {
					final String linkHash = input.getKey();
					final EntityId moduleId = allModuleIds.get(linkHash);
					if (moduleId != null) {
						final List<ModulePart> parts = input.getValue();
						calculateResolved(resolved, moduleId, parts);
					}
				}
				final Pair<List<UUID>, List<ResolvedModulePart>> pairs = Pair.of(childrenDeep, resolved);
				finalMap.put(entry.getKey() ,pairs);
			}
		}

		return finalMap;

	}

	private void calculateResolved(final List<ResolvedModulePart> resolved,
								   final EntityId moduleId,
			                       @Nullable final List<ModulePart> moduleParts) {

		if (moduleParts != null) {
			for (final ModulePart modulepart : mergeModuleParts(moduleParts)) {
				final ResolvedModulePart resolvedModulePart = new ResolvedModulePart(moduleId,
						modulepart.getLocation().orElseThrow(IllegalStateException::new));
				resolved.add(resolvedModulePart);
			}
		} else {
			resolved.add(new ResolvedModulePart(moduleId));
		}
	}

	private Map<FunctionalBlockPojo, List<UUID>>  generateAMapOfBlockAndChildren(final Map<UUID, List<UUID>> childrenDeepOfParents,
			                                     final List<FunctionalBlockPojo> functionalBlockPojos
			                                    ) {
        final Map<FunctionalBlockPojo, List<UUID>> functionalBlockPojoMap = new HashMap<>();
		for (final FunctionalBlockPojo block : functionalBlockPojos) {
			functionalBlockPojoMap.put(block, Optional.ofNullable(childrenDeepOfParents.get(block.getUid())).orElse(Collections.emptyList()));
		}
        return functionalBlockPojoMap;
	}

	@Override
	public void persist(final UUID functionalBlockId, final Pair<List<UUID>, List<ResolvedModulePart>> data) {
		functionalBlockService.setChildrenDeep(functionalBlockId, Objects.requireNonNull(data, "Data must not be null").getLeft());
		functionalBlockService.setResolvedModuleParts(functionalBlockId, data.getRight());
	}

	private Map<String, EntityId> getAllModuleIds(final EntityId projectId, final Stream<ModulePart> moduleParts) {
		final List<String> linkHashes = moduleParts.map(ModulePart::getModuleLinkHash).toList();
		if (linkHashes.isEmpty()) {
			return Collections.emptyMap();
		}
		return moduleService.findModuleIdsByLinkHash(q -> q.ofProject(projectId).withLinkHashes(linkHashes));
	}

	private List<ModulePart> getAllModuleParts(final FunctionalBlockPojo functionalBlock, final List<UUID> childrenDeep, final Map<UUID, List<ModulePart>> moduleParts) {
		final List<ModulePart> ret = new ArrayList<>(functionalBlock.getModuleParts());

		for (final UUID child : childrenDeep) {
			final List<ModulePart> childParts = moduleParts.get(child);
			if (childParts != null) {
				ret.addAll(childParts);
			}
		}

		return ret;
	}


	private Map<String, List<ModulePart>> groupByLinkHashCheckNullLocations(final List<ModulePart> moduleParts) {
		final Map<String, List<ModulePart>> groupedModuleParts = moduleParts.stream().collect(Collectors.groupingBy(ModulePart::getModuleLinkHash));
		groupedModuleParts.forEach((key, list) -> {
			if (list.stream().anyMatch(modulePart -> modulePart.getLocation().isEmpty())) {
				groupedModuleParts.put(key, null);
			}
		});
		return groupedModuleParts;
	}

	private List<ModulePart> mergeModuleParts(final List<ModulePart> moduleParts) {
		final List<ModulePart> mergedEntries = new ArrayList<>();
		ModulePart currentMergedEntry = null;
		moduleParts.sort((modulepart1, modulepart2) -> modulepart1.getLocation().get().getOffset().compareTo(modulepart2.getLocation().get().getOffset()));
		for (final ModulePart modulepart : moduleParts) {
			if (currentMergedEntry == null) {
				currentMergedEntry = modulepart;
			} else {
				final ModuleLocation nextLocation = modulepart.getLocation().orElseThrow(IllegalStateException::new);
				final ModuleLocation currentLocation = currentMergedEntry.getLocation()
						.orElseThrow(IllegalStateException::new);
				if (nextLocation.getOffset() <= currentLocation.getOffset()
						+ currentLocation.getLength() + 1) {
					currentMergedEntry = mergeModulePartBasedOnLocations(currentMergedEntry.getModuleLinkHash(), currentLocation, nextLocation);
				} else {
					mergedEntries.add(currentMergedEntry);
					currentMergedEntry = modulepart;
				}
			}
		}
		if (currentMergedEntry != null) {
			mergedEntries.add(currentMergedEntry);
		}
		return mergedEntries;

	}

	private ModulePart mergeModulePartBasedOnLocations(final String linkHash, final ModuleLocation current, final ModuleLocation next) {
		final int newOffset = current.getOffset();
		final int newEnd = Math.max(current.getOffset() + current.getLength(),
				next.getOffset() + next.getLength());
		final int newLength = newEnd - newOffset;
		return new ModulePart(linkHash, new ModuleLocation(newOffset, newLength));
	}
}
