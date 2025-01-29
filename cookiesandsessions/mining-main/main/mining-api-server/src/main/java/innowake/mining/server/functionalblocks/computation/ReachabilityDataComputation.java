/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.computation;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.server.functionalblocks.FunctionalBlockUtil;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLink;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import innowake.mining.shared.entities.functionalblocks.ReachabilityDataPojo;
import innowake.mining.shared.entities.functionalblocks.ReachabilityDataPojoPrototype;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Computation of {@link ReachabilityDataPojo} -- runs on functional blocks of type {@link FunctionalBlockType#RA_TOP_DOWN}.
 */
@Component
public class ReachabilityDataComputation implements FunctionalBlockComputation<Collection<ReachabilityDataPojoPrototype>> {

	private final FunctionalBlockService functionalBlockService;
	private final ModuleService moduleService;

	public ReachabilityDataComputation(final FunctionalBlockService functionalBlockService, final ModuleService moduleService) {
		this.functionalBlockService = functionalBlockService;
		this.moduleService = moduleService;
	}

	@Override
	public boolean accept(final FunctionalBlockPojo functionalBlock) {
		/* only runs on "top-down" reachability blocks for now, as I think we don't need it for bottom-up */
		return FunctionalBlockUtil.hasType(functionalBlock, FunctionalBlockType.RA_TOP_DOWN);
	}


	@Override
	public Map<FunctionalBlockPojo, Collection<ReachabilityDataPojoPrototype>> computeBatched(final List<FunctionalBlockPojo> functionalBlocks,
			final ProgressMonitor progressMonitor) {

		final Map<FunctionalBlockPojo, Collection<ReachabilityDataPojoPrototype>> results = new HashMap<>();
		if ( ! functionalBlocks.isEmpty()) {
			final EntityId project = functionalBlocks.get(0).getProject();
			functionalBlocks.forEach(fb -> {
				if (fb.isOfType(FunctionalBlockType.MERGE_PARENT)) {
					/* only for Merged Parent the reachability Computation will be done for each of the child */
					functionalBlockService.find(q -> q.byUids(fb.getChildren()).withType(FunctionalBlockType.RA_TOP_DOWN))
							.forEach(child -> extractReachabilityData(results, project, fb, child, progressMonitor));
				} else {
					/* for Other block the reachability Computation will be done for Reachability Blocks */
					extractReachabilityData(results, project, null, fb, progressMonitor);
				}
			});
		}
		return results;
	}

	private void extractReachabilityData(final Map<FunctionalBlockPojo, Collection<ReachabilityDataPojoPrototype>> results, final EntityId project,
										 final @Nullable FunctionalBlockPojo mergeParent, final FunctionalBlockPojo reachabilityDataFunctionalBlock,
										 final ProgressMonitor progressMonitor) {
		final List<FunctionalBlockPojo> upperBounds = new ArrayList<>();
		final List<FunctionalBlockPojo> lowerBounds = new ArrayList<>();
		final List<FunctionalBlockPojo> accessModules = new ArrayList<>();
		final List<FunctionalBlockPojo> callChains = new ArrayList<>();
		functionalBlockService.find(q -> q.byUids(reachabilityDataFunctionalBlock.getChildren())).forEach(child -> {
			if (FunctionalBlockUtil.hasType(child, FunctionalBlockType.RA_ACCESS_MODULE)) {
				accessModules.add(child);
			} else if (FunctionalBlockUtil.hasType(child, FunctionalBlockType.RA_UPPER_BOUND)) {
				upperBounds.add(child);
			} else if (FunctionalBlockUtil.hasType(child, FunctionalBlockType.RA_LOWER_BOUND)) {
				lowerBounds.add(child);
			} else if (FunctionalBlockUtil.hasType(child, FunctionalBlockType.CALL_CHAIN)) {
				callChains.add(child);
			}
		});

		if (upperBounds.size() != 1 || callChains.size() != 1) {
			throw new IllegalStateException("Expected exactly one upper bound and call chain for a reachability block");
		}
		final List<ReachabilityDataPojoPrototype> reachabilityData =
				calculateReachability(project, upperBounds.get(0), lowerBounds, accessModules, callChains.get(0), reachabilityDataFunctionalBlock,
						progressMonitor);
		if( ! reachabilityData.isEmpty()) {
			if (mergeParent != null) {
				results.computeIfAbsent(mergeParent, k -> new HashSet<>()).addAll(reachabilityData);
			} else {
				results.put(reachabilityDataFunctionalBlock, reachabilityData);
			}
		}
	}

	private List<ReachabilityDataPojoPrototype> calculateReachability(final EntityId project, final FunctionalBlockPojo upperBound,
			final List<FunctionalBlockPojo> lowerBounds, final List<FunctionalBlockPojo> accessModuleBlocks, final FunctionalBlockPojo callChain,
			final FunctionalBlockPojo reachabilityBlock, final ProgressMonitor progressMonitor) {
		final Map<FunctionalBlockPojo, EntityId> upperBoundModules = raBlocksToModule(project, List.of(upperBound));
		final Map<FunctionalBlockPojo, EntityId> lowerBoundModules = raBlocksToModule(project, lowerBounds);
		final Map<UUID, Pair<FunctionalBlockPojo, EntityId>> accessModules =
				raBlocksToModule(project, accessModuleBlocks).entrySet().parallelStream().map(entry -> Pair.of(entry.getKey(), entry.getValue())).collect(
						Collectors.toMap(p -> p.getLeft().getUid(), Function.identity()));
		final var callChainLinks = functionalBlockService.getLinks(callChain.getUid());
		final var cLinksMappedToChildB = callChainLinks.parallelStream().collect(Collectors.groupingBy(FunctionalBlockLink::getChildB));
		final var reachabilityBlockLinks = functionalBlockService.getLinks(reachabilityBlock.getUid());
		final var rLinksMappedToChildB = reachabilityBlockLinks.parallelStream().collect(Collectors.groupingBy(FunctionalBlockLink::getChildB));
		final var callChainModules = moduleBlocksToModule(project, callChain.getChildren());


		final List<ReachabilityDataPojoPrototype> result = new ArrayList<>();

		for (final Map.Entry<FunctionalBlockPojo, EntityId> upperBoundEntry : upperBoundModules.entrySet()) {
			progressMonitor.checkCanceled();
			final EntityId upperBoundModule = upperBoundEntry.getValue();
			if (lowerBoundModules.isEmpty()) {
					result.add(new ReachabilityDataPojoPrototype().setUid(UUID.randomUUID())
							.setUpperBoundModuleId(upperBoundModule));
			} else {
				for (final Map.Entry<FunctionalBlockPojo, EntityId> lowerBoundEntry : lowerBoundModules.entrySet()) {
					progressMonitor.checkCanceled();
					final FunctionalBlockPojo lowerBoundBlock = lowerBoundEntry.getKey();
					final var currentAccessModuleModules = rLinksMappedToChildB.getOrDefault(lowerBoundBlock.getUid(), List.of()).stream()
							.map(link -> accessModules.get(link.getChildA()))
							.filter(p -> Objects.nonNull(p.getRight()))
							.toList();
					final EntityId lowerBoundModule = lowerBoundEntry.getValue();
					if (currentAccessModuleModules.isEmpty()) {
						result.add(getReachabilityData(lowerBoundBlock,	Pair.of(upperBoundEntry.getKey(), upperBoundModule),
								Pair.of(lowerBoundEntry.getKey(), lowerBoundModule), callChainModules,
								cLinksMappedToChildB, rLinksMappedToChildB, null));
					} else {
						for (final var accessModuleModule : currentAccessModuleModules) {
							progressMonitor.checkCanceled();
							result.add(getReachabilityData(lowerBoundBlock,	Pair.of(upperBoundEntry.getKey(), upperBoundModule),
									Pair.of(lowerBoundEntry.getKey(), lowerBoundModule), callChainModules,
									cLinksMappedToChildB, rLinksMappedToChildB, accessModuleModule));
						}
					}
				}
			}
		}
		return result;
	}

	private Map<FunctionalBlockPojo, EntityId> raBlocksToModule(final EntityId projectId, final List<FunctionalBlockPojo> blocks) {
		final Map<FunctionalBlockPojo, List<UUID>> blockToChildMap = blocks.stream()
				.collect(Collectors.toMap(Function.identity(), FunctionalBlockPojo::getChildren));

		final Map<UUID, GeneratedFrom> generatedFrom = functionalBlockService.getGeneratedFrom(blockToChildMap.values().stream().flatMap(List::stream).toList());

		final List<String> linkHashes = generatedFrom.values().stream()
				.map(GeneratedFrom::getModuleLinkHash)
				.filter(Optional::isPresent)
				.map(Optional::get)
				.toList();

		final Map<String, EntityId> moduleIdsByLinkHash = moduleService.findModuleIdsByLinkHash(q -> q.ofProject(projectId).withLinkHashes(linkHashes));

		final Map<FunctionalBlockPojo, EntityId> ret = new HashMap<>(blocks.size());
		for (final FunctionalBlockPojo block : blocks) {
			final List<UUID> children = blockToChildMap.get(block);
			if (children.isEmpty()) {
				continue;
			}

			final GeneratedFrom gf = generatedFrom.get(children.get(0));
			if (gf == null) {
				continue;
			}

			final Optional<String> moduleLinkHash = gf.getModuleLinkHash();
			if (moduleLinkHash.isEmpty()) {
				continue;
			}

			final EntityId moduleId = moduleIdsByLinkHash.get(moduleLinkHash.get());
			if (moduleId == null) {
				continue;
			}

			ret.put(block, moduleId);
		}

		return ret;
	}

	private Map<UUID, EntityId> moduleBlocksToModule(final EntityId projectId, final List<UUID> blocks) {
		final Map<UUID, GeneratedFrom> generatedFrom = functionalBlockService.getGeneratedFrom(blocks);

		final List<String> linkHashes = generatedFrom.values().stream()
				.map(GeneratedFrom::getModuleLinkHash)
				.filter(Optional::isPresent)
				.map(Optional::get)
				.toList();

		final Map<String, EntityId> moduleIdsByLinkHash = moduleService.findModuleIdsByLinkHash(q -> q.ofProject(projectId).withLinkHashes(linkHashes));

		final Map<UUID, EntityId> ret = new HashMap<>(blocks.size());
		for (final UUID block : blocks) {
			final GeneratedFrom gf = generatedFrom.get(block);
			if (gf == null) {
				continue;
			}

			final Optional<String> moduleLinkHash = gf.getModuleLinkHash();
			if (moduleLinkHash.isEmpty()) {
				continue;
			}

			final EntityId moduleId = moduleIdsByLinkHash.get(moduleLinkHash.get());
			if (moduleId == null) {
				continue;
			}

			ret.put(block, moduleId);
		}

		return ret;
	}

	private ReachabilityDataPojoPrototype getReachabilityData(final FunctionalBlockPojo lowerBoundBlock,
			final Pair<FunctionalBlockPojo, EntityId> upperBoundModule,	final Pair<FunctionalBlockPojo, EntityId> lowerBoundModule,
			final Map<UUID, EntityId> callChainModules, final Map<UUID, List<FunctionalBlockLink>> callChainLinks,
			final Map<UUID, List<FunctionalBlockLink>> reachabilityBlockLinks, @Nullable final Pair<FunctionalBlockPojo, EntityId> accessModule) {

		final var prototype = new ReachabilityDataPojoPrototype()
				.setUid(UUID.randomUUID())
				.setUpperBoundModuleId(upperBoundModule.getRight())
				.setLowerBoundModuleId(lowerBoundModule.getRight());

		final List<String> accessType = new ArrayList<>();
		if (accessModule != null) {
			prototype.setAccessModuleId(accessModule.getRight());

			/* try to get the access type from reachability block first */
			final var rawAccessType = reachabilityBlockLinks.getOrDefault(lowerBoundModule.getLeft().getUid(), List.of()).stream()
					.filter(link -> link.getChildA().equals(accessModule.getLeft().getUid())).findFirst()
					.map(link -> link.getFlags().get(FunctionalBlockLinkFlag.RA_ACCESS_TYPE.name()))
					.orElse(lowerBoundBlock.getFlags().get(FunctionalBlockFlag.RA_ACCESS_TYPE.name()));
			if (rawAccessType != null) {
				if (rawAccessType instanceof Collection) {
					((Collection<?>) rawAccessType).stream().map(Object::toString).forEach(accessType::add);
				} else {
					accessType.add(rawAccessType.toString());
				}
				prototype.setAccessTypes(accessType);
			}
		}

		prototype.setIntermediateModules(findIntermediateModules(callChainModules, callChainLinks, upperBoundModule, lowerBoundModule, accessModule));

		return prototype;
	}

	private Set<EntityId> findIntermediateModules(final Map<UUID, EntityId> callChainModules,
			final Map<UUID, List<FunctionalBlockLink>> links, final Pair<FunctionalBlockPojo, EntityId> upperBoundModule,
			final Pair<FunctionalBlockPojo, EntityId> lowerBoundModule, final @Nullable Pair<FunctionalBlockPojo, EntityId> accessModule) {
		final var intermediateModules = new HashSet<EntityId>();
		final var upperBoundModuleUuid = upperBoundModule.getLeft().getChildren().get(0);
		final var lowerBoundModuleUuid = lowerBoundModule.getLeft().getChildren().get(0);
		final var modulePresentInPaths = new HashSet<UUID>();
		final var paths = new HashSet<Set<UUID>>();
		findPaths(links, upperBoundModuleUuid, accessModule == null ? lowerBoundModuleUuid : accessModule.getLeft().getChildren().get(0),
				paths, modulePresentInPaths, new HashSet<>(), new HashSet<>());
		paths.forEach(path -> path.forEach(
				uid -> Optional.ofNullable(callChainModules.get(uid)).ifPresent(intermediateModules::add)));
		intermediateModules.removeIf(
				module -> module.equals(upperBoundModule.getRight()) || module.equals(lowerBoundModule.getRight())
						|| (accessModule != null && module.equals(accessModule.getRight())));
		return intermediateModules;
	}

	private void findPaths(final Map<UUID, List<FunctionalBlockLink>> linksMappedToChildB, final UUID target, final UUID current, final Set<Set<UUID>> paths,
			final Set<UUID> modulePresentInPaths, final Set<UUID> currentPath, final Set<UUID> visited) {
		linksMappedToChildB.getOrDefault(current, List.of()).forEach(link -> {
			final UUID childA = link.getChildA();
			if (childA.equals(target) || modulePresentInPaths.contains(childA)) {
				paths.add(currentPath);
				modulePresentInPaths.addAll(currentPath);
			} else if ( ! visited.contains(childA)) {
				final var newPath = new HashSet<>(currentPath);
				newPath.add(childA);
				visited.add(childA);
				findPaths(linksMappedToChildB, target, childA, paths, modulePresentInPaths, newPath, visited);
			}
		});
	}

	@Override
	public void persist(final UUID functionalBlockId, final Collection<ReachabilityDataPojoPrototype> data) {
		functionalBlockService.setReachabilityData(functionalBlockId, Objects.requireNonNull(data));
	}

}
