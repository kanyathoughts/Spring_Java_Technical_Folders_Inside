/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.computation;

import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.server.functionalblocks.FunctionalBlockUtil;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.entities.functionalblocks.*;
import innowake.mining.shared.model.FeatureId;
import org.ff4j.FF4j;
import org.springframework.stereotype.Component;
import org.apache.commons.lang3.tuple.Pair;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Generates links between blocks of type {@link FunctionalBlockType#RA_TOP_DOWN} inside the {@link FunctionalBlockType#REACHABILITY_NETWORK} block
 * based on resource access.
 */
@Component
public class ReachabilityResourceNetworkComputation implements FunctionalBlockComputation<List<FunctionalBlockLink>> {

	private final FunctionalBlockService functionalBlockService;

	private final FF4j ff4j;

	public ReachabilityResourceNetworkComputation(final FunctionalBlockService functionalBlockService, final FF4j ff4j) {
		this.functionalBlockService = functionalBlockService;
		this.ff4j = ff4j;
	}

	@Override
	public boolean accept(final FunctionalBlockPojo functionalBlock) {
		if ( ! ff4j.getFeature(FeatureId.REACHABILITY_RESOURCE_NETWORK.getId()).isEnable()) {
			return false;
		}

		/* this computation runs only on the REACHABILITY_NETWORK block and only if the block was modified since the last computation */
		if (FunctionalBlockUtil.hasType(functionalBlock, FunctionalBlockType.REACHABILITY_NETWORK)) {
			final Long generatedAt = (Long) functionalBlock.getFlags().get(FunctionalBlockFlag.GENERATED_AT.name());
			final Long computedAt = (Long) functionalBlock.getFlags().get(FunctionalBlockFlag.COMPUTED_AT.name());
			return generatedAt == null || computedAt == null || generatedAt.compareTo(computedAt) > 0;
		}
		return false;
	}

	@Override
	public List<FunctionalBlockLink> compute(final FunctionalBlockPojo reachabilityNetwork, final ProgressMonitor progressMonitor) {
		final List<FunctionalBlockPojo> lowerBoundModules = functionalBlockService.find(q -> {
			q.ofProject(reachabilityNetwork.getProject());
			q.withType(FunctionalBlockType.MODULE);
			q.withParent(p1 -> {
				p1.ofProject(reachabilityNetwork.getProject());
				p1.withType(FunctionalBlockType.RA_LOWER_BOUND);
				p1.withParent(p2 -> {
					p2.ofProject(reachabilityNetwork.getProject());
					p2.withType(FunctionalBlockType.RA_TOP_DOWN);
				});
			});
		});

		final Set<Pair<FunctionalBlockPojo, FunctionalBlockPojo>> readingLowerBoundsAndModules = new HashSet<>();
		final Set<Pair<FunctionalBlockPojo, FunctionalBlockPojo>> writingLowerBoundsAndModules = new HashSet<>();
		for (final FunctionalBlockPojo module : lowerBoundModules) {
			progressMonitor.checkCanceled();
			final List<FunctionalBlockPojo> parents = functionalBlockService.find(q -> q
					.ofProject(reachabilityNetwork.getProject())
					.withType(FunctionalBlockType.RA_LOWER_BOUND)
					.withParent(p -> p.withType(FunctionalBlockType.RA_TOP_DOWN))
					.withChild(c -> c.byUid(module.getUid())));
			parents.stream().filter(p -> checkFlagValueByKey(p.getFlags(), "READ"))
					.forEach(p -> readingLowerBoundsAndModules.add(Pair.of(p, module)));
			parents.stream().filter(p -> checkFlagValueByKey(p.getFlags(), "WRITE"))
					.forEach(p -> writingLowerBoundsAndModules.add(Pair.of(p, module)));
		}

		final var readers = getReachabilityBlocks(readingLowerBoundsAndModules, true, progressMonitor);
		final var writers = getReachabilityBlocks(writingLowerBoundsAndModules, false, progressMonitor);

		return calculateDirectedLinks(reachabilityNetwork, readers, writers, progressMonitor);
	}

	private List<FunctionalBlockLink> calculateDirectedLinks(final FunctionalBlockPojo reachabilityNetwork,
			final Map<FunctionalBlockPojo, Set<FunctionalBlockPojo>> readers, final Map<FunctionalBlockPojo, Set<FunctionalBlockPojo>> writers,
			final ProgressMonitor progressMonitor) {

		final var links = new HashMap<Pair<FunctionalBlockPojo, FunctionalBlockPojo>, FunctionalBlockLink>();

		// loop over all modules that have the WRITE flag
		for (final Map.Entry<FunctionalBlockPojo, Set<FunctionalBlockPojo>> writer : writers.entrySet()) {
			progressMonitor.checkCanceled();
			// get the writing blocks
			final var topDownWritingBlocks = writer.getValue();
			// get the reading blocks
			final var topDownReadingBlocks = readers.get(writer.getKey());
			if (topDownWritingBlocks != null && topDownReadingBlocks != null) {
				calculateLinksBetweenBlocks(reachabilityNetwork, links, writer.getKey(), topDownWritingBlocks, topDownReadingBlocks, progressMonitor);
			}
		}
		return new ArrayList<>(links.values());
	}

	private void calculateLinksBetweenBlocks(final FunctionalBlockPojo reachabilityNetwork,
			final HashMap<Pair<FunctionalBlockPojo,FunctionalBlockPojo>, FunctionalBlockLink> links,
			final FunctionalBlockPojo writer,
			final Set<FunctionalBlockPojo> topDownWritingBlocks,
			final Set<FunctionalBlockPojo> topDownReadingBlocks,
			final ProgressMonitor progressMonitor) {
		for (final var writingBlock : topDownWritingBlocks) {
			for (final var readingBlock : topDownReadingBlocks) {
				progressMonitor.checkCanceled();
				//check if readingBlock and writingBlock are the same
				if(writingBlock.equals(readingBlock)) {
					continue;
				}
				// check if the link already exists
				final var pair = Pair.of(writingBlock, readingBlock);
				var link = links.get(pair);
				if (link == null) {
					// create the link between writerBlock and readerBlock, using the writer as shared resource
					link = createLink(reachabilityNetwork, writingBlock, readingBlock);
				}
				//add module as shared resource to link
				putSharedResourceFlag(link, writer.getUid());

				links.put(pair, link);
			}
		}
	}

	@SuppressWarnings("unchecked")
	private void putSharedResourceFlag(final FunctionalBlockLink link, final UUID sharedResourceId) {
		final Object existingFlags = link.getFlags().get(FunctionalBlockLinkFlag.RA_SHARED_RESOURCE_ID.name());

		if (existingFlags instanceof Collection) {
			final Collection<Object> existingFlagsCollection = (Collection<Object>) existingFlags;
			final Collection<UUID> flagsCollection;
			if (existingFlagsCollection.stream().anyMatch(flag -> ! (flag instanceof UUID))) {
				flagsCollection = existingFlagsCollection.parallelStream().map(fb -> UUID.fromString(fb.toString())).collect(Collectors.toSet());
			} else {
				flagsCollection = (Collection<UUID>) existingFlags;
			}
			flagsCollection.add(sharedResourceId);
			link.getFlags().put(FunctionalBlockLinkFlag.RA_SHARED_RESOURCE_ID.name(), flagsCollection);
		} else {
			final var sharedResources = new HashSet<UUID>();
			sharedResources.add(sharedResourceId);
			link.getFlags().put(FunctionalBlockLinkFlag.RA_SHARED_RESOURCE_ID.name(), sharedResources);
		}
	}

	private FunctionalBlockLink createLink(final FunctionalBlockPojo reachabilityNetwork, final FunctionalBlockPojo fromBlock,
			final FunctionalBlockPojo toBlock) {
		final Map<String, Object> linksFlags = new HashMap<>();
		linksFlags.put(FunctionalBlockLinkFlag.TYPE.name(),
				List.of(FunctionalBlockLinkType.RA_SHARED_RESOURCE.name(), FunctionalBlockLinkType.DIRECTED.name()));
		linksFlags.put(FunctionalBlockLinkFlag.GENERATED_BY.name(), "ReachabilityResourceNetwork");

		return new FunctionalBlockLink(UUID.randomUUID(), reachabilityNetwork.getUid(), fromBlock.getUid(), toBlock.getUid(), null, linksFlags, null);
	}

	@Override
	public void persist(final UUID functionalBlockId, final List<FunctionalBlockLink> links) {
		if ( ! Objects.requireNonNull(links).isEmpty()) {
			functionalBlockService.setLinks(functionalBlockId, Objects.requireNonNull(links));
		}
	}

	private Map<FunctionalBlockPojo, Set<FunctionalBlockPojo>> getReachabilityBlocks(
			final Set<Pair<FunctionalBlockPojo, FunctionalBlockPojo>> lowerBoundsAndModules, final boolean isReadAccess,
			final ProgressMonitor progressMonitor) {
		final Map<FunctionalBlockPojo, Set<FunctionalBlockPojo>> map = new HashMap<>();
		final var accessType = isReadAccess ? "READ" : "WRITE";
		for (final Pair<FunctionalBlockPojo, FunctionalBlockPojo> pair : lowerBoundsAndModules) {
			progressMonitor.checkCanceled();
			final FunctionalBlockPojo lowerBound = pair.getLeft();
			final FunctionalBlockPojo module = pair.getRight();
			final var reachabilityBlocks = functionalBlockService.find(q -> q
					.ofProject(lowerBound.getProject())
					.withType(FunctionalBlockType.RA_TOP_DOWN)
					.withChildForMergedAndSingleReachabilityBlocks(c -> c.byUid(lowerBound.getUid()))
					.notWithParent(p -> p.withType(FunctionalBlockType.MERGE_PARENT)))
					.stream()
					.collect(Collectors.toMap(FunctionalBlockPojo::getUid, b -> b));

			final var links = functionalBlockService.getLinks(reachabilityBlocks.keySet());
			final var accessors = links.entrySet().parallelStream()
					.filter(e -> e.getValue().stream().anyMatch(l -> l.getChildB().equals(lowerBound.getUid()) && checkFlagValueByKey(l.getFlags(), accessType)))
					.map(Map.Entry::getKey)
					.collect(Collectors.toSet());
			final Set<FunctionalBlockPojo> blocksForModule = map.computeIfAbsent(module, k -> new HashSet<>());
			accessors.forEach(a -> Optional.ofNullable(reachabilityBlocks.get(a)).ifPresent(blocksForModule::add));
		}
		return map;
	}

	private boolean checkFlagValueByKey(final Map<String, Object> map, final String value) {
		final Object o = map.get(FunctionalBlockFlag.RA_ACCESS_TYPE.name());
		if (o == null) {
			return false;
		}
		if (o instanceof Collection) {
			return ((Collection<?>) o).contains(value);
		}
		return o.toString().equals(value);
	}

}
