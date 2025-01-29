/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.functionalblocks.computation;

import innowake.lib.core.util.collection.Pair;
import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.server.functionalblocks.FunctionalBlockUtil;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SchedulerInfoService;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLink;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkType;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.scheduler.SchedulerEntryRelationshipPojo;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.hazelcast.jet.impl.util.Util.distinctBy;

/**
 * Computation for the reachability network based on the scheduler information.
 */
@Component
public class ReachabilitySchedulerNetworkComputation implements FunctionalBlockComputation<List<FunctionalBlockLink>> {

	private final FunctionalBlockService functionalBlockService;
	private final SchedulerInfoService schedulerInfoService;
	private static final int PAGE_LIMIT = 1000;

	@Autowired
	public ReachabilitySchedulerNetworkComputation(final FunctionalBlockService functionalBlockService, final SchedulerInfoService schedulerInfoService) {
		this.functionalBlockService = functionalBlockService;
		this.schedulerInfoService = schedulerInfoService;
	}

	@Override
	public boolean accept(final FunctionalBlockPojo functionalBlock) {
		return FunctionalBlockUtil.hasType(functionalBlock, FunctionalBlockType.REACHABILITY_NETWORK);
	}

	@Override
	public List<FunctionalBlockLink> compute(final FunctionalBlockPojo reachabilityNetwork, final ProgressMonitor progressMonitor) {
		final EntityId projectId = reachabilityNetwork.getProject();
		final List<FunctionalBlockLink> result = new ArrayList<>();
		Pagination paged = new Pagination(PAGE_LIMIT);
		int size;
		do { /* fetch relationships in pages instead of whole */
			progressMonitor.checkCanceled();
			final Paged<SchedulerEntryRelationshipPojo> pagedRelationships = schedulerInfoService.findRelationships(paged, q -> q.ofProject(projectId));
			final List<SchedulerEntryRelationshipPojo> relationships = pagedRelationships.getContent()
					.stream()
					/* removes duplicate relationships between modules */
					.filter(distinctBy(r -> new Pair<>(r.getPredecessorModule(), r.getSuccessorModule())))
					.collect(Collectors.toList());

			final List<EntityId> modules = relationships.stream()
					.flatMap(x -> Stream.of(x.getPredecessorModule(), x.getSuccessorModule()))
					.distinct()
					.map(EntityId::of)
					.collect(Collectors.toList());

			final List<FunctionalBlockPojo> upperBoundFunctionalBlocks = functionalBlockService.find(q -> {
				q.ofProject(projectId);
				q.withType(FunctionalBlockType.RA_UPPER_BOUND);
				q.withResolvedModuleParts(modules);
				q.withParent(p1 -> {
					p1.ofProject(projectId);
					p1.withTypes(Collections.singleton(FunctionalBlockType.RA_TOP_DOWN));
				});
			});

			final Map<UUID, UUID> resolvedModuleParts = functionalBlockService.getResolvedModuleParts(upperBoundFunctionalBlocks.stream()
							.map(FunctionalBlockPojo::getUid)
							.collect(Collectors.toList()))
					.entrySet()
					.stream()
					.filter(entry -> entry.getValue().size() == 1)
					.collect(Collectors.toMap(Map.Entry::getKey, entry -> entry.getValue()
							.get(0)
							.getModuleId()
							.getUid()));

			final Map<UUID, UUID> moduleToReachabilityBlock = upperBoundFunctionalBlocks.stream()
					.collect(Collectors.toMap(block -> resolvedModuleParts.get(block.getUid()), block -> block.getParents()
							.get(0)));

			final Map<String, Object> linksFlags = new HashMap<>();
			linksFlags.put(FunctionalBlockLinkFlag.TYPE.name(),
					List.of(FunctionalBlockLinkType.RA_FROM_SCHEDULER_INFO.name(), FunctionalBlockLinkType.DIRECTED.name()));
			linksFlags.put(FunctionalBlockLinkFlag.GENERATED_BY.name(),
					"ReachabilitySchedulerNetwork");

			relationships.stream()
					.filter(relationship -> moduleToReachabilityBlock.containsKey(relationship.getPredecessorModule())
							&& moduleToReachabilityBlock.containsKey(relationship.getSuccessorModule()))
					.map(relationship -> new FunctionalBlockLink(UUID.randomUUID(), reachabilityNetwork.getUid(),
							moduleToReachabilityBlock.get(relationship.getPredecessorModule()),
							moduleToReachabilityBlock.get(relationship.getSuccessorModule()), null, linksFlags, null))
					.forEach(result::add);

			size = pagedRelationships.getSize();
			paged = paged.nextPage();
		} while (size == PAGE_LIMIT);

		return result;
	}

	@Override
	public void persist(final UUID functionalBlockId, final List<FunctionalBlockLink> links) {
		if ( ! Objects.requireNonNull(links).isEmpty()) {
			functionalBlockService.setLinks(functionalBlockId, Objects.requireNonNull(links));
		}
	}
}
