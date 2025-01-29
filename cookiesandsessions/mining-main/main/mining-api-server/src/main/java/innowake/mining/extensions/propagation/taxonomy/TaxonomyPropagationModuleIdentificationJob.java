/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.propagation.taxonomy;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.lib.job.api.Status;
import innowake.mining.server.job.MiningJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.TaxonomyService;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.TaxonomyPojo;
import innowake.mining.shared.model.DatabaseAccessType;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.taxonomy.assignment.PropagationData;

/**
 * Job which handles {@link TaxonomyPropagationModuleIdentificationJob}
 */
public class TaxonomyPropagationModuleIdentificationJob extends MiningJob<ArrayList<PropagationData>> {

	private static final List<String> ACCESS_TYPES = List.of("FILE_ACCESS_TYPE", "DB_ACCESS_TYPE");
	private static final Logger LOG = LoggerFactory.getLogger(TaxonomyPropagationModuleIdentificationJob.class);

	private final List<RelationshipType> incomingReferences;
	private final List<RelationshipType> outgoingReferences;
	private List<EntityId> taxonomyEids = Collections.emptyList();
	private List<EntityId> startModules = Collections.emptyList();
	private final List<String> readsWritesAccesses;
	
	@Autowired
	private transient TaxonomyService taxonomyService;
	
	@Autowired
	private transient ModuleService moduleService;

	/**
	 * Constructor takes project id and {@link TaxonomyPropagationRequest}
	 *
	 * @param projectId id of the {@code project}.
	 * @param taxonomyPropagationRequest {@link TaxonomyPropagationRequest} for the project.
	 */
	public TaxonomyPropagationModuleIdentificationJob(final EntityId projectId, final TaxonomyPropagationRequest taxonomyPropagationRequest) {
		super(projectId, null);

		taxonomyEids = taxonomyPropagationRequest.getTaxonomyIds();
		startModules = taxonomyPropagationRequest.getModuleIds();
		incomingReferences = taxonomyPropagationRequest.getIncomingReferences();
		outgoingReferences = taxonomyPropagationRequest.getOutgoingReferences();
		readsWritesAccesses = taxonomyPropagationRequest.getReadsWritesAccesses().stream()
														.map(DatabaseAccessType::name)
														.collect(Collectors.toList());
	}

	@Override
	protected Result<ArrayList<PropagationData>> run(final ProgressMonitor progressMonitor) {
		LOG.info("Started TaxonomyPropagationModuleIdentificationJob");
		progressMonitor.setJobDescription("Taxonomy Propagation ModuleIdentification");

		final Set<EntityId> taxonomyIds = new HashSet<>(taxonomyService.findIds(q -> q.byIds(taxonomyEids)));
		final Map<EntityId, Set<EntityId>> fetchedModuleToTaxonomies = new HashMap<>();
		startModules.forEach(moduleId -> {
			fetchedModuleToTaxonomies.put(moduleService.getModuleEntityId(moduleId), taxonomyIds);
		});

		final int workUnits = (int) moduleService.countModules(q -> q.ofProject(projectId)) - startModules.size();
		progressMonitor.begin(workUnits);

		final Map<Long, Set<EntityId>> propagationModule = findPropagationModules(fetchedModuleToTaxonomies, progressMonitor, workUnits);
		final ArrayList<PropagationData> propagationDataList = propagationModule.entrySet().stream()
																	.map(e -> new PropagationData(e.getKey(), e.getValue()))
																	.collect(Collectors.toCollection(ArrayList::new));
		LOG.info("Completed TaxonomyPropagationModuleIdentificationJob");
		if (propagationDataList.isEmpty()) {
			return new Result<>(new Status(new IllegalArgumentException("No modules could be identified for propagation")));
		} else {
			final String successLogMessage = "Identified all the referenced modules that are to be participate in taxonomy propagation";
			LOG.info(successLogMessage);
			progressMonitor.setStepDescription(successLogMessage);
			return new Result<>(Status.OK, propagationDataList);
		}
	}
	
	/**
	 * Searches the dependent modules referenced by the input list of modules and propagate the taxonomies to them
	 */
	private final Map<Long, Set<EntityId>> findPropagationModules(final Map<EntityId, Set<EntityId>> startModulesMap, final ProgressMonitor progressMonitor,
														final int totalWorkUnits) {
		final Map<EntityId, Set<EntityId>> propagationModules = new HashMap<>();
		final Set<EntityId> moduleIds = new HashSet<>();
		int unitsWorked = 0;
		for (final Map.Entry<EntityId, Set<EntityId>> entry : startModulesMap.entrySet()) {
			moduleIds.add(entry.getKey());
			propagationModules.put(entry.getKey(), entry.getValue());
		}
		while ( ! moduleIds.isEmpty()) {
			LOG.debug("Find all the Referenced modules for {} modules.", moduleIds.size());
			progressMonitor.setStepDescription("Finding reference modules");
			final Map<UUID, Set<UUID>> modulesMap = findAllReferencedModules(moduleIds);
			moduleIds.clear();
			LOG.debug("Find Taxonomy details for Propagation");
			progressMonitor.setStepDescription("Find Taxonomy details for Propagation");

			final Set<EntityId> mods = modulesMap.values().stream()
					.flatMap(Collection::stream)
					.map(EntityId::of)
					.collect(Collectors.toSet());
			final Map<EntityId, Set<EntityId>> referenceTaxonomies = taxonomyService.findTaxonomyIdPerModule(q -> q.ofModules(mods));

			if (propagationModules.isEmpty()) {
				propagationModules.putAll(setPropagationData(modulesMap, referenceTaxonomies, startModulesMap, moduleIds));
			} else {
				propagationModules.putAll(setPropagationData(modulesMap, referenceTaxonomies, propagationModules, moduleIds));
			}
			if ( ! propagationModules.isEmpty() ) {
				final int propagationModulesSize = propagationModules.size();
				progressMonitor.worked(propagationModulesSize);
				unitsWorked += propagationModulesSize;
			}
		}
		progressMonitor.worked(totalWorkUnits - unitsWorked);

		/* Unfortunately we have to switch to module nid here */
		final Map<Long, Set<EntityId>> propagationModules2 = new HashMap<>();
		propagationModules.entrySet().forEach(e -> propagationModules2.put(moduleService.getModuleNid(e.getKey()), e.getValue()));

		filteringTaxonomies(propagationModules2);
		LOG.info("Identified all the taxonomies that are to be assigned to all the reference modules");
		progressMonitor.setStepDescription("identified all the taxonomies that are to be assigned to all the reference modules");
		return propagationModules2;
	}
	
	/**
	 * identify all the taxonomies that are to be assigned to all the reference modules
	 */
	private Map<EntityId, Set<EntityId>> setPropagationData(final Map<UUID, Set<UUID>> modulesMap, final Map<EntityId, Set<EntityId>> referenceTaxonomies,
			final Map<EntityId, Set<EntityId>> propagationMap, final Set<EntityId> moduleSet) {
		final Map<EntityId, Set<EntityId>> propogatingModules = new HashMap<>();
		for (final Entry<UUID, Set<UUID>> entry : modulesMap.entrySet()) {
			final Set<UUID> toModules = entry.getValue();
			final Set<EntityId> taxonomies = propagationMap.get(EntityId.of(entry.getKey()));
			if (taxonomies != null && ! taxonomies.isEmpty()) {
				for (final UUID toModule : toModules) {
					final EntityId dst = EntityId.of(toModule);
					propagateTaxonomies(dst, referenceTaxonomies.get(dst), taxonomies, propogatingModules, propagationMap, moduleSet);
				}
			}
		}
		return propogatingModules;
	}

	
	private void propagateTaxonomies(final EntityId toModule, final Set<EntityId> assignedTaxonomies, final Set<EntityId> taxonomies,
			final Map<EntityId, Set<EntityId>> propogatingModules, final Map<EntityId, Set<EntityId>> propagationMap, final Set<EntityId> moduleSet) {
		
		final Set<EntityId> propagatingTaxonomies;
		if (CollectionUtils.isNotEmpty(assignedTaxonomies)) {
			propagatingTaxonomies = taxonomies.stream().filter(taxonomy -> ! assignedTaxonomies.contains(taxonomy)).collect(Collectors.toSet());
		} else {
			propagatingTaxonomies = taxonomies;
		}
		final Set<EntityId> propagatedTaxonomies = propagationMap.get(toModule);
		if (propagatedTaxonomies != null && ! propagatedTaxonomies.isEmpty()) {
			propagatedTaxonomies.addAll(propagatingTaxonomies);
			if(propogatingModules.get(toModule) != null && ! propogatingModules.get(toModule).isEmpty()) {
				propagatedTaxonomies.addAll(propogatingModules.get(toModule));
			}
			propogatingModules.put(toModule, propagatedTaxonomies);
		} else if (! startModules.contains(toModule) && ! propagationMap.containsKey(toModule)) {
			moduleSet.add(toModule);
			if (propogatingModules.get(toModule) != null && ! propogatingModules.get(toModule).isEmpty()) {
				propagatingTaxonomies.addAll(propogatingModules.get(toModule));    
			}
			propogatingModules.put(toModule, propagatingTaxonomies);
		}
	}

	private Map<UUID, Set<UUID>> findAllReferencedModules(final Set<EntityId> moduleIds) {
		final Map<UUID, Set<UUID>> referenceMap = new HashMap<>();
		findReferences(referenceMap, moduleIds, incomingReferences, RelationshipDirection.IN);
		findReferences(referenceMap, moduleIds, outgoingReferences, RelationshipDirection.OUT);
		return referenceMap;
	}

	private void findReferences(final Map<UUID, Set<UUID>> referenceMap, final Set<EntityId> moduleIds, final List<RelationshipType> relationships,
			final RelationshipDirection direction) {
		if ( ! readsWritesAccesses.isEmpty()) {
			final List<ModuleRelationshipPojo> references = moduleService.findRelationship(q -> q.ofProject(projectId)
																			.ofModulesInDirection(EntityId.allUids(moduleIds), direction)
																			.withType(RelationshipType.ACCESSES)
																			.withProperties(ACCESS_TYPES, readsWritesAccesses, true));

			getReferenceMap(referenceMap, references, direction);
		}

		relationships.forEach(reference -> {
			final List<ModuleRelationshipPojo> references = moduleService.findRelationship(q -> q.ofProject(projectId)
																			.ofModulesInDirection(EntityId.allUids(moduleIds), direction)
																			.withTypes(relationships));

			getReferenceMap(referenceMap, references, direction);
		});
	}

	private void getReferenceMap(final Map<UUID, Set<UUID>> moduleMap, final List<ModuleRelationshipPojo> references, final RelationshipDirection direction) {
		for (final ModuleRelationshipPojo ref : references) {
			final var fromId = direction == RelationshipDirection.IN ? ref.getDstModule() : ref.getSrcModule();
			final var toId = direction == RelationshipDirection.IN ? ref.getSrcModule() : ref.getDstModule();
			final Set<UUID> toModules = moduleMap.computeIfAbsent(fromId, k -> new HashSet<>());
			toModules.add(toId);
		}
	}
	
	/**
	 * Filtering the Taxonomies that were already assigned to Modules.
	 * 
	 * @param
	 */
	private void filteringTaxonomies(final Map<Long, Set<EntityId>> propagationModules) {
		final Map<Long, List<TaxonomyPojo>> assignedTaxonomies = taxonomyService.findTaxonomiesPerModule(q -> q.ofModules(startModules))
																		.entrySet().stream()
																		.collect(Collectors.toMap(e -> e.getKey().getNid(), Entry::getValue));
		for (final Entry<Long, Set<EntityId>> entry : propagationModules.entrySet()) {
			final Set<EntityId> filteredTaxonomyIdsSet = new HashSet<>();
			final Set<EntityId> assignedTaxonomyIds = assignedTaxonomies.getOrDefault(entry.getKey(), Collections.emptyList())
					.stream()
					.map(TaxonomyPojo::identity)
					.collect(Collectors.toSet());
			for (final EntityId taxonomyId : entry.getValue()) {
				if ( ! assignedTaxonomyIds.contains(taxonomyId)) {
					filteredTaxonomyIdsSet.add(taxonomyId);
				}
			}
			LOG.debug("Number of taxonomies that need to be assigned to Module is {}", filteredTaxonomyIdsSet.size());
			entry.setValue(filteredTaxonomyIdsSet);
		}
		propagationModules.entrySet().removeIf(e -> e.getValue().isEmpty());
	}
}
