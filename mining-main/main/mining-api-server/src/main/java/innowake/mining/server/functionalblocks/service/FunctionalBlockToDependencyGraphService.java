package innowake.mining.server.functionalblocks.service;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.tuple.Pair;
import org.springframework.stereotype.Service;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.datapoints.FilterObjectCoercionService;
import innowake.mining.data.datapoints.FilterObjectService;
import innowake.mining.server.functionalblocks.FunctionalBlockUtil;
import innowake.mining.server.graphql.controller.FunctionalBlocksGraphQlController;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.datapoints.definition.filters.FilterOperators;
import innowake.mining.shared.entities.MiningPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModuleRelationshipPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFieldName;
import innowake.mining.shared.model.aggregations.AggregationOperator;
import innowake.mining.shared.model.functionalblocks.ReachabilityBlockGraphFilterRequest;
import innowake.mining.shared.model.functionalblocks.ReachabilityNetworkGraphFilterRequest;
import innowake.mining.shared.model.functionalblocks.TechnologyType;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLink;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockStatus;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import innowake.mining.shared.entities.functionalblocks.ResolvedModulePart;
import innowake.mining.shared.model.Creator;
import innowake.mining.shared.model.Identification;
import innowake.mining.shared.model.Origin;
import innowake.mining.shared.model.RelationshipDirection;
import innowake.mining.shared.model.RelationshipType;
import innowake.mining.shared.model.Storage;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.mining.shared.model.dependency.graph.DependencyGraph;

/**
 * Converts the {@linkplain FunctionalBlockService#getLinks(UUID) links} of a functional block into {@link DependencyGraph} data structure.
 */
@Service
public class FunctionalBlockToDependencyGraphService {
	/* we put the ID of the corresponding functional block into the info map of the returned "Modules" */
	public static final String FUNCTIONAL_BLOCK_ID_INFO_PROPERTY = "functionalBlockId";

	public static final String PEER_COUNT = "peerCount";
	public static final String DELETED_CHILDREN = "deletedChildren";
	private final FunctionalBlockService functionalBlockService;
	private final FilterObjectService filterObjectService;
	private final FilterObjectCoercionService filterObjectCoercionService;
	private final ModuleService moduleService;

	public FunctionalBlockToDependencyGraphService(final FilterObjectService filterObjectService, final FunctionalBlockService functionalBlockService,
			final FilterObjectCoercionService filterObjectCoercionService, final ModuleService moduleService) {
		this.filterObjectService = filterObjectService;
		this.functionalBlockService = functionalBlockService;
		this.filterObjectCoercionService = filterObjectCoercionService;
		this.moduleService = moduleService;
	}

	/**
	 * Converts the links found on a Reachability Block FunctionalBlockPojo into a DependencyGraph object.
	 * @param block the functional block
	 * @param dependencyGraphFilterRequest the filter request
	 * @return DependencyGraph object for Links Found on FunctionalBlockPojo.
	 */
	public DependencyGraph toFunctionalBlockGraph(final FunctionalBlockPojo block, @Nullable final ReachabilityBlockGraphFilterRequest dependencyGraphFilterRequest) {
		if (block.isOfType(FunctionalBlockType.CALL_CHAIN)) {
			return getReachabilityBlockDependencyGraph(block, functionalBlockService.getLinks(block.getUid()), dependencyGraphFilterRequest);
		} else if (FunctionalBlockUtil.hasType(block, FunctionalBlockType.MERGE_PARENT)) {
			final var callChainBlocks = functionalBlockService.findChildrenDeep(block.getUid(), 2, q -> q.withType(FunctionalBlockType.CALL_CHAIN)
					.withParent(p -> p.withType(FunctionalBlockType.RA_TOP_DOWN).withFlag(FunctionalBlockFlag.DELETED, false)));
			final var links = functionalBlockService.getLinks(callChainBlocks.stream().map(FunctionalBlockPojo::getUid).collect(Collectors.toSet()))
					.values().parallelStream().flatMap(List::parallelStream).distinct()
					.collect(Collectors.toList());
			return getReachabilityBlockDependencyGraph(block, links, dependencyGraphFilterRequest);
		} else {
			return new DependencyGraph(List.of(), List.of(), Set.of());
		}
	}

	/**
	 * Converts the links found on a Reachability Network FunctionalBlockPojo into a DependencyGraph object.
	 * @param uid the functional block uid
	 * @param filterRequest the filter object
	 * @return DependencyGraph object for Links Found on FunctionalBlockPojo.
	 */
	public Optional<DependencyGraph> toReachabilityNetworkGraph(final UUID uid, final ReachabilityNetworkGraphFilterRequest filterRequest) {
		final Map<String, Object> filterObject = Objects.requireNonNullElse(filterRequest.getFilterObject(), new HashMap<>());
		final var foundBlock = functionalBlockService.find(uid);
		if (foundBlock.isPresent() && foundBlock.get().isOfType(FunctionalBlockType.REACHABILITY_NETWORK)) {
			final var networkBlock = foundBlock.get();
			final var linkType = filterRequest.getFunctionalBlockLinkType();
			final boolean hideInactiveBlocks;
			if (Objects.requireNonNullElse(filterObject, new HashMap<>()).containsKey("content_status")) {
				final var contentStatus = filterObject.get("content_status");
				hideInactiveBlocks = contentStatus.equals(Map.of("notEq", FunctionalBlockStatus.INACTIVE.name())) ||
						contentStatus.equals(Map.of("eq", FunctionalBlockStatus.ACTIVE.name()));
			} else {
				hideInactiveBlocks = false;
			}
			final Map<UUID, ModulePojo> moduleMap = getNetworkModules(networkBlock, filterObject);
			if ( ! moduleMap.isEmpty()) {
				final Set<TechnologyType> technologyTypes = filterRequest.getTechnologyTypes();
				final List<ModuleRelationshipPojo> references = getNetworkGraphReferences(networkBlock, linkType, hideInactiveBlocks, technologyTypes);
				final var filteredOutModules = networkBlock.getChildren().parallelStream().filter(c -> ! moduleMap.containsKey(c)).collect(Collectors.toSet());
				final List<ModuleRelationshipPojo> filteredReferences = getFilteredNetworkGraphReferences(filteredOutModules, references, filterObject);
				final List<ModuleRelationshipPojo> mergedBiDirectionalReferences = mergeBiDirectionalReferences(filteredReferences);
				final List<ModulePojo> modules;
				if (linkType != null) {
					modules = filteredReferences.parallelStream()
									.flatMap(ref -> Stream.of(ref.getSrcModule(), ref.getDstModule()))
									.map(moduleMap::get)
									.filter(Objects::nonNull)
									.distinct()
									.collect(Collectors.toList());
				} else {
					modules = new ArrayList<>(moduleMap.values());
				}
				final Set<Long> rootModuleIds = modules.parallelStream().map(ModulePojo::getId).collect(Collectors.toSet());
				return Optional.of(new DependencyGraph(modules, mergedBiDirectionalReferences, rootModuleIds));
			}
			return Optional.of(new DependencyGraph(List.of(), List.of(), Set.of()));
		}
		return Optional.empty();
	}

	private List<ModuleRelationshipPojo> getNetworkGraphReferences(final FunctionalBlockPojo networkBlock, final Object linkType,
			final boolean hideInactiveBlocks, @Nullable final Set<TechnologyType> technologyTypes) {
		final Set<UUID> excludedLinkChildren = new HashSet<>();
		if (hideInactiveBlocks) {
			excludedLinkChildren.addAll(functionalBlockService.findUids(q ->
					q.ofProject(networkBlock.getProject())
							.byUids(networkBlock.getChildren())
							.withStatus(FunctionalBlockStatus.INACTIVE)));
		}
		final List<FunctionalBlockLink> links = functionalBlockService.getLinks(q -> {
			q.ofParent(networkBlock.getUid());
			if (linkType != null) {
				q.withFlag(FunctionalBlockLinkFlag.TYPE, linkType.toString());
			}
			if ( ! excludedLinkChildren.isEmpty()) {
				q.notWithChildAs(excludedLinkChildren);
				q.notWithChildBs(excludedLinkChildren);
			}
			if (technologyTypes != null && ! technologyTypes.isEmpty()) {
				q.withSharedResourceTechnologyTypes(technologyTypes.parallelStream().map(tt -> Pair.of(tt.getTechnology(), tt.getType()))
						.collect(Collectors.toSet()));
			}
		});
		return links.parallelStream().map(link -> new ModuleRelationshipPojo(
				UUID.randomUUID(), link.getChildA(), null, link.getChildB(), null, RelationshipType.REFERENCES, RelationshipDirection.OUT,
				link.getFlags(), null, null, List.of(), null, null, null)).toList();
	}

	private Map<UUID, ModulePojo> getNetworkModules(final FunctionalBlockPojo networkBlock, final @Nullable Map<String, Object> filterObject) {
		final AtomicLong i = new AtomicLong(0);
		return functionalBlockService.find(q -> {
					q.ofProject(networkBlock.getProject()).byUids(networkBlock.getChildren()).withType(FunctionalBlockType.RA_TOP_DOWN);
					if (filterObject != null && ! filterObject.isEmpty()) {
						filterObjectService.applyFilterObject(networkBlock.getProject().getNid(), FunctionalBlocksGraphQlController.QUERY_NAME,
								filterObjectCoercionService.coerceFilterObject(FunctionalBlocksGraphQlController.QUERY_NAME, filterObject), q);
					}
				}).parallelStream().map(fb -> {
					final var table = functionalBlockService.getAggregations(q -> q.withPeer(p -> p.byUid(fb.getUid()))
							.ofProject(FilterOperators.OPERATOR_EQ, fb.getProject().getNid())
							.withType(FilterOperators.OPERATOR_EQ, FunctionalBlockType.FUNCTIONAL_GROUP)
							.aggregate(FunctionalBlockFieldName.UID, AggregationOperator.COUNT));
					final var peerCount = table.map(data -> data.isEmpty() ? null : data.get(0))
							.map(row -> row.get(FunctionalBlockFieldName.UID.name().toLowerCase())).orElse(0);
					final Map<String, Object> flags = new HashMap<>(fb.getFlags());
					if (Integer.parseInt(peerCount.toString()) > 0) {
						flags.put(PEER_COUNT, peerCount);
					}
					if (FunctionalBlockUtil.hasType(fb, FunctionalBlockType.MERGE_PARENT)) {
						final List<UUID> deletedChildren = functionalBlockService.findUids(q -> q.ofProject(fb.getProject())
								.byUids(fb.getChildren()).withType(FunctionalBlockType.RA_TOP_DOWN)
								.withFlag(FunctionalBlockFlag.DELETED, true));
						if ( ! deletedChildren.isEmpty() && deletedChildren.size() < fb.getChildren().size()) {
							flags.put(DELETED_CHILDREN, true);
						}
					}
					return new ModulePojo(fb.getUid(), i.incrementAndGet(),
							fb.getCustomProperties(), fb.getProject(), null, null, fb.getName(), null, Technology.UNKNOWN, Type.UNKNOWN, Storage.UNDEFINED, Origin.CUSTOM, Creator.DISCOVERY, Identification.IDENTIFIED,
							flags, fb.getDescription(), null, null, "", null, null, false, null, null,
							null, null, 0, 0, 0, false, null, null, null, null, null);
				}).collect(Collectors.toMap(ModulePojo::getUid, Function.identity()));

	}

	private DependencyGraph getReachabilityBlockDependencyGraph(final FunctionalBlockPojo block, final List<FunctionalBlockLink> links,
			@Nullable final ReachabilityBlockGraphFilterRequest dependencyGraphFilterRequest) {
		final Map<UUID, GeneratedFrom> generatedFrom = getGeneratedFrom(links);
		final Map<UUID, ModulePojo> moduleMap = getModules(block, generatedFrom);
		final Collection<ModuleRelationshipPojo> references = getReachabilityBlockReferences(moduleMap, links);
		final Map<UUID, ModulePojo> rootModuleMap = getRootModuleMap(moduleMap, links);
		if (dependencyGraphFilterRequest == null) {
			return new DependencyGraph(new ArrayList<>(moduleMap.values()), new ArrayList<>(references),
					rootModuleMap.values().parallelStream().map(MiningPojo::getId).collect(Collectors.toSet()));
		}
		return getFilteredDependencyGraph(block, dependencyGraphFilterRequest, references, rootModuleMap, generatedFrom, moduleMap);
	}

	private Map<UUID, GeneratedFrom> getGeneratedFrom(final List<FunctionalBlockLink> links) {
		final Set<UUID> linkedChildren = new HashSet<>();
		for (final FunctionalBlockLink link : links) {
			linkedChildren.add(link.getChildA());
			linkedChildren.add(link.getChildB());
		}
		return functionalBlockService.getGeneratedFrom(linkedChildren);
	}

	private Map<UUID, ModulePojo> getModules(final FunctionalBlockPojo block, final Map<UUID, GeneratedFrom> generatedFromMap) {
		final var projectId = block.getProject();
		final List<ModulePojo> moduleV2s = moduleService.findModules(q -> q.ofProject(projectId).withLinkHashes(generatedFromMap.values().parallelStream()
				.map(GeneratedFrom::getModuleLinkHash)
				.filter(Optional::isPresent)
				.map(Optional::get)
				.collect(Collectors.toList())));

		final Map<UUID, FunctionalBlockPojo> moduleBlockMap = functionalBlockService.get(generatedFromMap.keySet()).stream()
				.collect(Collectors.toMap(FunctionalBlockPojo::getUid, Function.identity()));

		final Map<String, ModulePojo> modulesByLinkHash = moduleV2s.parallelStream()
				.collect(Collectors.toMap(ModulePojo::getLinkHash, Function.identity()));

		final Map<UUID, ModulePojo> moduleMap = new HashMap<>(generatedFromMap.size());
		generatedFromMap.forEach((key, value) -> {
			final Optional<String> moduleLinkHash = value.getModuleLinkHash();
			if (moduleLinkHash.isPresent()) {
				final String linkHash = moduleLinkHash.get();
				final ModulePojo module = modulesByLinkHash.get(linkHash);
				final FunctionalBlockPojo functionalBlockPojo = moduleBlockMap.get(key);
				final Map<String, Object> info = new HashMap<>(functionalBlockPojo.getFlags());
				info.put(FUNCTIONAL_BLOCK_ID_INFO_PROPERTY, key.toString());
				if (module == null) {
					final ModulePojo modulePojo1 = new ModulePojo(
							UUID.randomUUID(),
							0L, // Some random value to avoid null checks, anyway this value is not validated or used anywhere
							null,
							projectId, projectId.getUid(), projectId.getNid(),
							functionalBlockPojo.getName(), null,
							Technology.UNKNOWN, Type.UNKNOWN, null, null, null,
							null,
							info,
							null,
							null,
							null,
							null,
							null,
							null,
							false,
							null,
							null,
							null,
							null,
							0,
							0,
							0,
							false,
							null,
							null, null,
							null,
							null);
					moduleMap.put(key, modulePojo1);
				} else {
					final var table = functionalBlockService.getAggregations(q -> q.ofProject(FilterOperators.OPERATOR_EQ, projectId.getNid())
							.withResolvedModulePart(module.identity())
							.withType(FilterOperators.OPERATOR_EQ, FunctionalBlockType.FUNCTIONAL_GROUP)
							.aggregate(FunctionalBlockFieldName.UID, AggregationOperator.COUNT).distinct());
					final var peerCount = table.map(data -> data.isEmpty() ? null : data.get(0))
							.map(row -> row.get(FunctionalBlockFieldName.UID.name().toLowerCase())).orElse(0);
					/* info is not present or unmodifiable */
					module.getInfo().ifPresent(info::putAll);
					if (Integer.parseInt(peerCount.toString()) > 0) {
						info.put(PEER_COUNT, peerCount);
					}
					final ModulePojo modulePojo = new ModulePojo(module.getUid(), module.getId(), module.getCustomProperties(),
							module.getProject(), null, null,
							module.getName(), module.getPath().orElse(null),
							module.getTechnology(), module.getType(), module.getStorage(), module.getOrigin(), module.getCreator(),
							module.getIdentification(),
							info,
							module.getDescription().orElse(null),
							module.getSource().orElse(null),
							module.getContentHash().orElse(null),
							module.getLinkHash(),
							module.getLocation().orElse(null),
							module.getRepresentation().orElse(null),
							module.isRequiresReview(),
							module.getModifiedDate().orElse(null),
							module.getMetricsDate().orElse(null),
							module.getSourceMetrics().orElse(null),
							module.getContent().orElse(null),
							module.getErrors(),
							module.getStatements(),
							module.getSqlStatements(),
							module.isSourceCodeAvailable(),
							module.getParent().orElse(null), null, null,
							module.getParentPath().orElse(null),
							module.getDependencyHash().orElse(null));
					moduleMap.put(key, modulePojo);
				}
			}
		});
		return moduleMap;
	}

	private Collection<ModuleRelationshipPojo> getReachabilityBlockReferences(final Map<UUID, ModulePojo> moduleMap, final List<FunctionalBlockLink> links) {
		if ( ! moduleMap.isEmpty()) {
			final Set<ModuleRelationshipPojo> moduleRelationshipPojoSet = new HashSet<>();
			final UUID dummy = new UUID(0, 0);
			for (final FunctionalBlockLink link : links) {
				final Object flag = link.getFlags().get(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_TYPE.name());
				@SuppressWarnings("unchecked")
				final Map<String, Object> properties =
						(Map<String, Object>) link.getFlags().get(FunctionalBlockLinkFlag.CALL_CHAIN_EDGE_RELATIONSHIP_PROPERTIES.name());
				final List<RelationshipType> relationshipTypes = new ArrayList<>();
				if (flag instanceof Collection) {
					relationshipTypes.addAll(((Collection<?>) flag).stream().map(Object::toString).map(RelationshipType::from).toList());
				}
				relationshipTypes.forEach(type -> moduleRelationshipPojoSet.add(new ModuleRelationshipPojo(dummy,
						moduleMap.get(link.getChildA()).getUid(),
						null,
						moduleMap.get(link.getChildB()).getUid(),
						null,
						type,
						RelationshipDirection.OUT,
						properties,
						null,
						null,
						Collections.emptyList(),
						null,
						null,
						null)));
			}
			return moduleRelationshipPojoSet;
		}
		return new ArrayList<>();
	}

	private Map<UUID, ModulePojo> getRootModuleMap(final Map<UUID, ModulePojo> moduleMap,
			final List<FunctionalBlockLink> links) {
		/* return all "Modules" that are never the childB of a link */
		final Set<UUID> childBs = links.parallelStream().map(FunctionalBlockLink::getChildB).collect(Collectors.toSet());

		return moduleMap.entrySet().parallelStream()
				.filter(entry -> ! childBs.contains(entry.getKey()) && entry.getValue() != null)
				.collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
	}

	private DependencyGraph getFilteredDependencyGraph(final FunctionalBlockPojo block, final ReachabilityBlockGraphFilterRequest dependencyGraphFilterRequest,
			final Collection<ModuleRelationshipPojo> references, final Map<UUID, ModulePojo> rootModulesMap,
			final Map<UUID, GeneratedFrom> generatedFromMap, final Map<UUID, ModulePojo> moduleMap) {
		final Collection<RelationshipType> filteredRelationshipTypes = dependencyGraphFilterRequest.getRelationshipTypes();
		final var incomingAndOutgoingReferences = getIncomingAndOutGoingRelationships(references);
		final Map<UUID, Set<ModuleRelationshipPojo>> incomingReferences = incomingAndOutgoingReferences.getLeft();
		final Map<UUID, Set<ModuleRelationshipPojo>> outgoingReferences = incomingAndOutgoingReferences.getRight();

		final Set<UUID> filteredOutModules = extractFilteredOutModules(block.getProject(),
				dependencyGraphFilterRequest, rootModulesMap, generatedFromMap, moduleMap, incomingReferences);

		final Map<UUID, ModulePojo> filteredModuleMap = new HashMap<>();
		moduleMap.forEach((key, value) -> {
			if ( ! filteredOutModules.contains(value.getUid())) {
				filteredModuleMap.put(key, value);
			}
		});
		createMissingModuleRelationships(filteredOutModules, incomingReferences, outgoingReferences, filteredRelationshipTypes);

		return new DependencyGraph(new ArrayList<>(filteredModuleMap.values()), outgoingReferences.values().parallelStream().flatMap(Set::parallelStream)
				.filter(ref -> filteredRelationshipTypes == null || ref.getRelationship() == RelationshipType.ARTIFICIAL || filteredRelationshipTypes.isEmpty() ||
						filteredRelationshipTypes.contains(ref.getRelationship()))
				.collect(Collectors.toList()), rootModulesMap.values().parallelStream().map(ModulePojo::getId).collect(Collectors.toSet()));
	}

	private Pair<Map<UUID, Set<ModuleRelationshipPojo>>, Map<UUID, Set<ModuleRelationshipPojo>>> getIncomingAndOutGoingRelationships(
			final Collection<ModuleRelationshipPojo> references) {
		final Map<UUID, Set<ModuleRelationshipPojo>> outgoingReferences = new HashMap<>();
		final Map<UUID, Set<ModuleRelationshipPojo>> incomingReferences = new HashMap<>();

		references.forEach(ref -> {
			outgoingReferences.computeIfAbsent(ref.getSrcModule(), k -> new HashSet<>()).add(ref);
			incomingReferences.computeIfAbsent(ref.getDstModule(), k -> new HashSet<>()).add(ref);
		});
		return Pair.of(incomingReferences, outgoingReferences);
	}

	private Set<UUID> extractFilteredOutModules(final EntityId projectId, final ReachabilityBlockGraphFilterRequest dependencyGraphFilterRequest,
			final Map<UUID, ModulePojo> rootModulesMap, final Map<UUID, GeneratedFrom> generatedFromMap,
			final Map<UUID, ModulePojo> moduleMap, final Map<UUID, Set<ModuleRelationshipPojo>> incomingReferences) {

		final Set<UUID> filteredOutModules = new HashSet<>();
		final Set<String> linkHashes = generatedFromMap.values().parallelStream()
				.map(GeneratedFrom::getModuleLinkHash)
				.filter(Optional::isPresent)
				.map(Optional::get)
				.collect(Collectors.toSet());

		filteredOutModules.addAll(filterModulesByTechnologyTypes(projectId, dependencyGraphFilterRequest, linkHashes));
		filteredOutModules.addAll(filterModulesByTaxonomyIds(projectId, dependencyGraphFilterRequest, linkHashes));
		filteredOutModules.addAll(filterModulesByRelationshipTypes(dependencyGraphFilterRequest, rootModulesMap, incomingReferences));
		filteredOutModules.addAll(filterModulesByFunctionalBlockIds(dependencyGraphFilterRequest, rootModulesMap, moduleMap));

		filteredOutModules.removeAll(rootModulesMap.values().parallelStream().map(ModulePojo::getUid).collect(Collectors.toSet()));
		return filteredOutModules;
	}

	private List<UUID> filterModulesByTechnologyTypes(final EntityId projectId, final ReachabilityBlockGraphFilterRequest dependencyGraphFilterRequest,
			final Set<String> linkHashes) {
		if (dependencyGraphFilterRequest.getTechnologyTypes() != null && ! dependencyGraphFilterRequest.getTechnologyTypes().isEmpty()) {
			return moduleService.findModuleUids(q -> q.ofProject(projectId).withLinkHashes(linkHashes)
					.notWithTechnologiesAndTypes(dependencyGraphFilterRequest.getTechnologyTypes().parallelStream()
							.map(technologyType -> new Tuple2<>(technologyType.getTechnology(), technologyType.getType())).collect(Collectors.toSet())));
		}
		return Collections.emptyList();
	}

	private List<UUID> filterModulesByTaxonomyIds(final EntityId projectId, final ReachabilityBlockGraphFilterRequest dependencyGraphFilterRequest,
			final Set<String> linkHashes) {
		if (dependencyGraphFilterRequest.getTaxonomyIds() != null && ! dependencyGraphFilterRequest.getTaxonomyIds().isEmpty()) {
			return moduleService.findModuleUids(q -> q.ofProject(projectId)
					.withLinkHashes(linkHashes)
					.notWithTaxonomies(dependencyGraphFilterRequest.getTaxonomyIds()));
		}
		return Collections.emptyList();
	}

	private List<UUID> filterModulesByRelationshipTypes(final ReachabilityBlockGraphFilterRequest dependencyGraphFilterRequest,
			final Map<UUID, ModulePojo> rootModulesMap, final Map<UUID, Set<ModuleRelationshipPojo>> incomingReferences) {
		final List<UUID> filteredOutModules = new ArrayList<>();
		if (dependencyGraphFilterRequest.getRelationshipTypes() != null && ! dependencyGraphFilterRequest.getRelationshipTypes().isEmpty()) {
			incomingReferences.forEach((key, value) -> {
				final long incomingCount = value.stream().filter(ref -> dependencyGraphFilterRequest.getRelationshipTypes().contains(ref.getRelationship()))
						.count();
				if (incomingCount == 0 && rootModulesMap.values().stream().noneMatch(modulePojo -> modulePojo.getUid().equals(key))) {
					filteredOutModules.add(key);
				}
			});
		}
		return filteredOutModules;
	}

	private List<UUID> filterModulesByFunctionalBlockIds(final ReachabilityBlockGraphFilterRequest dependencyGraphFilterRequest,
			final Map<UUID, ModulePojo> rootModulesMap, final Map<UUID, ModulePojo> moduleMap) {
		final List<UUID> filteredOutModules = new ArrayList<>();
		final Set<EntityId> functionalBlockIds = dependencyGraphFilterRequest.getFunctionalBlockIds();
		if (functionalBlockIds != null && ! functionalBlockIds.isEmpty()) {
			final Map<UUID, List<ResolvedModulePart>> resolvedModuleParts = functionalBlockService.getResolvedModuleParts(functionalBlockIds
					.stream().map(EntityId::getUid).collect(Collectors.toSet()));
			final Set<UUID> modules = resolvedModuleParts.values().stream()
					.flatMap(List::stream)
					.map(resolvedModulePart -> resolvedModulePart.getModuleId().getUid())
					.collect(Collectors.toSet());
			moduleMap.forEach((key, value) -> {
				if ( ! modules.contains(value.getUid()) && ! rootModulesMap.containsKey(key)) {
					filteredOutModules.add(value.getUid());
				}
			});
		}
		return filteredOutModules;
	}

	private List<ModuleRelationshipPojo> mergeBiDirectionalReferences(final List<ModuleRelationshipPojo> references) {
		final Map<Tuple2<UUID, UUID>, ModuleRelationshipPojo> uniDirectionalReferences = new HashMap<>();
		final List<ModuleRelationshipPojo> mergedReferences = new ArrayList<>();
		references.forEach(ref -> {
			final Tuple2<UUID, UUID> key = Tuple2.of(ref.getSrcModule(), ref.getDstModule());
			final Tuple2<UUID, UUID> reverseKey = Tuple2.of(ref.getDstModule(), ref.getSrcModule());
			if (uniDirectionalReferences.containsKey(reverseKey)) {
				final ModuleRelationshipPojo reverseRef = uniDirectionalReferences.get(reverseKey);
				final Map<String, Object> mergedProperties = new HashMap<>();
				final Set<String> mergedTypes = new HashSet<>();
				final Set<UUID> mergedSharedResources = new HashSet<>();
				ref.getProperties().ifPresent(prop -> {
					mergedProperties.putAll(prop);
					Optional.ofNullable(prop.get(FunctionalBlockLinkFlag.TYPE.name())).map(Collection.class::cast).ifPresent(mergedTypes::addAll);
					Optional.ofNullable(prop.get(FunctionalBlockLinkFlag.RA_SHARED_RESOURCE_ID.name())).map(Collection.class::cast).ifPresent(mergedSharedResources::addAll);
				});
				reverseRef.getProperties().ifPresent(prop -> {
					mergedProperties.putAll(prop);
					Optional.ofNullable(prop.get(FunctionalBlockLinkFlag.TYPE.name())).map(Collection.class::cast).ifPresent(mergedTypes::addAll);
					Optional.ofNullable(prop.get(FunctionalBlockLinkFlag.RA_SHARED_RESOURCE_ID.name())).map(Collection.class::cast).ifPresent(mergedSharedResources::addAll);
				});
				mergedProperties.put(FunctionalBlockLinkFlag.TYPE.name(), mergedTypes);
				mergedProperties.put(FunctionalBlockLinkFlag.RA_SHARED_RESOURCE_ID.name(), mergedSharedResources);
				final ModuleRelationshipPojo mergedRef = new ModuleRelationshipPojo(UUID.randomUUID(),
						ref.getSrcModule(), null, ref.getDstModule(), null, ref.getRelationship(),
						RelationshipDirection.BOTH, mergedProperties, null, null, List.of(),
						null, null, null);
				mergedReferences.add(mergedRef);
				uniDirectionalReferences.remove(reverseKey);
			} else {
				uniDirectionalReferences.put(key, ref);
			}
		});
		mergedReferences.addAll(uniDirectionalReferences.values());
		return mergedReferences;
	}

	private List<ModuleRelationshipPojo> getFilteredNetworkGraphReferences(final Set<UUID> modules,
			final List<ModuleRelationshipPojo> links, final Map<String, Object> filterObject) {
		if (filterObject != null && ! filterObject.isEmpty()) {
			final var incomingAndOutgoingReferences = getIncomingAndOutGoingRelationships(links);
			final Map<UUID, Set<ModuleRelationshipPojo>> incomingReferences = incomingAndOutgoingReferences.getLeft();
			final Map<UUID, Set<ModuleRelationshipPojo>> outgoingReferences = incomingAndOutgoingReferences.getRight();
			createMissingModuleRelationships(modules, incomingReferences, outgoingReferences, null);
			return outgoingReferences.values().parallelStream().flatMap(Set::parallelStream).collect(Collectors.toList());
		} else {
			return links;
		}
	}

	private void createMissingModuleRelationships(final Set<UUID> filteredOutModules, final Map<UUID, Set<ModuleRelationshipPojo>> incomingReferences,
			final Map<UUID, Set<ModuleRelationshipPojo>> outgoingReferences, @Nullable final Collection<RelationshipType> filteredRelationshipTypes) {
		final Set<Tuple2<UUID, UUID>> createdReferences = new HashSet<>();
		filteredOutModules.forEach(module -> {
			final Set<ModuleRelationshipPojo> outgoing = outgoingReferences.get(module);
			final Set<ModuleRelationshipPojo> incoming = incomingReferences.get(module);
			if (outgoing != null && incoming != null) {
				for (final var out : Set.copyOf(outgoing)) {
					for (final var in : Set.copyOf(incoming)) {
						if (!createdReferences.contains(Tuple2.of(in.getSrcModule(), out.getDstModule()))) {
							final RelationshipType relationshipType = (filteredRelationshipTypes == null || filteredRelationshipTypes.isEmpty()
									|| filteredRelationshipTypes.contains(out.getRelationship())) ? RelationshipType.ARTIFICIAL : out.getRelationship();
							final ModuleRelationshipPojo ref = new ModuleRelationshipPojo(UUID.randomUUID(), in.getSrcModule(), null, out.getDstModule(), null,
									relationshipType, RelationshipDirection.OUT, null, null, null, Collections.emptyList(), null, null, null);
							createdReferences.add(Tuple2.of(in.getSrcModule(), out.getDstModule()));
							outgoingReferences.computeIfPresent(in.getSrcModule(), (key, value) -> {
								value.add(ref);
								value.remove(in);
								return value;
							});

							incomingReferences.computeIfPresent(out.getDstModule(), (key, value) -> {
								value.add(ref);
								value.remove(out);
								return value;
							});
						}
					}
				}
			}
			incomingReferences.remove(module);
			outgoingReferences.remove(module);
		});
		outgoingReferences.forEach((key, value) -> value.removeIf(ref -> filteredOutModules.contains(ref.getDstModule())));
		incomingReferences.forEach((key, value) -> value.removeIf(ref -> filteredOutModules.contains(ref.getSrcModule())));
	}
}
