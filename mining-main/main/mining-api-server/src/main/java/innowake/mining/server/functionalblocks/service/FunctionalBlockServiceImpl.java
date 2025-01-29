/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.functionalblocks.service;

import innowake.lib.core.lang.Assert;
import innowake.mining.data.access.postgres.FunctionalBlockPgDao;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.Table;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockFieldName;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLink;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkCondition;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockLinkFlag;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockStatus;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.entities.functionalblocks.GeneratedFrom;
import innowake.mining.shared.entities.functionalblocks.ModulePart;
import innowake.mining.shared.entities.functionalblocks.ReachabilityDataPojo;
import innowake.mining.shared.entities.functionalblocks.ReachabilityDataPojoPrototype;
import innowake.mining.shared.entities.functionalblocks.ResolvedModulePart;
import innowake.mining.shared.lang.BuildingConsumer;
import innowake.mining.shared.model.aggregations.AggregationRequest;
import innowake.mining.shared.model.aggregations.AggregationResult;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
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
import java.util.stream.Collectors;

@Service
public class FunctionalBlockServiceImpl implements FunctionalBlockService {

	private final FunctionalBlockPgDao functionalBlockDao;

	@Autowired
	public FunctionalBlockServiceImpl(@Qualifier("postgres") final JdbcTemplate jdbcTemplate) {
		functionalBlockDao = new FunctionalBlockPgDao(jdbcTemplate);
	}

	public FunctionalBlockServiceImpl(final FunctionalBlockPgDao functionalBlockDao) {
		this.functionalBlockDao = functionalBlockDao;
	}

	@Override
	public Optional<FunctionalBlockPojo> find(final UUID uid) {
		return functionalBlockDao.find(Assert.assertNotNull(uid));
	}

	@Override
	public List<FunctionalBlockPojo> find(final BuildingConsumer<FunctionalBlockInquiryBuilder> builder) {
		return functionalBlockDao.find(builder);
	}

	@Override
	public Paged<FunctionalBlockPojo> find(final Pagination paging, final BuildingConsumer<FunctionalBlockInquiryBuilder> builder) {
		return functionalBlockDao.find(paging, builder);
	}
	
	@Override
	public List<UUID> findUids(final BuildingConsumer<FunctionalBlockInquiryBuilder> builder) {
		return functionalBlockDao.findUids(builder);
	}

	@Override
	public List<FunctionalBlockPojo> findChildrenDeep(final UUID parent, final int maxDepth, final BuildingConsumer<FunctionalBlockInquiryBuilder> builder) {
		return functionalBlockDao.findChildrenDeep(List.of(parent), maxDepth, builder);
	}

	@Override
	public Map<UUID, List<FunctionalBlockPojo>> findChildrenDeep(final List<UUID> parents, final int maxDepth,
			final BuildingConsumer<FunctionalBlockInquiryBuilder> builder) {
		final List<FunctionalBlockPojo> childrenDeep = functionalBlockDao.findChildrenDeep(parents, maxDepth, builder);
		final Map<UUID, List<FunctionalBlockPojo>> ret = new HashMap<>(parents.size());
		for (final FunctionalBlockPojo child : childrenDeep) {
			final UUID parent = (UUID) child.getFlags().get(FunctionalBlockPgDao.ROOT_BLOCK_FLAG);
			if ( parent != null ) {
				ret.computeIfAbsent(parent, k -> new ArrayList<>()).add(child);
			}
		}
		return ret;
	}

	@Override
	public Paged<FunctionalBlockPojo> findChildrenDeep(final UUID parent, final Pagination paging, final int maxDepth,
			final BuildingConsumer<FunctionalBlockInquiryBuilder> builder) {
		return functionalBlockDao.findChildrenDeep(parent, paging, maxDepth, builder);
	}

	@Override
	public Map<UUID, List<UUID>> findChildrenIdsDeep(final List<UUID> parents, final int maxDepth) {
		return functionalBlockDao.findChildrenIdsDeep(parents, maxDepth);
	}

	@Override
	public Paged<FunctionalBlockPojo> findByName(final Pagination pagination, final Long projectId, final List<Long> moduleIds, final List<Long> taxonomyIds,
			final List<UUID> peers, final List<String> searchNames, final List<UUID> ddIds) {
		return functionalBlockDao.findByName(pagination, projectId, moduleIds, taxonomyIds, peers, searchNames, ddIds);
	}

	@Override
	public Set<String> findFunctionalBlockNamesByAnnotationId(final Long annotationId) {
		return functionalBlockDao.findFunctionalBlockNamesByAnnotationId(annotationId);
	}

	@Override
	public Paged<ReachabilityDataPojo> findReachabilityData(final Pagination paging, final BuildingConsumer<ReachabilityDataInquiryBuilder> builder) {
		return functionalBlockDao.findReachabilityData(paging, builder);
	}

	@Override
	public List<ReachabilityDataPojo> findReachabilityData(final BuildingConsumer<ReachabilityDataInquiryBuilder> builder) {
		return functionalBlockDao.findReachabilityData(builder);
	}

	@Override
	public List<FunctionalBlockPojo> get(final Collection<UUID> uuids) {
		return Assert.assertNotNull(uuids).isEmpty() ? Collections.emptyList() : functionalBlockDao.find(uuids);
	}

	@Override
	public List<AggregationResult<FunctionalBlockFieldName>> getAggregations(final AggregationRequest<FunctionalBlockFieldName> request) {
		return functionalBlockDao.getAggregations(q -> prepareAggregation(request, q))
				.map(table -> AggregationResult.getResultFromTable(table, request, FunctionalBlockFieldName.class)).orElse(Collections.emptyList());
	}

	@Override
	public List<AggregationResult<FunctionalBlockFieldName>> getAggregations(final BuildingConsumer<FunctionalBlockInquiryBuilder> builder,
			final AggregationRequest<FunctionalBlockFieldName> request) {
		return functionalBlockDao.getAggregations(q -> {
			builder.accept((FunctionalBlockInquiryBuilder) q);
			prepareAggregation(request, q);
		}).map(table -> AggregationResult.getResultFromTable(table, request, FunctionalBlockFieldName.class)).orElse(Collections.emptyList());
	}

	@Override
	public Optional<Table> getAggregations(final BuildingConsumer<FunctionalBlockAggregationInquiryBuilder<?>> builder) {
		return functionalBlockDao.getAggregations(builder);
	}

	private void prepareAggregation(final AggregationRequest<FunctionalBlockFieldName> request, final FunctionalBlockAggregationInquiryBuilder<?> q) {
		request.getFilterObject().forEach((fieldName, filter) -> filter.forEach((operator, value) -> {
			switch (fieldName) {
				case UID:
					q.byUid(operator, value);
					break;
				case TYPE:
					q.withType(operator, value);
					break;
				case PROJECT_ID:
					q.ofProject(operator, value);
					break;
				case REFERENCED_MODULE_TYPE:
					q.withModuleType(operator, value);
					break;
				case REFERENCED_MODULE_TECHNOLOGY:
					q.withModuleTechnology(operator, value);
					break;
				default:
					throw new UnsupportedOperationException("Unknown field name for Annotation " + fieldName);
			}
		}));
		request.getFields().forEach(q :: aggregate);
		request.getGroupBy().forEach(q :: groupBy);
		request.getOrderBy().forEach(q :: orderBy);
	}

	@Override
	@Transactional("postgres")
	public UUID create(final FunctionalBlockPojoPrototype functionalBlock) {
		validateFunctionalBlock(functionalBlock);
		return functionalBlockDao.createFunctionalBlock(functionalBlock);
	}

	@Override
	@Transactional("postgres")
	public void update(final FunctionalBlockPojoPrototype functionalBlock) {
		functionalBlock.uid.getNonNull();
		if ( functionalBlock.children.isDefined() ) {
			validateChildrenWhenLinksUpdate(functionalBlock);
		}
		functionalBlockDao.updateFunctionalBlock(functionalBlock);
	}

	@Override
	@Transactional("postgres")
	public void updateBlocksStatus(final Collection<UUID> uids, final FunctionalBlockStatus status) {
		functionalBlockDao.updateBlocksStatus(uids, status);
	}

	@Override
	@Transactional("postgres")
	public void delete(final UUID uid) {
		functionalBlockDao.deleteFunctionalBlock(uid);
	}

	@Override
	@Transactional("postgres")
	public void delete(final Collection<UUID> uids) {
		functionalBlockDao.deleteFunctionalBlocks(uids);
	}

	@Override
	public void deleteAllOfProject(final EntityId projectId) {
		functionalBlockDao.deleteAllOfProject(projectId);
	}

	@Override
	@Transactional("postgres")
	public void deleteGeneratedFromAnnotation(final Long annotationId) {
		functionalBlockDao.deleteGeneratedFromAnnotation(annotationId);
	}

	@Override
	@Transactional("postgres")
	public void deleteGeneratedFromAnnotations(final Collection<EntityId> annotationIds) {
		functionalBlockDao.deleteGeneratedFromAnnotations(annotationIds);
	}

	@Override
	@Transactional("postgres")
	public void updateFunctionBlockDescriptionAndNameOnAnnotationUpdate(final Long annotationId, final AnnotationPojo updatedAnnotation) {
		functionalBlockDao.updateFunctionBlockDescriptionAndNameOnAnnotationUpdate(annotationId, updatedAnnotation);
	}

	@Override
	public Map<Long, UUID> findGeneratedFromAnnotations(final Collection<EntityId> annotationIds) {
		return functionalBlockDao.findGeneratedFromAnnotations(annotationIds);
	}

	@Override
	public Map<String, UUID> findGeneratedFromModules(final Collection<String> moduleLinkHashes, final EntityId project) {
		return functionalBlockDao.findGeneratedFromModules(moduleLinkHashes, project);
	}

	@Override
	public Map<UUID, List<ModulePart>> getModuleParts(final Collection<UUID> uids) {
		return functionalBlockDao.getModuleParts(uids);
	}

	@Override
	@Transactional("postgres")
	public void setResolvedModuleParts(final UUID uid, final Collection<ResolvedModulePart> resolvedModuleParts) {
		functionalBlockDao.setResolvedModuleParts(uid, resolvedModuleParts);
	}

	@Override
	public List<ResolvedModulePart> getResolvedModuleParts(final UUID uid) {
		return functionalBlockDao.getResolvedModuleParts(uid);
	}

	@Override
	public Map<UUID, List<ResolvedModulePart>> getResolvedModuleParts(final Collection<UUID> uids) {
		return functionalBlockDao.getResolvedModuleParts(uids);
	}

	@Override
	@Transactional("postgres")
	public void setGeneratedFrom(final UUID uid, final GeneratedFrom generatedFrom) {
		functionalBlockDao.setGeneratedFrom(uid, generatedFrom);
	}

	@Override
	public Optional<GeneratedFrom> getGeneratedFrom(final UUID uid) {
		return functionalBlockDao.getGeneratedFrom(uid);
	}

	@Override
	public Map<UUID, GeneratedFrom> getGeneratedFrom(final Collection<UUID> uids) {
		return functionalBlockDao.getGeneratedFrom(uids);
	}

	@Override
	@Transactional("postgres")
	public void setReachabilityData(final UUID functionalBlockId, final Collection<ReachabilityDataPojoPrototype> reachabilityDataList) {
		functionalBlockDao.setReachabilityData(functionalBlockId, reachabilityDataList);
	}

	@Override
	public List<FunctionalBlockLink> getLinks(final UUID blockUid) {
		return functionalBlockDao.getFunctionalBlockLinks(q -> q.ofParent(blockUid));
	}

	@Override
	public List<FunctionalBlockLink> getLinks(final BuildingConsumer<FunctionalBlockLinkInquiryBuilder> builder) {
		return functionalBlockDao.getFunctionalBlockLinks(builder);
	}

	@Override
	public Map<UUID, List<FunctionalBlockLink>> getLinks(final Collection<UUID> uids) {
		return functionalBlockDao.getFunctionalBlockLinkMap(uids);
	}

	@Override
	@Transactional("postgres")
	public void setLinks(final UUID blockUid, final List<FunctionalBlockLink> links) {
		final Optional<FunctionalBlockPojo> functionalBlockPojo = find(blockUid);
		if ( functionalBlockPojo.isPresent() ) {
			final List<UUID> blockChildren = functionalBlockPojo.get().getChildren();
			final var fixedLinks = validateAndFixLinks(blockUid, blockChildren, links);
			final Collection<String> generatedBy = getGeneratedByFromLinks(fixedLinks);
			final BuildingConsumer<FunctionalBlockLinkInquiryBuilder> builder;
			if ( ! generatedBy.isEmpty() ) {
				builder = q -> q.ofParent(blockUid).withGeneratedBy(generatedBy);
			} else {
				builder = q -> q.ofParent(blockUid);
			}
			functionalBlockDao.deleteLinks(builder);
			for (final var link : links) {
				final FunctionalBlockLinkCondition condition = link.getCondition();
				if ( condition != null ) {
					functionalBlockDao.createFunctionalBlockCondition(condition);
				}
			}
			functionalBlockDao.createFunctionalBlockLinks(blockUid, fixedLinks);
		}
	}

	@Override
	public void deleteLinks(final UUID blockUid) {
		functionalBlockDao.deleteLinks(q -> q.ofParent(blockUid));
	}

	@Override
	public void deleteConditionsAndStatementTypeChildren(final UUID blockUid) {
		functionalBlockDao.deleteChildFunctionalConditionsAndFunctionalAsts(blockUid);
	}

	private Collection<String> getGeneratedByFromLinks(final List<FunctionalBlockLink> links) {
		return links.stream().map(FunctionalBlockLink :: getFlags).filter(Objects :: nonNull)
				.map(flags -> flags.get(FunctionalBlockLinkFlag.GENERATED_BY.name())).filter(Objects :: nonNull).map(Object :: toString)
				.collect(Collectors.toList());
	}

	private List<FunctionalBlockLink> validateAndFixLinks(final UUID blockUid, final List<UUID> children, final List<FunctionalBlockLink> links) {

		if ( children.isEmpty() && ! links.isEmpty() ) {
			throw new IllegalArgumentException(
					"While updating links of functional block " + blockUid + ": Block has no children," + " so no links can be created");
		}

		final var fixedLinks = new ArrayList<>(links);
		for (FunctionalBlockLink link : links) {
			if ( ! children.contains(link.getChildA()) ) {
				final List<UUID> childAParents = functionalBlockDao.find(link.getChildA()).map(FunctionalBlockPojo :: getParents)
						.orElse(Collections.emptyList());
				final Collection<UUID> possibleParents = CollectionUtils.intersection(children, childAParents);
				if ( possibleParents.size() > 1 ) {
					throw new IllegalArgumentException(
							"While updating links of functional block " + blockUid + ": Unable to create link " + link.getChildA() + " -> " + link.getChildB()
									+ ": " + link.getChildA() + " is not a child of the block and contains " + "multiple parents: " + possibleParents);
				} else if ( possibleParents.size() == 1 ) {
					fixedLinks.remove(link);
					final Map<String, Object> flags = new HashMap<>(link.getFlags());
					flags.put(FunctionalBlockLinkFlag.MERGE_CHILD_A.name(), link.getChildA());
					final var fixedLink = new FunctionalBlockLink(link.getUid(), blockUid, possibleParents.iterator().next(), link.getChildB(),
							link.getConditionLabel(), flags, link.getCondition());
					link = fixedLink; // this is required to check childB
					fixedLinks.add(fixedLink);
				} else {
					throw new IllegalArgumentException(
							"While updating links of functional block " + blockUid + ": Unable to create link " + link.getChildA() + " -> " + link.getChildB()
									+ ": " + link.getChildA() + " is not a child of the block");
				}
			}
			if ( ! children.contains(link.getChildB()) ) {
				final List<UUID> childBParents = functionalBlockDao.find(link.getChildB()).map(FunctionalBlockPojo :: getParents)
						.orElse(Collections.emptyList());
				final Collection<UUID> possibleParents = CollectionUtils.intersection(children, childBParents);
				if ( possibleParents.size() > 1 ) {
					throw new IllegalArgumentException(
							"While updating links of functional block " + blockUid + ": Unable to create link " + link.getChildA() + " -> " + link.getChildB()
									+ ": " + link.getChildB() + " is not a child of the block and contains " + "multiple parents: " + possibleParents);
				} else if ( possibleParents.size() == 1 ) {
					fixedLinks.remove(link);
					final Map<String, Object> flags = new HashMap<>(link.getFlags());
					flags.put(FunctionalBlockLinkFlag.MERGE_CHILD_B.name(), link.getChildB());
					fixedLinks.add(
							new FunctionalBlockLink(link.getUid(), blockUid, link.getChildA(), possibleParents.iterator().next(), link.getConditionLabel(),
									flags, link.getCondition()));
				} else {
					throw new IllegalArgumentException(
							"While updating links of functional block " + blockUid + ": Unable to create link " + link.getChildA() + " -> " + link.getChildB()
									+ ": " + link.getChildB() + " is not a child of the block");
				}
			}
		}
		return fixedLinks;
	}

	@Override
	@Transactional("postgres")
	public void merge(final UUID commonParent, final UUID mergeParent, final Collection<UUID> mergeChildren) {
		final List<FunctionalBlockPojo> functionalBlockPojos = functionalBlockDao.find(List.of(commonParent, mergeParent));
		FunctionalBlockPojo commonParentPojo = null;
		FunctionalBlockPojo mergeParentPojo = null;
		for (final FunctionalBlockPojo functionalBlockPojo : functionalBlockPojos) {
			if ( functionalBlockPojo.getUid().equals(mergeParent) ) {
				mergeParentPojo = functionalBlockPojo;
			} else if ( functionalBlockPojo.getUid().equals(commonParent) ) {
				commonParentPojo = functionalBlockPojo;
			}
		}

		if ( mergeParentPojo != null && commonParentPojo != null ) {
			final var commonParentChildren = new HashSet<>(commonParentPojo.getChildren());
			final var mergeParentChildren = new HashSet<>(mergeParentPojo.getChildren());
			if ( commonParentChildren.contains(mergeParent) && commonParentChildren.containsAll(mergeChildren) && ! CollectionUtils.containsAny(mergeChildren,
					mergeParentPojo.getChildren()) ) {
				final var commonParentPrototype = new FunctionalBlockPojoPrototype().setUid(commonParent);
				final var mergeParentPrototype = new FunctionalBlockPojoPrototype().setUid(mergeParent);
				final List<FunctionalBlockLink> mergeParentPojoLinks = getLinks(mergeParentPojo.getUid());
				final List<FunctionalBlockLink> commonParentPojoLinks = getLinks(commonParentPojo.getUid());
				var mergeParentLinks = new HashSet<>(mergeParentPojoLinks);
				var commonParentLinks = new HashSet<>(commonParentPojoLinks);

				for (final UUID mergeChild : mergeChildren) {
					/* Updating the children and parents */
					commonParentChildren.remove(mergeChild);
					mergeParentChildren.add(mergeChild);
					/* Updating the links */
					final var modifiableMergeParentLinks = new HashSet<>(mergeParentLinks);
					final var modifiableCommonParentLinks = new HashSet<>(commonParentLinks);
					commonParentLinks.forEach(link -> {
						final Map<String, Object> linkFlags = new HashMap<>(link.getFlags());
						if ( link.getChildA().equals(mergeParent) && link.getChildB().equals(mergeChild) ) {
							final UUID previousChild = UUID.fromString(Objects.requireNonNull(linkFlags.remove(FunctionalBlockLinkFlag.MERGE_CHILD_A.name()),
									"Merge child A flag not found on link " + link).toString());
							modifiableCommonParentLinks.remove(link);
							modifiableMergeParentLinks.add(
									new FunctionalBlockLink(link.getUid(), mergeParent, previousChild, mergeChild, link.getConditionLabel(), linkFlags,
											link.getCondition()));
						} else if ( link.getChildA().equals(mergeChild) && link.getChildB().equals(mergeParent) ) {
							final UUID previousChild = UUID.fromString(Objects.requireNonNull(linkFlags.remove(FunctionalBlockLinkFlag.MERGE_CHILD_B.name()),
									"Merge child B flag not found on link " + link).toString());
							modifiableCommonParentLinks.remove(link);
							modifiableMergeParentLinks.add(
									new FunctionalBlockLink(link.getUid(), mergeParent, mergeChild, previousChild, link.getConditionLabel(), linkFlags,
											link.getCondition()));
						} else if ( link.getChildA().equals(mergeChild) ) {
							linkFlags.put(FunctionalBlockLinkFlag.MERGE_CHILD_A.name(), mergeChild);
							modifiableCommonParentLinks.remove(link);
							modifiableCommonParentLinks.add(
									new FunctionalBlockLink(link.getUid(), commonParent, mergeParent, link.getChildB(), link.getConditionLabel(), linkFlags,
											link.getCondition()));
						} else if ( link.getChildB().equals(mergeChild) ) {
							linkFlags.put(FunctionalBlockLinkFlag.MERGE_CHILD_B.name(), mergeChild);
							modifiableCommonParentLinks.remove(link);
							modifiableCommonParentLinks.add(
									new FunctionalBlockLink(link.getUid(), commonParent, link.getChildA(), mergeParent, link.getConditionLabel(), linkFlags,
											link.getCondition()));
						}
					});

					mergeParentLinks = modifiableMergeParentLinks;
					commonParentLinks = modifiableCommonParentLinks;
				}

				/* temporarily remove all links, so that we can move the children without violating the constraint that linked blocks must be
				 * inside the same parent */
				setLinks(commonParent, Collections.emptyList());
				setLinks(mergeParent, Collections.emptyList());

				/* now update the children */
				functionalBlockDao.updateFunctionalBlock(commonParentPrototype.setChildren(new ArrayList<>(commonParentChildren)));
				functionalBlockDao.updateFunctionalBlock(mergeParentPrototype.setChildren(new ArrayList<>(mergeParentChildren)));

				/* and now set the correct (new) links */
				setLinks(commonParent, new ArrayList<>(commonParentLinks));
				setLinks(mergeParent, new ArrayList<>(mergeParentLinks));

				return;
			}
			throw new IllegalArgumentException(String.format("mergeParent [%s] and mergeChildren [%s] must be children of commonParent [%s] and mergeChildren"
					+ " must not be children of mergeParent before merging", mergeParent, mergeChildren, commonParentPojo));
		}
		throw new IllegalArgumentException(
				String.format("commonParent [%s] and mergeParent [%s] should be present in the database", commonParent, mergeParent));
	}

	@Override
	@Transactional("postgres")
	public void unmerge(final UUID commonParent, final UUID mergeParent, final Collection<UUID> mergeChildren) {
		final List<FunctionalBlockPojo> functionalBlockPojos = functionalBlockDao.find(List.of(commonParent, mergeParent));
		FunctionalBlockPojo commonParentPojo = null;
		FunctionalBlockPojo mergeParentPojo = null;
		for (final FunctionalBlockPojo functionalBlockPojo : functionalBlockPojos) {
			if ( functionalBlockPojo.getUid().equals(mergeParent) ) {
				mergeParentPojo = functionalBlockPojo;
			} else if ( functionalBlockPojo.getUid().equals(commonParent) ) {
				commonParentPojo = functionalBlockPojo;
			}
		}

		if ( mergeParentPojo != null && commonParentPojo != null ) {
			final var mergeParentChildren = new HashSet<>(mergeParentPojo.getChildren());
			final var commonParentChildren = new HashSet<>(commonParentPojo.getChildren());
			if ( commonParentPojo.getChildren().contains(mergeParent) && mergeParentChildren.containsAll(mergeChildren) ) {
				final var commonParentPrototype = new FunctionalBlockPojoPrototype().setUid(commonParent);
				final var mergeParentPrototype = new FunctionalBlockPojoPrototype().setUid(mergeParent);
				final List<FunctionalBlockLink> mergeParentPojoLink = getLinks(mergeParentPojo.getUid());
				final List<FunctionalBlockLink> commonParentPojoLink = getLinks(commonParentPojo.getUid());
				var mergeParentLinks = new HashSet<>(mergeParentPojoLink);
				var commonParentLinks = new HashSet<>(commonParentPojoLink);

				for (final var mergeChild : mergeChildren) {
					/* Updating the children and parents */
					mergeParentChildren.remove(mergeChild);
					commonParentChildren.add(mergeChild);

					/* Updating the links */
					final var modifiableMergeParentLinks = new HashSet<>(mergeParentLinks);
					final var modifiableCommonParentLinks = new HashSet<>(commonParentLinks);
					commonParentLinks.forEach(link -> {
						final Map<String, Object> linkFlags = new HashMap<>(link.getFlags());
						if ( link.getChildA().equals(mergeParent) && mergeChild.equals(UUID.fromString(
								Objects.requireNonNull(linkFlags.get(FunctionalBlockLinkFlag.MERGE_CHILD_A.name()),
										"Merge child A flag not found on link " + link).toString())) ) {
							linkFlags.remove(FunctionalBlockLinkFlag.MERGE_CHILD_A.name());
							modifiableCommonParentLinks.remove(link);
							modifiableCommonParentLinks.add(
									new FunctionalBlockLink(link.getUid(), commonParent, mergeChild, link.getChildB(), link.getConditionLabel(), linkFlags,
											link.getCondition()));
						} else if ( link.getChildB().equals(mergeParent) && mergeChild.equals(UUID.fromString(
								Objects.requireNonNull(linkFlags.get(FunctionalBlockLinkFlag.MERGE_CHILD_B.name()),
										"Merge child B flag not found on link " + link).toString())) ) {
							linkFlags.remove(FunctionalBlockLinkFlag.MERGE_CHILD_B.name());
							modifiableCommonParentLinks.remove(link);
							modifiableCommonParentLinks.add(
									new FunctionalBlockLink(link.getUid(), commonParent, link.getChildA(), mergeChild, link.getConditionLabel(), linkFlags,
											link.getCondition()));
						}
					});
					mergeParentLinks.forEach(link -> {
						final Map<String, Object> linkFlags = new HashMap<>(link.getFlags());
						if ( link.getChildA().equals(mergeChild) ) {
							linkFlags.put(FunctionalBlockLinkFlag.MERGE_CHILD_B.name(), link.getChildB());
							modifiableMergeParentLinks.remove(link);
							modifiableCommonParentLinks.add(
									new FunctionalBlockLink(link.getUid(), commonParent, mergeChild, mergeParent, link.getConditionLabel(), linkFlags,
											link.getCondition()));
						} else if ( link.getChildB().equals(mergeChild) ) {
							linkFlags.put(FunctionalBlockLinkFlag.MERGE_CHILD_A.name(), link.getChildA());
							modifiableMergeParentLinks.remove(link);
							modifiableCommonParentLinks.add(
									new FunctionalBlockLink(link.getUid(), commonParent, mergeParent, mergeChild, link.getConditionLabel(), linkFlags,
											link.getCondition()));
						}
					});

					mergeParentLinks = modifiableMergeParentLinks;
					commonParentLinks = modifiableCommonParentLinks;
				}

				/* temporarily remove all links, so that we can move the children without violating the constraint that linked blocks must be
				 * inside the same parent */
				setLinks(commonParent, Collections.emptyList());
				setLinks(mergeParent, Collections.emptyList());

				/* now update the children */
				functionalBlockDao.updateFunctionalBlock(mergeParentPrototype.setChildren(new ArrayList<>(mergeParentChildren)));
				functionalBlockDao.updateFunctionalBlock(commonParentPrototype.setChildren(new ArrayList<>(commonParentChildren)));

				/* and now set the correct (new) links */
				setLinks(commonParent, new ArrayList<>(commonParentLinks));
				setLinks(mergeParent, new ArrayList<>(mergeParentLinks));

				return;
			}
			throw new IllegalArgumentException(
					String.format("mergeChildren [%s] must be children of mergeParent [%s] and that should be child of commonParent [%s]", mergeChildren,
							mergeParentPojo, commonParentPojo));
		}
		throw new IllegalArgumentException(
				String.format("commonParent [%s] and mergeParent [%s] should be present in the database", commonParent, mergeParent));
	}

	@Override
	@Transactional("postgres")
	public void setChildrenDeep(final UUID parent, final List<UUID> children) {
		functionalBlockDao.setChildrenDeep(parent, children);
	}

	@Override
	public List<UUID> getChildrenDeep(final UUID parent) {
		return functionalBlockDao.getChildrenDeep(parent);
	}

	private void validateFunctionalBlock(final FunctionalBlockPojoPrototype functionalBlock) {
		if ( functionalBlock == null ) {
			throw new IllegalArgumentException("FunctionalBlock cannot be null");
		}

		if ( ! functionalBlock.project.isDefined() ) {
			throw new IllegalArgumentException("Project attribute of a functionalBlock cannot be null");
		}

		if ( ! functionalBlock.name.isDefined() ) {
			throw new IllegalArgumentException("Name attribute of a functionalBlock  cannot be null");
		}

		if ( ! functionalBlock.description.isDefined() ) {
			throw new IllegalArgumentException("Description attribute of a functionalBlock cannot be null");
		}
	}

	private void validateChildrenWhenLinksUpdate(final FunctionalBlockPojoPrototype functionalBlock) {
		final List<FunctionalBlockLink> functionalBlockLinks = getLinks(functionalBlock.uid.getNonNull());
		final List<UUID> linkChilds = new ArrayList<>();

		for (final FunctionalBlockLink link : functionalBlockLinks) {
			linkChilds.add(link.getChildA());
			linkChilds.add(link.getChildB());
		}
		linkChilds.removeAll(functionalBlock.children.getNonNull());
		if ( ! linkChilds.isEmpty() ) {
			throw new IllegalArgumentException("While updating functional block " + functionalBlock.uid.get() + ": Unable to update the list of children"
					+ " because links exist between the following children which are no longer part of the block: " + linkChilds);
		}
	}

	@Override
	@Transactional("postgres")
	public void setReferencedDataDictionaries(final UUID blockUid, final List<UUID> dataDictionaryIds) {
		functionalBlockDao.setReferencedDataDictionaries(blockUid, dataDictionaryIds);
	}

	@Override
	public List<UUID> getReferencedDataDictionaries(final UUID functionalBlockUid) {
		return functionalBlockDao.getReferencedDataDictionaries(functionalBlockUid);
	}

	@Override
	public Map<EntityId, UUID> getSingleParentFunctionalUnitsByAnnotationType(final EntityId projectId, final UUID blockUid, final String annotationType) {
		return functionalBlockDao.getSingleParentFunctionalUnitsByAnnotationType(projectId, blockUid, annotationType);
	}

	@Override
	public List<Pair<UUID, UUID>> findPeers(final BuildingConsumer<FunctionalBlockInquiryBuilder> blockFilter,
			final BuildingConsumer<FunctionalBlockInquiryBuilder> peerFilter) {
		return functionalBlockDao.findPeers(blockFilter, peerFilter);
	}

	@Override
	public List<Pair<UUID, UUID>> findPeers(final BuildingConsumer<FunctionalBlockInquiryBuilder> blockFilter, final FunctionalBlockType peerType) {
		return functionalBlockDao.findPeers(blockFilter, peerType);
	}

	@Override
	public void setComputedAt(final Collection<UUID> blockUids, final Instant computedAt) {
		functionalBlockDao.setComputedAt(blockUids, computedAt);
	}
}
