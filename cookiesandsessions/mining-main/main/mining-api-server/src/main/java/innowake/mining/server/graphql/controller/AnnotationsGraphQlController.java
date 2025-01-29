/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.graphql.controller;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.annotation.Order;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.BatchMapping;
import org.springframework.graphql.data.method.annotation.SchemaMapping;
import org.springframework.stereotype.Controller;

import graphql.execution.DataFetcherResult;
import graphql.schema.DataFetchingEnvironment;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.data.datapoints.FilterObjectService;
import innowake.mining.data.datapoints.MiningDataPointBuilder;
import innowake.mining.data.datapoints.MiningDataPointSource;
import innowake.mining.data.datapoints.SortObjectService;
import innowake.mining.data.datapoints.registry.DataPointRegistry;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.graphql.GraphQlUtil;
import innowake.mining.server.graphql.MiningGraphQLQueries;
import innowake.mining.server.graphql.MiningQueryMapping;
import innowake.mining.server.opensearch.OpenSearchService;
import innowake.mining.server.util.UserNameUtil;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.AnnotationService.AnnotationInquiryBuilder;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.FunctionalBlockService;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.annotations.Usage;
import innowake.mining.shared.datapoints.annotations.UsageAttribute;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition.ScalarType;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.SearchFilterAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.ViewModeAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.TableAttributes;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockPojo;
import innowake.mining.shared.entities.functionalblocks.FunctionalBlockType;
import innowake.mining.shared.service.UserRoleService;

/**
 * Defines data points for {@link AnnotationPojo}.
 *
 * Implementation had to be moved to a separate class to fix circular dependency
 * on {@link DataPointRegistry}.
 */
@Controller
@Order(3)
public class AnnotationsGraphQlController implements MiningDataPointSource {
	
	/**
	 * Object encapsulating sort and filter options when filtering by functional group name.
	 * See the sort and filter callbacks set up in {@link AnnotationsGraphQlController}.
	 */
	static class CustomFunctionalGroupSortFilter {

		@Nullable
		String filterValue;
		@Nullable
		SortDirection sortDirection;

		void filter(final String filterValue) {
			if ( ! filterValue.isEmpty() && ! filterValue.contains("*")) {
				this.filterValue = filterValue.concat("*");
			} else {
				this.filterValue = filterValue;
			}
		}

		void sort(final SortDirection sortDirection) {
			this.sortDirection = sortDirection;
		}
	}
	
	@Autowired
	private AnnotationService annotationService;
	@Autowired
	private ModuleService moduleService;
	@Autowired
	private FunctionalBlockService functionalBlockService;
	@Autowired
	private FilterObjectService filterObjectService;
	@Autowired
	private SortObjectService sortObjectService;
	@Autowired
	private DataDictionaryService dataDictionaryService;

	@Autowired
	private UserRoleService userRoleService;
	@Autowired
	private UserNameUtil userNameUtil;
	
	@Autowired
	private DataPointRegistry dpRegistry;

	/**
	 * Query for {@linkplain AnnotationPojo annotations} of a project.
	 * 
	 * @param projectId the ID of the project that contains the annotation
	 * @param page the page number
	 * @param size the number of elements
	 * @param sortObject sort conditions in object format
	 * @param filterObject filtering conditions in object format
	 * @param useOpenSearch flag to use {@link OpenSearchService}
	 * @return the list of annotations
	 */
	@MiningQueryMapping
	@Nature({MINING})
	@Role({VIEWER})
	public DataFetcherResult<Paged<AnnotationPojo>> annotations(@Argument final Long projectId,
			@Argument @Nullable final Integer page,
			@Argument @Nullable final Integer size,
			@Argument(name = "sortObject") @Nullable final List<Map<String, String>> sortObject,
			@Argument(name = "filterObject") @Nullable final Map<String, Object> filterObject,
			@Argument @Nullable final Boolean useOpenSearch) {
		
		final int intPage = Optional.ofNullable(page).orElse(0).intValue();
		final int intSize = Optional.ofNullable(size).orElse(0).intValue();
		
		final Paged<AnnotationPojo> result = annotationService.find(Pagination.at(intPage, intSize), q -> {
			q.ofProject(EntityId.of(projectId));
			if (filterObject != null) {
				filterObjectService.applyFilterObject(projectId, MiningGraphQLQueries.ANNOTATIONS, filterObject, q);
			}
			if (sortObject != null) {
				sortObjectService.applySortObject(projectId, MiningGraphQLQueries.ANNOTATIONS, sortObject, q);
			}
			// default sorting, apply last for least significance
			q.sortByModuleLocation(SortDirection.ASCENDING);
		});
		
		return DataFetcherResult.<Paged<AnnotationPojo>>newResult()
				.data(result)
				.localContext(new ControllerLocalContext(projectId, userRoleService.getProjectIds(), userRoleService.getClientAdminIds(),
						userRoleService.isAdmin()))
				.build();
	}
	
	@SchemaMapping(typeName = MiningEnitityNames.ANNOTATION)
	@MiningDataPoint(displayName = "Created By", description = "Name of the user who created the Annotation")
	@Usage(value = Usages.MINING_UI_ANNOTATIONS_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Modifications")
	})
	public String createdByUserName(final AnnotationPojo annotation) {
		return userNameUtil.getUserName(annotation.getCreatedByUserId());
	}
	
	@SchemaMapping(typeName = MiningEnitityNames.ANNOTATION)
	@MiningDataPoint(displayName = "Modified By", description = "Name of the user who last modified the Annotation")
	@Usage(value = Usages.MINING_UI_ANNOTATIONS_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Modifications"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "7")
	})
	@Nullable
	public String updatedByUserName(final AnnotationPojo annotation) {
		return annotation.getUpdatedByUserId().map(userNameUtil::getUserName).orElse("SYSTEM");
	}
	
	@SuppressWarnings("unused")
	@SchemaMapping(typeName = MiningEnitityNames.ANNOTATION)
	public List<DataDictionaryPojo> linkedDataDictionaries(final AnnotationPojo annotation, final DataFetchingEnvironment env,
			@Argument final boolean isBusiness) {
		return dataDictionaryService.find(q -> q.ofAnnotation(annotation.identity())
				.withIsBusiness(isBusiness));
	}

	@BatchMapping(typeName = MiningEnitityNames.ANNOTATION)
	public Map<AnnotationPojo, List<Map<String, Object>>> functionalGroups(final List<AnnotationPojo> annotations) {
 		if (annotations.isEmpty()) {
			return Collections.emptyMap();
		}
		final Map<Long, UUID> functionalUnits = functionalBlockService.findGeneratedFromAnnotations(annotations.stream()
				.map(annotation -> EntityId.of(Assert.assertNotNull(annotation.getId()))).collect(Collectors.toList()));

		final Map<Long, List<Map<String, Object>>> annotationToFunctionalGroups = new HashMap<>(annotations.size());
		for (final Map.Entry<Long, UUID> entry : functionalUnits.entrySet()) {
			final FunctionalBlockPojo functionalUnit = functionalBlockService.find(entry.getValue())
					.orElseThrow(() -> new MiningEntityNotFoundException(FunctionalBlockPojo.class, entry.getValue().toString()));

			final List<FunctionalBlockPojo> functionalGroups = functionalBlockService.find(q -> q
					.byUids(functionalUnit.getParents())
					.withType(FunctionalBlockType.FUNCTIONAL_GROUP));

			annotationToFunctionalGroups.put(entry.getKey(), functionalGroups.stream()
					.map(fg -> Map.<String, Object>of(
							"uid", fg.getUid(),
							"name", fg.getName(),
							"annotationSequenceNumber", fg.getChildren().indexOf(functionalUnit.getUid()) + 1
					))
					.collect(Collectors.toList()));
		}

		final Map<AnnotationPojo, List<Map<String, Object>>> ret = new HashMap<>(annotations.size());
		annotations.forEach(annotation -> ret.put(annotation, annotationToFunctionalGroups.getOrDefault(annotation.getId(), Collections.emptyList())));

		return ret;
	}
	
	@BatchMapping(typeName = MiningEnitityNames.ANNOTATION)
	public Map<AnnotationPojo, List<DataDictionaryPojo>> linkedDataDictionaryEntries(final List<AnnotationPojo> annotations) {
		if (annotations.isEmpty()) {
			return Collections.emptyMap();
		}
		final var idToAnnotation = annotations.stream().collect(Collectors.toMap(AnnotationPojo::identity, t -> t));
		final List<DataDictionaryPojo> functionalUnits = dataDictionaryService.find(q -> q.ofAnnotations(idToAnnotation.keySet()));
		final Map<AnnotationPojo, List<DataDictionaryPojo>> annotationToFunctionalGroups = new HashMap<>(annotations.size());
		annotations.forEach(a -> annotationToFunctionalGroups.put(a, null));

		for (final DataDictionaryPojo dataDictionaryPojo : functionalUnits) {
			dataDictionaryPojo.getAnnotations().stream()
				.map(idToAnnotation::get)
				.forEach(a -> annotationToFunctionalGroups.computeIfAbsent(a, x -> new LinkedList<>()).add(dataDictionaryPojo));
		}

		return annotationToFunctionalGroups;
	}
	
	@SchemaMapping(typeName = MiningEnitityNames.ANNOTATION)
	public ModulePojo module(final AnnotationPojo annotation, final DataFetchingEnvironment env) {
		return moduleService.findAnyModule(q -> {
				q.byId(annotation.getModule());
				GraphQlUtil.applyDynamicSelection(dpRegistry, annotation.getProjectNid(), MiningEnitityNames.MODULE, new String[] {}, env, q, Stream.empty());
			}).orElse(null);
	}
	
	@SchemaMapping(typeName = MiningEnitityNames.ANNOTATION)
	@MiningDataPoint(displayName = "Offset", description = "Module location offset")
	@Usage(value = Usages.MINING_UI_ANNOTATIONS_TABLE)
	@Usage(Usages.SORT_BY)
	@Nullable
	public Integer annotationOffset(final AnnotationPojo annotation) {
		return annotation.getOffset().orElse(0);
	}
	
	/**
	 * ANNOTATIONS.
	 */
	private static final String ANNOTATION_FUNCTIONAL_GROUP_TYPE_NAME = "AnnotationFunctionalGroup";
	private static final String ISBUSINESS = "isBusiness";

	@Override
	public void provideDataPoints(final MiningDataPointBuilder builder) {
		builder.defineType(MiningEnitityNames.ANNOTATION)
			.representedBy(AnnotationPojo.class)
			.withDefaultProperties()
			.add();

		builder.defineType(ANNOTATION_FUNCTIONAL_GROUP_TYPE_NAME)
				.add();

		builder.defineDataPointsFromSchemaMappingAnnotations(AnnotationsGraphQlController.class);

		builder.extend(MiningEnitityNames.MODULE, "id")
			.withFiltering(MiningGraphQLQueries.ANNOTATIONS, f -> {
				f.<AnnotationInquiryBuilder, Long>eq((q, v) -> q.ofModule(EntityId.of(v)));
				f.<AnnotationInquiryBuilder, Collection<Long>>in((q, v) -> q.ofModules(v.stream().map(EntityId::of).collect(Collectors.toList())));
			})
			.withSorting(MiningGraphQLQueries.ANNOTATIONS, AnnotationInquiryBuilder::sortNid)
			.add();
		builder.extend(MiningEnitityNames.MODULE, "type")
			.withFiltering(MiningGraphQLQueries.ANNOTATIONS, f -> {
				f.eq(AnnotationInquiryBuilder::withModuleType);
				f.in(AnnotationInquiryBuilder::withModuleTypes);
			})
			.add();
		builder.extend(MiningEnitityNames.MODULE, "technology")
			.withFiltering(MiningGraphQLQueries.ANNOTATIONS, f -> {
				f.eq(AnnotationInquiryBuilder::withModuleTechnology);
				f.in(AnnotationInquiryBuilder::withModuleTechnologies);
			})
			.add();

		builder.extend(MiningEnitityNames.MODULE, "name")
			.withSorting(MiningGraphQLQueries.ANNOTATIONS, AnnotationInquiryBuilder::sortByModuleName)
			.withFiltering(MiningGraphQLQueries.ANNOTATIONS, f ->
				f.<AnnotationInquiryBuilder, String>eq((q, v) -> q.withLikeModuleName(v, true))
			)
			.add();
		
//		"innowake_mining_shared_model_AnnotationMetadata"	"reason"	"sqlFragmentEq"	"metaData.reason.name CONTAINS ?"
//		"innowake_mining_shared_model_AnnotationMetadata"	"reason"	"sqlFragmentNone"	"metaData.reason.size() = 0"
//		"innowake_mining_shared_model_AnnotationMetadata"	"reason"	"sqlFragmentOrderBy"	"metaData.reason.name"
		builder.extend(MiningEnitityNames.ANNOTATION, "reasons")
			.withSorting(MiningGraphQLQueries.ANNOTATIONS, AnnotationInquiryBuilder::sortReasons)
			.withFiltering(MiningGraphQLQueries.ANNOTATIONS, f -> {
				//isAbsent will call withReasons(null), which should filter Annotations with empty or undefined reasons
				f.isAbsent(AnnotationInquiryBuilder::withReason);
				//contains has to be modeled as *someName*
				f.eq(AnnotationInquiryBuilder::withReason);
				//for multiselect filtering, call is made to withReasons which checks whether the input has atleast one of the reasons to be filtered.
				f.in(AnnotationInquiryBuilder::withReasons);
				})
			.add();
		//		"Annotation"	"name"	"sqlFragmentEq"	"SEARCH_INDEX(""Annotation_name_ft"", ?) = true"
		//		"Annotation"	"name"	"sqlFragmentEqFlags"	"toLowercaseescapeLucenebeginsWithendsWith"
		//		"Annotation"	"name"	"sqlFragmentOrderBy"	"name"
		builder.extend(MiningEnitityNames.ANNOTATION, "name")
			.withDisplayName("Annotation Description")
			.withSorting(MiningGraphQLQueries.ANNOTATIONS, AnnotationInquiryBuilder::sortName)
			.withFiltering(MiningGraphQLQueries.ANNOTATIONS, f -> f.<AnnotationInquiryBuilder, String>eq((q, v) -> q.withName(v.contains("*") ? v : "*" + v + "*")))
			.add();
		//		"Annotation"	"typeLink"	"sqlFragmentEq"	"typeLink.name = ?"
		//		"Annotation"	"typeLink"	"sqlFragmentIn"	"typeLink.name IN ?"
		//		"Annotation"	"typeLink"	"sqlFragmentOrderBy"	"typeLink.name"
		builder.extend(MiningEnitityNames.ANNOTATION, "type")
			.withDisplayName("Annotation Type")
			.withSorting(MiningGraphQLQueries.ANNOTATIONS, AnnotationInquiryBuilder::sortType)
			.withFiltering(MiningGraphQLQueries.ANNOTATIONS, f -> {
				f.eq(AnnotationInquiryBuilder::withType);
				f.in(AnnotationInquiryBuilder::withTypes);
			})
			.add();
	
		builder.extend(MiningEnitityNames.ANNOTATION, "state")
			.withDisplayName("State")
			.withSorting(MiningGraphQLQueries.ANNOTATIONS, AnnotationInquiryBuilder::sortState)
			.withFiltering(MiningGraphQLQueries.ANNOTATIONS, f -> {
				f.eq(AnnotationInquiryBuilder::withState);
				f.in(AnnotationInquiryBuilder::withStates);
			})
			.add();
		
		builder.extend(MiningEnitityNames.ANNOTATION, "annotationOffset")
			.withSorting(MiningGraphQLQueries.ANNOTATIONS, AnnotationInquiryBuilder::sortByModuleLocation)
			.add();
	
		//		"AnnotationCategory"	"name"	"sqlFragmentEq"	"categoryLink.name = ?"
		//		"AnnotationCategory"	"name"	"sqlFragmentIn"	"categoryLink.name IN ?"
		//		"AnnotationCategory"	"name"	"sqlFragmentNone"	"categoryLink.size() = 0"
		//		"AnnotationCategory"	"name"	"sqlFragmentOrderBy"	"categoryLink.name"
		builder.extend(MiningEnitityNames.ANNOTATION, "categoryName")
				.withDisplayName("Category")
				.withSorting(MiningGraphQLQueries.ANNOTATIONS, AnnotationInquiryBuilder::sortCategory)
				.withFiltering(MiningGraphQLQueries.ANNOTATIONS, f -> {
					f.eq(AnnotationInquiryBuilder::withCategory);
					f.in(AnnotationInquiryBuilder::withCategoryNames);
					f.isAbsent(AnnotationInquiryBuilder::withCategory);
				})
				.add();
	
		//		"Annotation"	"id"	"sqlFragmentEq"	"id = ?"
		//		"Annotation"	"id"	"sqlFragmentIn"	"id IN ?"
		//		"Annotation"	"id"	"SQL_FRAGMENT_LTE"	"id <= ?"
		//		"Annotation"	"id"	"SQL_FRAGMENT_GTE"	"id >= ?"
		builder.extend(MiningEnitityNames.ANNOTATION, "id")
				.withFiltering(MiningGraphQLQueries.ANNOTATIONS, f -> {
					//If this type needs to be String instead of Long, we need an EntityId.ofObject method that can handle both.
					f.<AnnotationInquiryBuilder, Long>eq((b, val) -> b.byId(EntityId.of(val)));
					/* WMIN-13996: Use annotation nids */
					f.<AnnotationInquiryBuilder, Collection<Long>>in((b, val) -> b.byNids(val));
					f.<AnnotationInquiryBuilder, Long>lte((b, val) -> b.byMaxId(EntityId.of(val)));
					f.<AnnotationInquiryBuilder, Long>gte((b, val) -> b.byMinId(EntityId.of(val)));
					f.<AnnotationInquiryBuilder, Long>gt((b, val) -> b.byMinId(EntityId.of(val)));
				})
				.withSorting(MiningGraphQLQueries.ANNOTATIONS, AnnotationInquiryBuilder::sortNid)
				.withDisplayName("Annotation Id")
				.withDescription("The unique id of the Annotation")
				.withUsage(Usages.MINING_UI_ANNOTATIONS_TABLE)
				.withUsageAttribute(Usages.MINING_UI_ANNOTATIONS_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsage(Usages.MINING_UI_GRAPHML_EXPORT)
				.withUsage(Usages.SEARCH_FILTER)
				.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_NUMBER)
				.withUsage(Usages.SORT_BY)
				.add();
	
		//		"Annotation"	"sourceAttachment"	"sqlFragmentEq"	"sourceAttachmentLink.content.toLowerCase() CONTAINSTEXT ?"
		//		"Annotation"	"sourceAttachment"	"sqlFragmentEqFlags"	"toLowercase"
		builder.extend(MiningEnitityNames.ANNOTATION, "sourceAttachment")
				.withDisplayName("Source Code")
				.withDescription("Source Code attached to the Annotation")
				.type(ScalarType.STRING)
				.withFiltering(MiningGraphQLQueries.ANNOTATIONS, f -> f.<AnnotationInquiryBuilder, String>eq(AnnotationInquiryBuilder::filterSourceContains))
				.withCustomFetch(env -> {
					final AnnotationPojo pojo = env.getSource();
					return pojo.getSourceAttachment().orElse(null);
				})
				.withUsage(Usages.MINING_UI_ANNOTATIONS_TABLE)
				.withUsageAttribute(Usages.MINING_UI_ANNOTATIONS_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsageAttribute(Usages.MINING_UI_ANNOTATIONS_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "5")
				.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_TEXT)
				.add();

	
		builder.extend(MiningEnitityNames.ANNOTATION, "moduleName")
				.withDisplayName("Module Name")
				.withFiltering(MiningGraphQLQueries.ANNOTATIONS, f -> f.<AnnotationInquiryBuilder, String>eq((b, val) -> b.withModuleName(val, true)))
				.withSorting(MiningGraphQLQueries.ANNOTATIONS, AnnotationInquiryBuilder::sortByModuleName)
				.add();
	
		builder.extend(MiningEnitityNames.ANNOTATION, "updatedByUserName")
			.withCustomFetch(env -> {
				final AnnotationPojo pojo = env.getSource();
				return pojo.getUpdatedByUserName();
			})
			.withDescription("Name of the user who last modified the Annotation")
			.withDisplayName("Modified By")
				.withUsage(Usages.MINING_UI_ANNOTATIONS_TABLE)
				.withUsageAttribute(Usages.MINING_UI_ANNOTATIONS_TABLE, TableAttributes.CATEGORY, "Modifications")
				.withUsageAttribute(Usages.MINING_UI_ANNOTATIONS_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "7")
			.add();

		builder.defineAlias(MiningEnitityNames.ANNOTATION, "linkedBusinessDataDictionary")
			.forDataPoint("linkedDataDictionaries")
			.withSubSelection("name")
			.withParameter(ISBUSINESS, true)
			.withDisplayName("Business Variables Referenced")
			.withDescription("Return the List of Business Variables Referenced associated with this Annotation")
			.withUsage(Usages.MINING_UI_ANNOTATIONS_TABLE)
			.withUsageAttribute(Usages.MINING_UI_ANNOTATIONS_TABLE, TableAttributes.CATEGORY, "Base Data")
			.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.TOGETHER_WITH, "linkedBusinessDataDictionary.id")
			.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.DISPLAY_AS, ViewModeAttributes.DISPLAY_AS_LINK_OPEN_MODAL)
			.add();
		
		builder.defineAlias(MiningEnitityNames.ANNOTATION, "linkedNonBusinessDataDictionary")
			.forDataPoint("linkedDataDictionaries")
			.withSubSelection("name")
			.withParameter(ISBUSINESS, false)
			.withDisplayName("Non-Business Variables Referenced")
			.withDescription("Return the List of Non-Business Variables Referenced associated with this Annotation")
			.withUsage(Usages.MINING_UI_ANNOTATIONS_TABLE)
			.withUsageAttribute(Usages.MINING_UI_ANNOTATIONS_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.TOGETHER_WITH,
						"linkedNonBusinessDataDictionary.id")
			.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.DISPLAY_AS, ViewModeAttributes.DISPLAY_AS_LINK_OPEN_MODAL)
			.add();

		builder.defineDataPoint(MiningEnitityNames.ANNOTATION, "functionalGroups")
				.arrayOfType(ANNOTATION_FUNCTIONAL_GROUP_TYPE_NAME)
				.add();

		builder.defineDataPoint(ANNOTATION_FUNCTIONAL_GROUP_TYPE_NAME, "uid")
				.type(UUID.class)
				.withDisplayName("Functional Group Id")
				.withDescription("Id of the Functional Group to which the Annotation belongs")
				.add();
		builder.defineDataPoint(ANNOTATION_FUNCTIONAL_GROUP_TYPE_NAME, "name")
				.type(String.class)
				.withDisplayName("Functional Block Name")
				.withDescription("Name of the Functional Block to which the Annotation belongs")
				.withUsage(Usages.MINING_UI_ANNOTATIONS_TABLE)
					.withUsageAttribute(Usages.MINING_UI_ANNOTATIONS_TABLE, TableAttributes.CATEGORY, "Functional Blocks")
					.withUsageAttribute(Usages.MINING_UI_ANNOTATIONS_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE,
							SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_ANNOTATION_AGGREGATED_VALUES)
					.withUsageAttribute(Usages.MINING_UI_ANNOTATIONS_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME, "CATEGORY")
				.withUsage(Usages.SEARCH_FILTER)
					.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_TEXT)
				.withUsage(Usages.SORT_BY)
					.withSorting(MiningGraphQLQueries.ANNOTATIONS, AnnotationInquiryBuilder::sortByFunctionalGroupName)
				.withFiltering(MiningGraphQLQueries.ANNOTATIONS, f -> {
					f.eq(AnnotationInquiryBuilder::withFunctionalGroupName);
					f.isAbsent((q, v) -> ((AnnotationInquiryBuilder) q).withoutFunctionalGroupAssignment());
				})
				.add();
		builder.defineDataPoint(ANNOTATION_FUNCTIONAL_GROUP_TYPE_NAME, "annotationSequenceNumber")
				.type(Integer.class)
				.withDisplayName("Sequence Number")
				.withDescription("Index of the Annotation inside of the Functional Block")
				.withUsage(Usages.MINING_UI_ANNOTATIONS_TABLE)
				.withUsageAttribute(Usages.MINING_UI_ANNOTATIONS_TABLE, TableAttributes.CATEGORY, "Functional Blocks")
				.add();
	}

}
