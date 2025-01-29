/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.graphql.controller;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import graphql.execution.DataFetcherResult;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.access.postgres.DataDictionaryPgDao.DataDictionaryQueryBuilder;
import innowake.mining.data.access.postgres.PgUtil;
import innowake.mining.data.datapoints.FilterObjectService;
import innowake.mining.data.datapoints.MiningDataPointBuilder;
import innowake.mining.data.datapoints.MiningDataPointSource;
import innowake.mining.data.datapoints.SortObjectService;
import innowake.mining.data.datapoints.registry.DataPointRegistry;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.graphql.MiningGraphQLQueries;
import innowake.mining.server.graphql.MiningQueryMapping;
import innowake.mining.server.util.UserNameUtil;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.AnnotationService;
import innowake.mining.shared.access.DataDictionaryService;
import innowake.mining.shared.access.DataDictionaryService.DataDictionaryInquiryBuilder;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.annotations.Usage;
import innowake.mining.shared.datapoints.annotations.UsageAttribute;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinition.ScalarType;
import innowake.mining.shared.datapoints.definition.filters.FilterOperators;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.SearchFilterAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.ViewModeAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.TableAttributes;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.DataDictionaryPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.model.DataDictionaryVariableScope;
import innowake.mining.shared.model.DefinedLocation;
import innowake.mining.shared.service.UserRoleService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.SchemaMapping;
import org.springframework.stereotype.Controller;

/**
 * Defines data points for {@link DataDictionaryGraphQlController}.
 *
 * Implementation had to be moved to a separate class to fix circular dependency
 * on {@link DataPointRegistry}.
 */
@Controller
public class DataDictionaryGraphQlController implements MiningDataPointSource {

	public static final String QUERY_NAME = "dataDictionaries";
	static final String SYSTEM_USER = "system_user";
	private final AnnotationService annotationService;
	private final DataDictionaryService dataDictionaryService;
	private final ModuleService moduleService;
	private final SortObjectService sortObjectService;
	private final FilterObjectService filterObjectService;
	private final UserRoleService userRoleService;
	private final UserNameUtil userNameUtil;

	@Autowired
	public DataDictionaryGraphQlController(final AnnotationService annotationService, final DataDictionaryService dataDictionaryService,
			final ModuleService moduleService, final SortObjectService sortObjectService,
			final FilterObjectService filterObjectService, final UserRoleService userRoleService, final UserNameUtil userNameUtil) {
		this.annotationService = annotationService;
		this.dataDictionaryService = dataDictionaryService;
		this.moduleService = moduleService;
		this.sortObjectService = sortObjectService;
		this.filterObjectService = filterObjectService;
		this.userRoleService = userRoleService;
		this.userNameUtil = userNameUtil;
	}

	/**
	 * Query for {@linkplain DataDictionaryPojo dataDictionaryEntries} of a Data Dictionary.
	 * 
	 * @param projectId the ID of the project that contains the dataDictionaryEntries
	 * @param page the page number
	 * @param size the number of elements
	 * @param sortObject sort conditions in object format
	 * @param filterObject filtering conditions in object format
	 * @return the list of statements contained in the dataDictionaryEntry
	 */
	@MiningQueryMapping
	@Nature({MINING})
	@Role({VIEWER})
	public DataFetcherResult<Paged<DataDictionaryPojo>> dataDictionaries(@Argument final Long projectId,
			@Argument @Nullable final Integer page,
			@Argument @Nullable final Integer size,
			@Argument(name = "sortObject") @Nullable final List<Map<String, String>> sortObject,
			@Argument(name = "filterObject") @Nullable final Map<String, Object> filterObject) {

		final int intPage = Optional.ofNullable(page).orElse(0);
		final int intSize = Optional.ofNullable(size).orElse(0);

		final Paged<DataDictionaryPojo> result = dataDictionaryService.find(Pagination.at(intPage, intSize), q -> {
			q.ofModuleProject(EntityId.of(projectId));
			if (filterObject != null) {
				filterObjectService.applyFilterObject(projectId, QUERY_NAME, filterObject, q);
			}
			if (sortObject != null) {
				sortObjectService.applySortObject(projectId, QUERY_NAME, sortObject, q);
			}
		});

		return DataFetcherResult.<Paged<DataDictionaryPojo>>newResult().data(result)
				.localContext(
						new ControllerLocalContext(projectId, userRoleService.getProjectIds(), userRoleService.getClientAdminIds(), userRoleService.isAdmin()))
				.build();
	}
	
	@SchemaMapping(typeName = MiningEnitityNames.DATA_DICTIONARY)
	@MiningDataPoint(displayName = "Field Type", description = "Whether the source field is a GROUP or an ELEMENTARY field.")
	@Usage(value = Usages.MINING_UI_DATADICTIONARY_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "3")
	})
	public String fieldType(final DataDictionaryPojo dataDictionary) {
		return "GROUP".equalsIgnoreCase(dataDictionary.getFormat().orElse(null)) ? "GROUP" : "ELEMENTARY";
	}
	
	@SchemaMapping(typeName = MiningEnitityNames.DATA_DICTIONARY)
	@MiningDataPoint(displayName = "Created By", description = "The user Name for the user who created the data dictionary entry field")
	@Usage(value = Usages.MINING_UI_DATADICTIONARY_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Modifications"),
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "9")
	})
	public String createdByUserName(final DataDictionaryPojo dataDictionaryEntry) {
		return userNameUtil.getUserName(dataDictionaryEntry.getCreatedByUserId());
	}
	
	@SchemaMapping(typeName = MiningEnitityNames.DATA_DICTIONARY)
	@MiningDataPoint(displayName = "Modified By", description = "Name of the user who last modified the Annotation")
	@Usage(value = Usages.MINING_UI_DATADICTIONARY_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Modifications")
	})
	public String updatedByUserName(final DataDictionaryPojo dataDictionaryEntry) {
		final String userName = dataDictionaryEntry.getUpdatedByUserId().orElse(null);
		if (userName == null || userName.equals(SYSTEM_USER)) {
			return "";
		}
		return userNameUtil.getUserName(userName);
	}

	@SchemaMapping(typeName = MiningEnitityNames.DATA_DICTIONARY)
	@MiningDataPoint(displayName = "Linked Annotations", description = "The linked annotations of the data dictionary entry field")
	public List<AnnotationPojo> linkedAnnotations(final DataDictionaryPojo dataDictionaryEntry) {
		return annotationService.find(q -> q.ofDataDictionaryEntry(dataDictionaryEntry.identity()));
	}

	@SchemaMapping(typeName = MiningEnitityNames.DATA_DICTIONARY)
	public ModulePojo module(final DataDictionaryPojo dataDictionaryEntry) {
		return moduleService.getModule(dataDictionaryEntry.getModule());
	}
	
	@Override
	public void provideDataPoints(final MiningDataPointBuilder builder) {
		/* add DataDictionaryEntryV2 to schema */
		builder.defineType(MiningEnitityNames.DATA_DICTIONARY)
			.representedBy(DataDictionaryPojo.class)
			.withDefaultProperties()
			.add();
		
		builder.extend(MiningEnitityNames.DATA_DICTIONARY, "scopes")
			.type(ScalarType.JSON)
			.add();
		
		builder.defineDataPoint(MiningEnitityNames.DATA_DICTIONARY, "scopeNames")
			.arrayOfType(ScalarType.STRING)
			.withCustomFetch(env -> env.<DataDictionaryPojo>getSource().getScopes().keySet().stream().map(DataDictionaryVariableScope::toString).toArray())
			.withDisplayName("Scope")
			.withDescription("The identified scope or usage of the data dictionary entry field")
			.withUsage(Usages.MINING_UI_DATADICTIONARY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE,
						SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_DATA_DICTIONARY_AGGREGATED_VALUES)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME, "SCOPE_LINK")
			.withUsage(Usages.SEARCH_FILTER)
				.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_MULTI_SELECT)
			.withFiltering(MiningGraphQLQueries.DATA_DICTIONARY, f -> {
					f.<DataDictionaryQueryBuilder, String>eq((q, value) -> q.withScope(FilterOperators.OPERATOR_EQ, value));
					f.<DataDictionaryQueryBuilder, List<String>>in((q, value) -> q.withScope(FilterOperators.OPERATOR_IN, value));
			})
			.add();

		/* define queries and additional custom data points */
		builder.defineDataPointsFromSchemaMappingAnnotations(DataDictionaryGraphQlController.class);

		builder.defineAlias(MiningEnitityNames.DATA_DICTIONARY, "linkedBusinessRules")
			.forDataPoint("linkedAnnotations")
			.withSubSelection("name")
			.withDisplayName("Annotation Referencing Entry")
			.withDescription("Return the List of Annotation associated with this Data Dictionary.")
			.withUsage(Usages.MINING_UI_DATADICTIONARY_TABLE)
			.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.CATEGORY, "Base Data")
			.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.TOGETHER_WITH, "linkedBusinessRules.id")
			.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.DISPLAY_AS, ViewModeAttributes.DISPLAY_AS_LINK_OPEN_MODAL)
			.add();

		builder.extend(MiningEnitityNames.DATA_DICTIONARY, "id")
			.withDisplayName("Entry ID")
			.withDescription("ID of the Data Dictionary Entry.")
			.withUsage(Usages.MINING_UI_DATADICTIONARY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.CATEGORY, "Base Data")
			.withUsage(Usages.SEARCH_FILTER)
				.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_NUMBER)
			.withUsage(Usages.SORT_BY)
			.withFiltering(MiningGraphQLQueries.DATA_DICTIONARY, f -> {
					f.<DataDictionaryService.DataDictionaryInquiryBuilder, Long>eq((b, value) -> b.byId(EntityId.of(value)));
					f.<DataDictionaryService.DataDictionaryInquiryBuilder, Collection<Long>>in((b, value) -> b.byNids(value));
			})
			.withSorting(MiningGraphQLQueries.DATA_DICTIONARY, DataDictionaryService.DataDictionaryInquiryBuilder::sortNid)
			.add();

		builder.extend(MiningEnitityNames.DATA_DICTIONARY, "name")
			.withDisplayName("Field Name")
			.withDescription("The name of the Data Field.")
			.withUsage(Usages.MINING_UI_DATADICTIONARY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "0")
			.withUsage(Usages.SEARCH_FILTER)
				.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_TEXT)
			.withUsage(Usages.SORT_BY)
			.withFiltering(MiningGraphQLQueries.DATA_DICTIONARY, f ->
					f.<DataDictionaryService.DataDictionaryInquiryBuilder, String>eq((b, value) -> {
						final String transformedValue = PgUtil.pgPattern(value.trim()) + "%";
						b.withName(transformedValue);
					}))
			.withSorting(MiningGraphQLQueries.DATA_DICTIONARY, DataDictionaryService.DataDictionaryInquiryBuilder::sortName)
			.add();

		builder.extend(MiningEnitityNames.MODULE, "id")
			.withFiltering(MiningGraphQLQueries.DATA_DICTIONARY, f -> {
				f.<DataDictionaryInquiryBuilder, Long>eq((q, v) -> q.ofModule(EntityId.of(v)));
				f.<DataDictionaryInquiryBuilder, Collection<Long>>in((q, v) -> q.ofModules(v.stream().map(EntityId::of).collect(Collectors.toList())));
			})
			.add();

		builder.extend(MiningEnitityNames.MODULE, "name")
			.withDisplayName("Module Name")
			.withDescription("The module for which the data dictionary entry field was created")
			.withUsage(Usages.MINING_UI_DATADICTIONARY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "8")
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.CATEGORY, "Base Data")
			.withUsage(Usages.SEARCH_FILTER)
				.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_TEXT)
			.withFiltering(MiningGraphQLQueries.DATA_DICTIONARY, f ->
					f.<DataDictionaryService.DataDictionaryInquiryBuilder, String>eq((b, value) -> {
						final String transformedValue = PgUtil.pgPattern(value.trim()) + "%";
						b.ofModuleName(transformedValue);
					}))
			.withSorting(MiningGraphQLQueries.DATA_DICTIONARY, DataDictionaryService.DataDictionaryInquiryBuilder::sortModuleName)
			.add();

		builder.extend(MiningEnitityNames.DATA_DICTIONARY, "length")
			.withDisplayName("Length")
			.withDescription("The length of the Data Field")
			.withUsage(Usages.MINING_UI_DATADICTIONARY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "5")
			.withSorting(MiningGraphQLQueries.DATA_DICTIONARY, DataDictionaryService.DataDictionaryInquiryBuilder::sortLength)
			.add();

		builder.extend(MiningEnitityNames.DATA_DICTIONARY, "format")
			.withDisplayName("Field Format")
			.withDescription("The format of the Data Field.")
			.withUsage(Usages.MINING_UI_DATADICTIONARY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "4")
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE,
						SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_DATA_DICTIONARY_AGGREGATED_VALUES)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME, "FORMAT")
			.withUsage(Usages.SEARCH_FILTER)
				.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_MULTI_SELECT)
			.withFiltering(MiningGraphQLQueries.DATA_DICTIONARY, f -> {
					f.<DataDictionaryService.DataDictionaryInquiryBuilder, String>eq((b, v) -> b.withFormats(List.of(v)));
					f.in(DataDictionaryService.DataDictionaryInquiryBuilder::withFormats);
			})
			.add();

		builder.extend(MiningEnitityNames.DATA_DICTIONARY, "description")
			.withDisplayName("Field Description")
			.withDescription("Field Description of Data Dictionary")
			.withUsage(Usages.MINING_UI_DATADICTIONARY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "1")
			.withFiltering(MiningGraphQLQueries.DATA_DICTIONARY, f ->
					f.eq(DataDictionaryService.DataDictionaryInquiryBuilder::withDescription))
			.add();

		builder.extend(MiningEnitityNames.DATA_DICTIONARY, "scopes")
			.withDisplayName("Scope")
			.withDescription("The identified scope or usage of the data dictionary entry field")
			.withFiltering(MiningGraphQLQueries.DATA_DICTIONARY, f -> {
					f.<DataDictionaryQueryBuilder, String>eq((q, value) -> q.withScope(FilterOperators.OPERATOR_EQ, value));
					f.<DataDictionaryQueryBuilder, List<String>>in((q, value) -> q.withScope(FilterOperators.OPERATOR_IN, value));
			})
			.add();

		builder.extend(MiningEnitityNames.DATA_DICTIONARY, "picClause")
			.withDisplayName("PIC Clause")
			.withDescription("The PIC clause of the Data Field")
			.withUsage(Usages.MINING_UI_DATADICTIONARY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.CATEGORY, "Base Data")
			.withUsage(Usages.SEARCH_FILTER)
				.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_TEXT)
			.withFiltering(MiningGraphQLQueries.DATA_DICTIONARY, f ->
					f.<DataDictionaryService.DataDictionaryInquiryBuilder, String>eq((b, value) -> {
						final String transformedValue = PgUtil.pgPattern(value.trim()) + "%";
						b.withPicClause(transformedValue);
					}))
			.add();

		builder.extend(MiningEnitityNames.DATA_DICTIONARY, "definedLocation")
			.withDisplayName("Defined Location")
			.withDescription("The defined location of the Data Field")
			.withUsage(Usages.MINING_UI_DATADICTIONARY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE,
						SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_DATA_DICTIONARY_AGGREGATED_VALUES)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME, "DEFINED_LOCATION")
			.withUsage(Usages.SEARCH_FILTER)
				.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_MULTI_SELECT)
			.withUsage(Usages.VIEW_MODE)
				.withUsageAttribute(Usages.VIEW_MODE, ViewModeAttributes.LABEL_MAPPING, "DEFINED_LOCATION_LABELS")
			.withFiltering(MiningGraphQLQueries.DATA_DICTIONARY, f -> {
					f.<DataDictionaryService.DataDictionaryInquiryBuilder, DefinedLocation>eq((q, v) -> q.withDefinedLocations(List.of(v)));
					f.in(DataDictionaryService.DataDictionaryInquiryBuilder::withDefinedLocations);
			})
			.add();

		builder.extend(MiningEnitityNames.DATA_DICTIONARY, "state")
			.withDisplayName("State")
			.withDescription("State of the data dictionary entry")
			.withUsage(Usages.MINING_UI_DATADICTIONARY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE,
						SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_DATA_DICTIONARY_AGGREGATED_VALUES)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME, "STATE")
			.withUsage(Usages.SEARCH_FILTER)
				.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_MULTI_SELECT)
			.withFiltering(MiningGraphQLQueries.DATA_DICTIONARY, f ->
					f.in(DataDictionaryService.DataDictionaryInquiryBuilder::withStates))
			.add();

		builder.extend(MiningEnitityNames.DATA_DICTIONARY, "isBusiness")
			.withDisplayName("Business")
			.withDescription("Indicates whether the data dictionary field is business-related")
			.withUsage(Usages.MINING_UI_DATADICTIONARY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE,
						SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_DATA_DICTIONARY_AGGREGATED_VALUES)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME, "IS_BUSINESS")
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, SearchFilterAttributes.RSQL_FRAGMENT, "isBusiness=in=($'{$query})")
			.withUsage(Usages.SEARCH_FILTER)
				.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_MULTI_SELECT)
			.withFiltering(MiningGraphQLQueries.DATA_DICTIONARY, f -> {
				f.<DataDictionaryService.DataDictionaryInquiryBuilder, Object>isTrue((b, v) -> b.withIsBusiness(true));
				f.<DataDictionaryService.DataDictionaryInquiryBuilder, Object>isFalse((b, v) -> b.withIsBusiness(false));
			})
			.add();

		builder.extend(MiningEnitityNames.DATA_DICTIONARY, "fieldTransformation")
			.withDisplayName("Field Transformation")
			.withDescription("Line of code where the data dictionary entry field final value is computed or a value moved into it")
			.withUsage(Usages.MINING_UI_DATADICTIONARY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.CATEGORY, "Base Data")
			.withUsage(Usages.SEARCH_FILTER)
				.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_TEXT)
			.withFiltering(MiningGraphQLQueries.DATA_DICTIONARY, f ->
					f.<DataDictionaryService.DataDictionaryInquiryBuilder, String>eq((b, value) -> {
						final String transformedValue = PgUtil.pgPattern(value.trim()) + "%";
						b.withFieldTransformation(transformedValue);
					}))
			.add();

		builder.extend(MiningEnitityNames.DATA_DICTIONARY, "sourceInput")
			.withDisplayName("Source Input")
			.withDescription("Any manually defined source/input information about the data dictionary entry field")
			.withUsage(Usages.MINING_UI_DATADICTIONARY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.CATEGORY, "Description")
			.withUsage(Usages.SEARCH_FILTER)
				.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_TEXT)
			.withFiltering(MiningGraphQLQueries.DATA_DICTIONARY, f ->
					f.<DataDictionaryService.DataDictionaryInquiryBuilder, String>eq((b, value) -> {
						final String transformedValue = PgUtil.pgPattern(value.trim()) + "%";
						b.withSourceInput(transformedValue);
					}))
			.add();

		builder.extend(MiningEnitityNames.DATA_DICTIONARY, "targetOutput")
			.withDisplayName("Target Output")
			.withDescription("Any manually defined target/output information about the data dictionary entry field")
			.withUsage(Usages.MINING_UI_DATADICTIONARY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.CATEGORY, "Description")
			.withUsage(Usages.SEARCH_FILTER)
				.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_TEXT)
			.withFiltering(MiningGraphQLQueries.DATA_DICTIONARY, f ->
					f.<DataDictionaryService.DataDictionaryInquiryBuilder, String>eq((b, value) -> {
						final String transformedValue = PgUtil.pgPattern(value.trim()) + "%";
						b.withTargetOutput(transformedValue);
					}))
			.add();

		builder.extend(MiningEnitityNames.DATA_DICTIONARY, "initialValue")
			.withDisplayName("Initial Value")
			.withDescription("Any initial information about the data dictionary entry field")
			.withUsage(Usages.MINING_UI_DATADICTIONARY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.CATEGORY, "Description")
			.withUsage(Usages.SEARCH_FILTER)
				.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_TEXT)
			.withFiltering(MiningGraphQLQueries.DATA_DICTIONARY, f ->
					f.<DataDictionaryService.DataDictionaryInquiryBuilder, String>eq((b, value) -> {
						final String transformedValue = PgUtil.pgPattern(value.trim()) + "%";
						b.withInitialValue(transformedValue);
					}))
			.add();

		builder.extend(MiningEnitityNames.DATA_DICTIONARY, "isReferenced")
			.withDisplayName("Referenced")
			.withDescription("Indicates whether a data dictionary entry field is referenced within the module code")
			.withUsage(Usages.MINING_UI_DATADICTIONARY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE,
						SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_DATA_DICTIONARY_AGGREGATED_VALUES)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME, "IS_REFERENCED")
			.withUsage(Usages.SEARCH_FILTER)
				.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_MULTI_SELECT)
			.withFiltering(MiningGraphQLQueries.DATA_DICTIONARY, f -> {
				f.<DataDictionaryService.DataDictionaryInquiryBuilder, Object>isTrue((b, v) -> b.withIsReferenced(true));
				f.<DataDictionaryService.DataDictionaryInquiryBuilder, Object>isFalse((b, v) -> b.withIsReferenced(false));
				f.in(DataDictionaryService.DataDictionaryInquiryBuilder::withIsReferencedIn);
			})
			.add();

		builder.extend(MiningEnitityNames.DATA_DICTIONARY, "usage")
			.withDisplayName("Field Usage")
			.withDescription("Shows the internals of the usage clause on a Cobol field declaration")
			.withUsage(Usages.MINING_UI_DATADICTIONARY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE,
						SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_DATA_DICTIONARY_AGGREGATED_VALUES)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME, "FIELD_USAGE")
			.withUsage(Usages.SEARCH_FILTER)
				.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_MULTI_SELECT)
			.withFiltering(MiningGraphQLQueries.DATA_DICTIONARY, f ->
					f.in(DataDictionaryService.DataDictionaryInquiryBuilder::withUsages))
			.add();

		builder.extend(MiningEnitityNames.DATA_DICTIONARY, "fieldLevel")
			.withDisplayName("Field Level")
			.withDescription("The level of the COBOL data dictionary entry field")
			.withUsage(Usages.MINING_UI_DATADICTIONARY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "7")
			.withUsage(Usages.SEARCH_FILTER)
				.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_TEXT)
			.withUsage(Usages.SORT_BY)
			.withSorting(MiningGraphQLQueries.DATA_DICTIONARY, DataDictionaryService.DataDictionaryInquiryBuilder::sortFieldLevel)
			.add();

		builder.extend(MiningEnitityNames.DATA_DICTIONARY, "parentGroup")
			.withDisplayName("Group Field")
			.withDescription("The parent group of the data dictionary entry field")
			.withUsage(Usages.MINING_UI_DATADICTIONARY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "6")
			.withUsage(Usages.SEARCH_FILTER)
				.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_TEXT)
			.withUsage(Usages.SORT_BY)
			.withFiltering(MiningGraphQLQueries.DATA_DICTIONARY, f ->
					f.<DataDictionaryService.DataDictionaryInquiryBuilder, String>eq((b, value) -> {
						final String transformedValue = PgUtil.pgPattern(value.trim()).toLowerCase() + "%";
						b.withParentGroup(transformedValue);
					}))
			.withSorting(MiningGraphQLQueries.DATA_DICTIONARY, DataDictionaryService.DataDictionaryInquiryBuilder::sortParentGroup)
			.add();
		
		builder.extend(MiningEnitityNames.DATA_DICTIONARY, "translatedFieldValue")
		.withDisplayName("Translated Field Value")
		.withDescription("The custom translation of this DataDictionaryEntry")
		.withUsage(Usages.MINING_UI_DATADICTIONARY_TABLE)
			.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.CATEGORY, "Base Data")
			.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "2")
		.withUsage(Usages.SEARCH_FILTER)
			.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_TEXT)
		.withUsage(Usages.SORT_BY)
		.withSorting(MiningGraphQLQueries.DATA_DICTIONARY, DataDictionaryService.DataDictionaryInquiryBuilder::sortTranslatedFieldValue)
		.add();

		builder.defineDataPoint(MiningEnitityNames.DATA_DICTIONARY, "accessType")
				.arrayOfType(ScalarType.STRING)
				.withCustomFetch(env -> env.<DataDictionaryPojo>getSource().getScopes().values().stream()
						.map(attr -> attr.get("accessType"))
						.flatMap(attrString -> {
							if (attrString != null) {
								final var split = attrString.split(",");
								return Arrays.stream(split);
							}
							return null;
						})
						.collect(Collectors.toSet()))
				.withDisplayName("Access Type")
				.withDescription("The identified access type of the data dictionary entry field")
				.withUsage(Usages.MINING_UI_DATADICTIONARY_TABLE)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE,
						SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_MODE_DATA_DICTIONARY_AGGREGATED_VALUES)
				.withUsageAttribute(Usages.MINING_UI_DATADICTIONARY_TABLE, SearchFilterAttributes.MULTI_SELECT_VALUE_RETRIEVAL_FIELD_NAME, "SCOPE_ATTRIBUTES")
				.withUsage(Usages.SEARCH_FILTER)
				.withUsageAttribute(Usages.SEARCH_FILTER, SearchFilterAttributes.FILTER_MODE, SearchFilterAttributes.FILTER_MODE_MULTI_SELECT)
				.withFiltering(MiningGraphQLQueries.DATA_DICTIONARY, f -> {
					f.<DataDictionaryQueryBuilder, String>eq((q, value) -> q.withScopeAccessType(FilterOperators.OPERATOR_EQ, value));
					f.<DataDictionaryQueryBuilder, List<String>>in((q, value) -> q.withScopeAccessType(FilterOperators.OPERATOR_IN, value));
				})
				.add();
	}
}
