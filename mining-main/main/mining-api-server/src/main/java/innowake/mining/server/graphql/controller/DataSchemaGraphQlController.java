/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.graphql.controller;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.SchemaMapping;
import org.springframework.stereotype.Controller;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.datapoints.MiningDataPointBuilder;
import innowake.mining.data.datapoints.MiningDataPointSource;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.graphql.MiningQueryMapping;
import innowake.mining.shared.access.FieldInfoService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.annotations.Usage;
import innowake.mining.shared.datapoints.annotations.UsageAttribute;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.general.SearchFilterAttributes;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.TableAttributes;
import innowake.mining.shared.entities.FieldInfoPojo;

/**
 * Controller for the "fieldInfos" GraphQl Query.
 */
@Controller
public class DataSchemaGraphQlController implements MiningDataPointSource {

	@Autowired
	private FieldInfoService fieldInfoService;

	/**
	 * Query for field infos of a module.
	 * 
	 * @param projectId the ID of the project that contains the module
	 * @param moduleId ID of the module
	 * @return the list of field information for the module
	 */
	@MiningQueryMapping
	@Nature({MINING})
	@Role({VIEWER})
	public List<FieldInfoPojo> fieldInfos(@Argument final EntityId projectId, @Argument final EntityId moduleId) {
		return fieldInfoService.find(q -> q.ofProject(projectId)
											.ofModule(moduleId));
	}

	@SchemaMapping(typeName = "FieldInfo")
	@MiningDataPoint(displayName = "Data Type", description = "Data Type of the Field or Column")
	@Usage(value = Usages.SEARCH_FILTER, attributes = {
			@UsageAttribute(key = SearchFilterAttributes.FILTER_MODE, value = SearchFilterAttributes.FILTER_MODE_MULTI_SELECT)
	})
	@Usage(value = Usages.MINING_UI_TABLE_COLUMNS_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "2")
	})
	@Nullable
	public String dataType(final FieldInfoPojo fieldInfo) {
		return getProperty(fieldInfo, "type");
	}

	@SchemaMapping(typeName = "FieldInfo")
	@MiningDataPoint(displayName = "Primary Key", description = "The index of the column in the table's primary key")
	@Usage(value = Usages.SEARCH_FILTER, attributes = {
			@UsageAttribute(key = SearchFilterAttributes.FILTER_MODE, value = SearchFilterAttributes.FILTER_MODE_MULTI_SELECT)
	})
	@Usage(value = Usages.MINING_UI_TABLE_COLUMNS_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "5")
	})
	@Nullable
	public String primaryKey(final FieldInfoPojo fieldInfo) {
		return getProperty(fieldInfo, "primaryKey");
	}

	@SchemaMapping(typeName = "FieldInfo")
	@MiningDataPoint(displayName = "Auto Increment", description = "Whether the value for this column is generated via auto increment")
	@Usage(value = Usages.SEARCH_FILTER, attributes = {
			@UsageAttribute(key = SearchFilterAttributes.FILTER_MODE, value = SearchFilterAttributes.FILTER_MODE_MULTI_SELECT)
	})
	@Usage(value = Usages.MINING_UI_TABLE_COLUMNS_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "6")
	})
	public boolean autoIncrement(final FieldInfoPojo fieldInfo) {
		final var properties = fieldInfo.getProperties();
		return properties.isPresent() && properties.get().containsKey("autoIncrement");
	}
	
	@SchemaMapping(typeName = "FieldInfo")
	@MiningDataPoint(displayName = "Field/Column Length", description = "The length of Field/Columns of the Tables")
	@Usage(value = Usages.SEARCH_FILTER, attributes = {
			@UsageAttribute(key = SearchFilterAttributes.FILTER_MODE, value = SearchFilterAttributes.FILTER_MODE_NUMBER)
	})
	@Usage(value = Usages.MINING_UI_TABLE_COLUMNS_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.DEFAULT_COLUMN_INDEX, value = "3")
	})
	public Long size(final FieldInfoPojo fieldInfo) {
		final String property = getProperty(fieldInfo, "size");
		return property == null ? -1l : Long.valueOf(property);
	}

	@Override
	public void provideDataPoints(MiningDataPointBuilder builder) {
		builder.defineDataPointsFromSchemaMappingAnnotations(this);
	}

	@Nullable
	private static String getProperty(final FieldInfoPojo fieldInfo, final String propertyName) {
		return fieldInfo.getProperties()
				.map(properties -> (String) properties.get(propertyName))
				.orElse(null);
	}
}
