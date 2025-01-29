/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.graphql.controller;

import graphql.execution.DataFetcherResult;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.datapoints.MiningDataPointBuilder;
import innowake.mining.data.datapoints.MiningDataPointSource;
import innowake.mining.data.datapoints.registry.DataPointRegistry;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.graphql.MiningQueryMapping;
import innowake.mining.shared.MiningEnitityNames;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.SchedulerInfoService;
import innowake.mining.shared.datapoints.annotations.MiningDataPoint;
import innowake.mining.shared.datapoints.annotations.Usage;
import innowake.mining.shared.datapoints.annotations.UsageAttribute;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.TableAttributes;
import innowake.mining.shared.entities.scheduler.SchedulerImportPojo;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.SchemaMapping;
import org.springframework.stereotype.Controller;

import java.util.List;
import java.util.Optional;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.VIEWER;

/**
 * Defines data points for {@link SchedulerInfoGraphQlController}.
 * <p>
 * Implementation had to be moved to a separate class to fix circular dependency
 * on {@link DataPointRegistry}.
 */
@Controller
public class SchedulerInfoGraphQlController implements MiningDataPointSource {

	private final SchedulerInfoService schedulerInfoService;

	/**
	 * Constructor for SchedulerImportGraphQlController. Initializes the controller with a scheduler information service.
	 * @param schedulerInfoService The scheduler information service to be used by this controller.
	 */
	@Autowired
	public SchedulerInfoGraphQlController(final SchedulerInfoService schedulerInfoService) {
		this.schedulerInfoService = schedulerInfoService;
	}

	/**
	 * Fetches a paginated list of scheduler imports based on the provided project ID and pagination parameters.
	 * @param projectId The ID of the project for which scheduler imports are to be fetched.
	 * @param page The page number to fetch (zero-based). If null, defaults to 0.
	 * @param size The number of items per page. If null, defaults to 0.
	 * @return A DataFetcherResult containing a paginated list of SchedulerImportPojo objects.
	 */
	@MiningQueryMapping
	@Nature({MINING})
	@Role({VIEWER})
	public DataFetcherResult<Paged<SchedulerImportPojo>> schedulerImports(@Argument final Long projectId,
			@Argument @Nullable final Integer page,
			@Argument @Nullable final Integer size) {

		final int intPage = Optional.ofNullable(page).orElse(0);
		final int intSize = Optional.ofNullable(size).orElse(0);

		final Paged<SchedulerImportPojo> result = schedulerInfoService.findImports(Pagination.at(intPage, intSize), q -> q.ofProject(EntityId.of(projectId)));

		return DataFetcherResult.<Paged<SchedulerImportPojo>>newResult().data(result)
				.build();
	}

	/**
	 * Extracts and returns a list of operations selected during the scheduler import process.
	 * @param schedulerImport The scheduler import object from which to extract the operations.
	 * @return A list of strings representing the operations selected during the import. Returns "None" if no operations were selected.
	 */
	@SchemaMapping(typeName = MiningEnitityNames.SCHEDULER_IMPORT)
	@MiningDataPoint(displayName = "Operations Selected", description = "The operations selected while importing the file")
	@Usage(value = Usages.MINING_UI_SCHEDULER_IMPORT_TABLE, attributes = {
			@UsageAttribute(key = TableAttributes.CATEGORY, value = "Base Data")
	})
	public List<String> operationsSelected(final SchedulerImportPojo schedulerImport) {
		final List<String> operations = schedulerImport.getProperties().entrySet().stream().filter(entry -> entry.getValue().equals(true))
				.map(entry -> {
					if (entry.getKey().equals("createModuleIfMissing")) {
						return "Created Missing Module";
					} else {
						return "Established Module Relationship";
					}
				}).toList();
		return operations.isEmpty() ? List.of("None") : operations;
	}

	@Override
	public void provideDataPoints(final MiningDataPointBuilder builder) {
		builder.defineType(MiningEnitityNames.SCHEDULER_IMPORT)
				.representedBy(SchedulerImportPojo.class)
				.withDefaultProperties()
				.add();

		builder.defineDataPointsFromSchemaMappingAnnotations(SchedulerInfoGraphQlController.class);

		builder.extend(MiningEnitityNames.SCHEDULER_IMPORT, "identifier")
				.withDisplayName("Identifier")
				.withDescription("The scheduler import identifier")
				.withUsage(Usages.MINING_UI_SCHEDULER_IMPORT_TABLE)
				.withUsageAttribute(Usages.MINING_UI_SCHEDULER_IMPORT_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsageAttribute(Usages.MINING_UI_SCHEDULER_IMPORT_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "0")
				.add();

		builder.extend(MiningEnitityNames.SCHEDULER_IMPORT, "schedulerType")
				.withCustomFetch(env -> {
					final SchedulerImportPojo pojo = env.getSource();
					return pojo.getSchedulerType().name();
				})
				.withDisplayName("Scheduler Type")
				.withDescription("The scheduler type")
				.withUsage(Usages.MINING_UI_SCHEDULER_IMPORT_TABLE)
				.withUsageAttribute(Usages.MINING_UI_SCHEDULER_IMPORT_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsageAttribute(Usages.MINING_UI_SCHEDULER_IMPORT_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "1")
				.add();

		builder.extend(MiningEnitityNames.SCHEDULER_IMPORT, "importerUsed")
				.withDisplayName("Importer")
				.withDescription("The importer used to import the file")
				.withUsage(Usages.MINING_UI_SCHEDULER_IMPORT_TABLE)
				.withUsageAttribute(Usages.MINING_UI_SCHEDULER_IMPORT_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsageAttribute(Usages.MINING_UI_SCHEDULER_IMPORT_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "2")
				.add();

		builder.extend(MiningEnitityNames.SCHEDULER_IMPORT, "description")
				.withDisplayName("Description")
				.withDescription("The description of the import")
				.withUsage(Usages.MINING_UI_SCHEDULER_IMPORT_TABLE)
				.withUsageAttribute(Usages.MINING_UI_SCHEDULER_IMPORT_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsageAttribute(Usages.MINING_UI_SCHEDULER_IMPORT_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "3")
				.add();

		builder.extend(MiningEnitityNames.SCHEDULER_IMPORT, "importedOn")
				.withDisplayName("Uploaded On")
				.withDescription("The date when the import was done")
				.withUsage(Usages.MINING_UI_SCHEDULER_IMPORT_TABLE)
				.withUsageAttribute(Usages.MINING_UI_SCHEDULER_IMPORT_TABLE, TableAttributes.CATEGORY, "Base Data")
				.withUsageAttribute(Usages.MINING_UI_SCHEDULER_IMPORT_TABLE, TableAttributes.DEFAULT_COLUMN_INDEX, "4")
				.add();
	}
}
