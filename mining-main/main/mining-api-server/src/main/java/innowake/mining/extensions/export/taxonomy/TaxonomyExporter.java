/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.extensions.export.taxonomy;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springframework.http.HttpEntity;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.Job;
import innowake.mining.data.datapoints.registry.DataPointRegistry;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.extensions.export.csv.DataPointCSVExporterJob;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.datapoints.definition.MiningDataPointDefinitionWithPath;
import innowake.mining.shared.datapoints.definition.usages.Usages;
import innowake.mining.shared.datapoints.definition.usages.attributes.miningui.TableAttributes;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;

/**
 * Taxonomy assignment exporter using {@link DataPointCSVExporterJob}.
 * <p>
 * This exporter will export all datapoints of Module that have the {@link Usages#TAXONOMY_ASSIGNMENTS_EXPORT} usage.
 */
@Component
public class TaxonomyExporter implements MiningJobExtension<FileSystemResult> {

	private final DataPointRegistry dataPointRegistry;

	public TaxonomyExporter(final DataPointRegistry dataPointRegistry) {
		this.dataPointRegistry = dataPointRegistry;
	}

	@Override
	public String getIdentifier() {
		return "taxonomy-assignments";
	}

	@Override
	public String getDescription() {
		return "Exports list of modules and respective taxonomy assignments";
	}

	@Override
	public Job<FileSystemResult> createJob(final EntityId projectId, final Map<String, List<String>> parameters, final HttpEntity<byte[]> inputData) {
		final String queryType = dataPointRegistry.getQueryDefinitions().get("modules").getReferenceTypeName();
		final List<MiningDataPointDefinitionWithPath> dataPoints = dataPointRegistry.getDataPointsForTypeRecursivelyWithUsage(
				Optional.of(projectId).map(EntityId::getNid), queryType, Collections.singletonList(Usages.TAXONOMY_ASSIGNMENTS_EXPORT));
		dataPoints.sort(Comparator.comparing(dp -> Optional.ofNullable(dp.getUsageAttributes().get(Usages.TAXONOMY_ASSIGNMENTS_EXPORT))
					.flatMap(attrs -> Optional.ofNullable(attrs.get(TableAttributes.DEFAULT_COLUMN_INDEX)))
					.orElseGet(dp::getName)));

		final Map<String, List<String>> exportParameters = Map.of(
			"$query", Collections.singletonList("modules"),
			"$columns", dataPoints.stream().map(MiningDataPointDefinitionWithPath::getPath).collect(Collectors.toList()),
			"$fileName", Collections.singletonList("taxonomy-assignments")
		);

		return new DataPointCSVExporterJob(projectId, exportParameters, SecurityContextHolder.getContext());
	}

	@Override
	public NatureType getRequiredNature() {
		return NatureType.MINING;
	}

	@Override
	public RoleType getRequiredRole() {
		return RoleType.VIEWER;
	}
}
