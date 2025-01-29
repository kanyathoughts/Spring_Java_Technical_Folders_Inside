/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.export.csv;

import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.List;
import java.util.Map;

import org.springframework.http.HttpEntity;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.Job;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;

/**
 * Generates CSV export from data points.
 * <p>
 * This exporter can be used to export anything that can be queried via GraphQL in CSV format. To do this a GET request must be sent to the exporter's
 * endpoint. The endpoint understands the following query parameters:
 * <ul>
 *     <li>{@code $query}: The name of the (top-level) GraphQL query (e.g. "modules", "annotations"). This option is mandatory.</li>
 *     <li>{@code $columns}: The paths of data points relative to the query's return type that shall be exported.
 *         This option can be passed multiple times to export multiple columns and at least one column is required.</li>
 *     <li>{@code $unroll}: An optional boolean parameter indicating whether the exported table shall be "flat", i.e
 *         it shall contain no nested arrays in any of the cells. See below for an example. Default: false</li>
 *    <li>Any other query parameters (e.g. "filter") are passed as arguments to the GraphQL query.</li>
 * </ul>
 * <p>
 * Example for the {@code $unroll} parameter:
 * <p>
 * Assume we want to export a Module with Taxonomies. If the module has multiple Taxonomies of one Taxonomy Type assigned, then the resulting table
 * would look like this:
 * <pre>
 *     "Module Name", "My Taxonomy Type"
 *     "FOOBAR",      "[Taxonomy1, Taxonomy2]"
 * </pre>
 * When the {@code $unroll} parameter is set, then the inner array is "unrolled" by duplicating rows. The result will look like this:
 * <pre>
 *     "Module Name", "My Taxonomy Type"
 *     "FOOBAR",      "Taxonomy1"
 *     "FOOBAR",      "Taxonomy2"
 * </pre>
 */
@Service
public class DataPointCSVExporter implements MiningJobExtension<FileSystemResult> {

	@Override
	public String getIdentifier() {
		return "datapoint-csv";
	}

	@Override
	public String getDescription() {
		return "CSV Data";
	}
	
	@Override
	public NatureType getRequiredNature() {
		return MINING;
	}

	@Override
	public RoleType getRequiredRole() {
		return VIEWER;
	}

	@Override
	public Job<FileSystemResult> createJob(final EntityId projectId, final Map<String, List<String>> parameters, final HttpEntity<byte[]> inputData) {
		return new DataPointCSVExporterJob(projectId, parameters, SecurityContextHolder.getContext());
	}
}
