/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.export.csv;

import static innowake.mining.shared.model.FeatureId.ORDERED_ANNOTATION_RULE_CSV_EXPORTER;
import static innowake.mining.shared.security.NatureType.MINING;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.List;
import java.util.Map;

import org.ff4j.FF4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;

import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.Job;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;

/**
 * Generates Ordered Annotation Rule CSV export from data points.
 * <p>
 * This exporter can be used to export Ordered Annotation Rule that can be queried via GraphQL in CSV format. To do this a GET request must be 
 * sent to the exporter's endpoint. The endpoint understands the following query parameters:
 * <ul>
 *     <li>{@code $query}: The name of the (top-level) GraphQL query (e.g. "annotations"). This option is mandatory.</li>
 *     <li>{@code $columns}: The paths of data points relative to the query's return type that shall be exported.
 *         This option can be passed multiple times to export multiple columns and at least one column is required.</li>
 *     <li>{@code $unroll}: An optional boolean parameter indicating whether the exported table shall be "flat", i.e
 *         it shall contain no nested arrays in any of the cells. See below for an example. Default: false</li>
 *    <li>Any other query parameters (e.g. "filter") are passed as arguments to the GraphQL query.</li>
 * </ul>
 * <p>
 */
@Service
public class OrderedAnnotationRuleCSVExporter implements MiningJobExtension<FileSystemResult> {
	
	@Autowired
	private FF4j ff4j;
	
	@Override
	public String getIdentifier() {
		return "ordered-annotation-rule-csv";
	}

	@Override
	public String getDescription() {
		return "Ordered Annotation Rule CSV Data";
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
		if ( ! ff4j.getFeature(ORDERED_ANNOTATION_RULE_CSV_EXPORTER.getId()).isEnable()) {
			throw new ResponseStatusException(HttpStatus.FORBIDDEN,
					"The orderedAnnotationRuleCsvExporter feature toggle must be active in order to use this feature");
		}
		return new OrderedAnnotationRuleCSVExporterJob(projectId, parameters, SecurityContextHolder.getContext());
	}

}
