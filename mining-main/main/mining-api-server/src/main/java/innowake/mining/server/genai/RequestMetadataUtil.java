/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.genai;

import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.SourceMetricsPojo;
import innowake.mining.shared.model.Technology;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

/**
 * Utility class for generating cost-tracking-related metadata for requests.
 * This Utility was added, so we can define and view the metadata that is tracked in one place.
 */
public class RequestMetadataUtil {

	private static final String LINES_OF_CODE = "loc";
	private static final String TECHNOLOGY = "technology";
	private static final String USE_CASE = "use_case";
	/* This value stored at this key is used to identify requests that are associated with the same action */
	private static final String UUID = "uuid";

	private RequestMetadataUtil() {
		/* Hide implicit constructor */
	}

	/**
	 * Builds a metadata map for an annotation.
	 *
	 * @param sourceCode the source code the annotation refers to
	 * @param technology the technology of the module the annotation refers to
	 * @return an unmodifiable map containing the metadata
	 */
	public static Map<String, String> getAnnotationMetadata(final String sourceCode, final Technology technology, final UUID uuid) {
		return Map.of(
				USE_CASE, "annotation_description",
				LINES_OF_CODE, String.valueOf(sourceCode.lines().count()),
				TECHNOLOGY, technology.name(),
				UUID, uuid.toString());
	}

	/**
	 * Builds a metadata map for a module.
	 *
	 * @param module the module
	 * @return an unmodifiable map containing the metadata
	 */
	public static Map<String, String> getModuleMetadata(final ModulePojo module) {
		final Map<String, String> map = new HashMap<>();
		map.put(USE_CASE, "module_description");
		final Optional<SourceMetricsPojo> oSourceMetrics = module.getSourceMetrics();
		if (oSourceMetrics.isPresent()) {
			final SourceMetricsPojo sourceMetrics = oSourceMetrics.get();
			map.put(LINES_OF_CODE, String.valueOf(sourceMetrics.getPhysicalLines()));
		}
		map.put(TECHNOLOGY, module.getTechnology().name());
		return Collections.unmodifiableMap(map);
	}

	/**
	 * Builds a metadata map for a reachability block.
	 *
	 * @param modulesWithDescription    the modules in the block that have a description
	 * @param modulesWithoutDescription the modules in the block that do not have a description
	 * @return an unmodifiable map containing the metadata
	 */
	public static Map<String, String> getReachabilityBlockMetadata(final List<ModulePojo> modulesWithDescription,
																   final List<ModulePojo> modulesWithoutDescription) {
		return Map.of(
				USE_CASE, "reachability_block_description",
				"num_of_modules", String.valueOf(modulesWithDescription.size() + modulesWithoutDescription.size()),
				"num_of_modules_with_description", String.valueOf(modulesWithDescription.size()),
				"length_of_module_descriptions", String.valueOf(
						modulesWithDescription.stream()
								.mapToInt(m -> m.getDescription().orElse("").length())
								.sum()));
	}

	/**
	 * Builds a metadata map for a functional block.
	 *
	 * @param annotations the annotations in the block
	 * @return an unmodifiable map containing the metadata for a functional block
	 */
	public static Map<String, String> getFunctionalBlockMetadata(final List<AnnotationPojo> annotations) {
		final int annotationDescriptionsLength = annotations.stream()
				.mapToInt(annotation -> annotation.getName().length())
				.sum();
		return Map.of(
				USE_CASE, "functional_block_description",
				"num_of_annotations", String.valueOf(annotations.size()),
				"length_of_annotation_descriptions", String.valueOf(annotationDescriptionsLength));
	}

	/**
	 * @return metadata for a similarity search.
	 */
	public static Map<String, String> getSimilaritySearchMetadata() {
		return Map.of(USE_CASE, "similarity_search");
	}

	/**
	 * Builds a metadata map for a knowledge service request.
	 *
	 * @return an unmodifiable map containing the metadata for a knowledge service request
	 */
	public static Map<String, String> getKnowledgeQueryMetadata(final AnnotationPojoPrototype annotation, final int numOfDataFields, final UUID uuid) {
		final Map<String, String> map = new HashMap<>();
		map.put(USE_CASE, "knowledge_query");
		map.put("num_of_data_fields", String.valueOf(numOfDataFields));
		map.put(UUID, uuid.toString());
		if (annotation.sourceAttachment.isPresent()) {
			final long lines = annotation.sourceAttachment.get().toString().lines().count();
			map.put(LINES_OF_CODE, String.valueOf(lines));
		}
		return Collections.unmodifiableMap(map);
	}
}
