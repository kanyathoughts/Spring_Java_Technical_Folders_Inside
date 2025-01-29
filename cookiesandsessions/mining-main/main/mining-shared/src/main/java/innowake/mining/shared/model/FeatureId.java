/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.model;

/**
 * Feature ID used by {@link Feature}.
 */
public enum FeatureId {

	// !!!! IMPORTANT !!!!
	// Every feature flag must be documented at https://amiconfluence.deloitte.com/x/NwInE
	// !!!! IMPORTANT !!!!

	ECLIPSE_DEEP_LINK("eclipseDeepLink"),
	INCREMENTAL_SCAN("incrementalScan"),
	PL1_EXPERIMENTAL("pl1Experimental"),
	JAVA_COLLECT_METHOD_CALLS("javaCollectMethodCalls"),
	COLLECT_PARSER_ERRORS("collectParserErrors"),
	DEPENDENCY_GRAPH_EXPLORE("dependencyGraphExplore"),
	LEGACY_WEB_VIEWS_IN_ECLIPSE("legacyWebViewsInEclipse"),
	GENERATIVE_AI_TRANSLATIONS("generativeAiTranslations"),
	CODE_VIEWER_HYPERLINKING("codeViewerHyperlinking"),
	CODE_VIEWER_ASSEMBLED_VIEW("codeViewerAssembledView"),
	CONSIDER_DATA_FLOW("considerDataFlow"),
	ORDERED_ANNOTATION_RULE_CSV_EXPORTER("orderedAnnotationRuleCsvExporter"),
	FUNCTIONAL_BLOCK_UI_PRODUCT_VISION("functionalBlockUiProductVision"),
	DISABLE_ANNOTATION_DATA_DICTIONARY_MANUAL_LINKING("disableAnnotationDataDictionaryManualLinking"),
	IDENTIFY_DDE_ONLY("identifyOnlyDDE"),
	SEMANTIC_SEARCH("semanticSearch"),
	GENERATE_FUNCTIONAL_BLOCK_AS_PER_MODULE_STRUCTURE("generateFunctionalBlockAsPerModuleStructure"),
	DETAILED_TAXONOMY_GRAPHML_EXPORT("detailedTaxonomyGraphmlExport"),
	REACHABILITY_RESOURCE_NETWORK("reachabilityResourceNetwork");

	// !!!! IMPORTANT !!!!
	// Every feature flag must be documented at https://amiconfluence.deloitte.com/x/NwInE
	// !!!! IMPORTANT !!!!

	private final String id;

	private FeatureId(final String id) {
		this.id = id;
	}

	public String getId() {
		return id;
	}

	/**
	 * Returns a {@code FeatureId} for a {@link String}.
	 *
	 * @param id the feature id as string
	 * @return the {@code FeatureId}
	 */
	public static FeatureId fromId(final String id) {
		for (final FeatureId featureId : values()) {
			if (featureId.id.equalsIgnoreCase(id)) {
				return featureId;
			}
		}
		throw new IllegalArgumentException(String.format("FeatureId for ID %s not found.", id));
	}
}
