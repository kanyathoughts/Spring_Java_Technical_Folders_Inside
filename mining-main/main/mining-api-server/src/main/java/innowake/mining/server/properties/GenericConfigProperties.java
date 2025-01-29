/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.properties;

import innowake.mining.server.genai.ResponseFormat;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.ConstructorBinding;
import org.springframework.web.util.UriComponentsBuilder;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.job.api.config.properties.JobConfigurationProperties;
import innowake.mining.shared.configuration.GenericConfiguration;

import java.util.Optional;

/**
 * Bean for mapping generic configuration values from the Spring application configuration (application.yml).
 */
@ConfigurationProperties(prefix = "configuration")
public class GenericConfigProperties implements GenericConfiguration {

	private static final int DISCOVERY_EXPORT_SORT_THRESHOLD = 100_000;
	private static final int DISCOVERY_PARSE_RESULT_CACHE_SIZE = 10_000;
	private static final int DISCOVERY_LIGHTWEIGHT_PARSE_RESULT_CACHE_SIZE = 1_000;
	private static final int DISCOVERY_MODULE_REPOSITORY_CACHE_SIZE = 200_000;
	private static final int DISCOVERY_SOURCE_OBJECT_CACHE_SIZE = 100_000;
	private static final long DISCOVERY_SOURCE_CODE_CACHE_SIZE = 2147483648L;
	private static final boolean DISCOVERY_CACHE_STRONG_REFERENCES = false;
	private static final boolean DISCOVERY_RESTART_ENABLED = false;
	private static final int CALL_CHAIN_MAXIMUM_EXPORT_THREADS = 8;
	private static final int CALL_CHAIN_MAXIMUM_CSV_EXPORT_LINES = 5_000_000;
	private static final long DISCOVERY_SOURCE_UPLOAD_MAXIMUM_SIZE = 10_000_000_000L;
	private static final double DISCOVERY_SOURCE_UPLOAD_MAXIMUM_RATIO = 100;
	private static final int DISCOVERY_SOURCE_UPLOAD_MAXIMUM_ENTRIES = 1_000_000;
	private static final String EDGE_REFERENCE_STORAGE_NAME = "file-based";
	private static final int MINING_AGGREGATION_CACHE_SIZE = 10_000;
	private static final int MINING_HOTSPOT_CACHE_SIZE = 10_000;
	private static final int MINING_MODULE_STATISTICS_CACHE_SIZE = 10_000;
	private static final int DISCOVERY_MODEL_DNA_CACHE_SIZE = 10_000;
	private static final int MINING_TAXONOMY_AGGREGATION_CACHE_SIZE = 10_000;
	private static final int MINING_TAXONOMY_CATEGORY_AGGREGATION_CACHE_SIZE = 10_000;
	private static final int SAVED_SEARCH_CACHE_SIZE = 10_000;
	private static final int KEYCLOAK_USER_NAME_CACHE_DURATION = 1800;
	private static final int MINING_UTILITY_AGGREGATION_CACHE_SIZE = 10_000;
	private static final int GENERIC_MINING_CACHE_SIZE = 10_000;
	private static final long METADATA_MAX_ZIP_SIZE_IN_MB = 4_000;

	private static final String GEN_AI_URL_DEFAULT = "http://10.224.103.236:8083";
	private static final String GEN_AI_API_KEY_DEFAULT = "";
	private static final String GEN_AI_PLUGIN_DEFAULT = "azure gpt 3.5";
	private static final int GEN_AI_MAX_NEW_TOKEN_DEFAULT = 256;
	private static final double GEN_AI_TEMPERATURE_DEFAULT = 0.7;
	private static final String GEN_AI_MODULE_DESCRIPTION_DEFAULT = "/deduce";
	private static final String GEN_AI_ANNOTATION_DESCRIPTION_DEFAULT = "/custom_prompt";
	private static final ResponseFormat GEN_AI_RESPONSE_FORMAT_CUSTOM_PROMPT_DEFAULT = ResponseFormat.JSON;
	private static final String GEN_AI_ANNOTATION_KNOWLEDGE_CATEGORY_DEFAULT = "VARIABLE_DESCRIPTION";
	private final int discoverCodeDbThreads;
	private final int discoverMetricsDbThreads;
	private final int maxStatementRetries;
	private final String tmpFolder;
	private final int discoveryExportSortThreshold;
	private final int discoveryParseResultCacheSize;
	private final int discoveryLightweightParseResultCacheSize;
	private final int discoveryModuleRepositoryCacheSize;
	private final int discoverySourceObjectCacheSize;
	private final long discoverySourceCodeCacheSize;
	private final boolean discoveryCacheStrongReferences;
	private final boolean discoveryRestartEnabled;
	private final int callChainMaximumExportThreads;
	private final int callChainMaximumCsvExportLines;
	private final long discoverySourceUploadMaximumSize;
	private final double discoverySourceUploadMaximumRatio;
	private final int discoverySourceUploadMaxEntries;
	private final String edgeReferenceStorageName;
	private final int discoveryDnaSimilarityPartitionSize;
	private final boolean discoveryDnaOptimalPersist;
	private final int miningAggregationCacheSize;
	private final int miningHotspotCacheSize;
	private final int miningModuleStatisticsCacheSize;
	private final int discoveryModelDnaCacheSize;
	private final int savedSearchCacheSize;
	private final int miningTaxonomyAggregationCacheSize;
	private final int miningTaxonomyCategoryAggregationCacheSize;
	private final int keycloakUserNameCacheDuration;
	private final int miningUtilityAggregationCacheSize;
	private final int genericMiningCacheSize;
	@Nullable
	@Value("${mining.metadata.maxZipImportSizeInMB}")
	private Long metadataMaxZipSizeInMB;
	@Nullable
	@Value("${mining.genAI.url}")
	private String genAiURL;
	@Nullable
	@Value("${mining.genAI.apiKey}")
	private String genAiApiKey;
	@Nullable
	@Value("${mining.genAI.plugIn}")
	private String genAiPlugin;
	@Nullable
	@Value("${mining.genAI.maxNewTokens}")
	private Integer genAiMaxNewToken;
	@Nullable
	@Value("${mining.genAI.temperature}")
	private Double genAiTemperature;
	@Nullable
	@Value("${mining.genAI.doSample}")
	private String genAiDoSample;
	@Nullable
	@Value("${mining.genAI.tokenRateLimit}")
	private Integer miningGenAiTokenRateLimit;
	@Nullable
	@Value("${mining.genAI.responseFormat}")
	private ResponseFormat responseFormat;

	@Nullable
	@Value("${mining.genAI.pathSegment.moduleDescription}")
	private String genAiModuleDescription;
	@Nullable
	@Value("${mining.genAI.pathSegment.annotationDescription}")
	private String genAiAnnotationDescription;
	
	@Nullable
	@Value("${mining.genAI.semanticSearchUrl}")
	private String semanticSearchUrl;
	
	@Nullable
	@Value("${mining.genAI.retrieverTopK}")
	private Integer retrieverTopK;

	@Nullable
	@Value("${mining.genAI.maxParallelTasks}")
	private Integer maxParallelTasks;

	@Value("${mining.genAI.prompt-cache-size}")
	private long promptCacheSize;

	@Value("${mining.genAI.prompt-cache-duration}")
	private long promptCacheDuration;

	@Nullable
	@Value("${mining.genAI.promptServiceUrl}")
	private String promptServiceUrl;

	@Nullable
	@Value("${mining.genAI.annotationKnowledgeCategory}")
	private String genAiAnnotationKnowledgeCategory;

	/**
	 * Constructor.
	 *
	 * @param discoverCodeDbThreads the amount of threads to use for discover code DB operations
	 * @param discoverMetricsDbThreads the amount of threads to use for discovery metrics DB operations
	 * @param maxStatementRetries the amount of retries for a database statement in case it failed due to concurrent modifications
	 * caused by orientDb optimistic locking
	 * @param discoveryExportSortThreshold the threshold value above which sorting will be disabled in discovery export excel and csv worksheet entries
	 * @param tmpFolder the temporary folder for the job API, e.g. for exporting source files
	 * @param discoveryParseResultCacheSize maximum number of entries for the Discovery parse result cache
	 * @param discoveryLightweightParseResultCacheSize maximum number of entries for the Discovery lightweight parse result cache
	 * @param discoveryModuleRepositoryCacheSize maximum number of entries in the Discovery module repository cache
	 * @param discoverySourceObjectCacheSize maximum number of entries in the Discovery source object cache
	 * @param discoverySourceCodeCacheSize maximum number of bytes in the Discovery source code cache
	 * @param discoveryCacheStrongReferences {@code true} uses strong keys and values in caches, {@code false} weak keys and soft values
	 * @param discoveryRestartEnabled {@code true} uses the restart file discoverMetricsRestartFile in jobResults folder to restart from the state stored by the latest discovery run
	 * @param callChainMaximumExportThreads maximum number of export threads to be used for computing call chains. A value lesser than 1 means use all available processors. Default is {@code 8}
	 * @param callChainMaximumCsvExportLines maximum number of CSV lines that can be exported by each single call chain job. Default is {@code 5,000,000}.
	 * @param discoverySourceUploadMaximumSize maximum number of uncompressed bytes for the Discovery source upload. Default is {@code 10,000,000,000}.
	 * @param discoverySourceUploadMaximumRatio maximum compression ratio of the Discovery source upload. Default is {@code 100}.
	 * @param discoverySourceUploadMaximumEntries maximum entries for the Discovery source upload. Default is {@code 1,000,000}
	 * @param edgeReferenceStorageName name of the to be used optimized edge reference persisting implementation
	 * @param discoveryDnaSimilarityPartitionSize the partition size to split the DNA string list for DNA similarity computation
	 * @param discoveryDnaOptimalPersist {@code true} persists minimal/required records during DNA computation
	 * @param miningAggregationCacheSize maximum number of entries for the aggregation cache
	 * @param miningHotspotCacheSize maximum number of entries for the hotspot cache
	 * @param miningModuleStatisticsCacheSize maximum number of entries for the module statistics cache
	 * @param discoveryModelDnaCacheSize maximum number of entries for the model DNA cache
	 * @param savedSearchCacheSize maximum number of entries for the saved search count cache
	 * @param miningTaxonomyAggregationCacheSize maximum number of entries for the Taxonomy Aggregation Cache
	 * @param miningTaxonomyCategoryAggregationCacheSize maximum number of entries for the Taxonomy Category Aggregation Cache
	 * @param keycloakUserNameCacheDuration the duration in seconds for the Keycloak user name cache
	 * @param miningUtilityAggregationCacheSize maximum number of entries for the Utility Aggregation Cache
	 * @param genericMiningCacheSize maximum number of entries for the generic mining cache
	 */
	@ConstructorBinding
	public GenericConfigProperties(
			final int discoverCodeDbThreads,
			final int discoverMetricsDbThreads,
			final int maxStatementRetries,
			final int discoveryExportSortThreshold,
			@Nullable final String tmpFolder,
			@Nullable final Integer discoveryParseResultCacheSize,
			@Nullable final Integer discoveryLightweightParseResultCacheSize,
			@Nullable final Integer discoveryModuleRepositoryCacheSize,
			@Nullable final Integer discoverySourceObjectCacheSize,
			@Nullable final Long discoverySourceCodeCacheSize,
			@Nullable final Boolean discoveryCacheStrongReferences,
			@Nullable final Boolean discoveryRestartEnabled,
			@Nullable final Integer callChainMaximumExportThreads,
			final int callChainMaximumCsvExportLines,
			@Nullable final Long discoverySourceUploadMaximumSize,
			@Nullable final Double discoverySourceUploadMaximumRatio,
			@Nullable final Integer discoverySourceUploadMaximumEntries,
			@Nullable final String edgeReferenceStorageName,
			final int discoveryDnaSimilarityPartitionSize,
			final boolean discoveryDnaOptimalPersist,
			@Nullable final Integer miningAggregationCacheSize,
			@Nullable final Integer miningHotspotCacheSize,
			@Nullable final Integer miningModuleStatisticsCacheSize,
			@Nullable final Integer discoveryModelDnaCacheSize,
			@Nullable final Integer savedSearchCacheSize,
			@Nullable final Integer miningTaxonomyAggregationCacheSize,
			@Nullable final Integer miningTaxonomyCategoryAggregationCacheSize,
			@Nullable final Integer keycloakUserNameCacheDuration,
			@Nullable final Integer miningUtilityAggregationCacheSize,
			@Nullable final Integer genericMiningCacheSize) {

		this.discoverCodeDbThreads = discoverCodeDbThreads > 0 ? discoverCodeDbThreads : JobConfigurationProperties.PROCESSORS;
		this.discoverMetricsDbThreads = discoverMetricsDbThreads > 0 ? discoverMetricsDbThreads : JobConfigurationProperties.PROCESSORS;
		this.maxStatementRetries = maxStatementRetries > 0 ? maxStatementRetries : JobConfigurationProperties.PROCESSORS;
		this.discoveryExportSortThreshold = discoveryExportSortThreshold > 0 ? discoveryExportSortThreshold : DISCOVERY_EXPORT_SORT_THRESHOLD;
		this.tmpFolder = tmpFolder != null ? tmpFolder : GenericConfiguration.super.getTemporaryFolder();

		this.discoveryParseResultCacheSize = discoveryParseResultCacheSize != null ?
				discoveryParseResultCacheSize.intValue() : DISCOVERY_PARSE_RESULT_CACHE_SIZE;
		this.discoveryLightweightParseResultCacheSize = discoveryLightweightParseResultCacheSize != null ?
				discoveryLightweightParseResultCacheSize.intValue() : DISCOVERY_LIGHTWEIGHT_PARSE_RESULT_CACHE_SIZE;
		this.discoveryModuleRepositoryCacheSize = discoveryModuleRepositoryCacheSize != null ?
				discoveryModuleRepositoryCacheSize.intValue() : DISCOVERY_MODULE_REPOSITORY_CACHE_SIZE;
		this.discoverySourceObjectCacheSize = discoverySourceObjectCacheSize != null ?
				discoverySourceObjectCacheSize.intValue() : DISCOVERY_SOURCE_OBJECT_CACHE_SIZE;
		this.discoverySourceCodeCacheSize = discoverySourceCodeCacheSize != null ?
				discoverySourceCodeCacheSize.longValue() : DISCOVERY_SOURCE_CODE_CACHE_SIZE;
		this.discoveryCacheStrongReferences = discoveryCacheStrongReferences != null ?
				discoveryCacheStrongReferences.booleanValue() : DISCOVERY_CACHE_STRONG_REFERENCES;
		this.discoveryRestartEnabled = discoveryRestartEnabled != null ?
				discoveryRestartEnabled.booleanValue() : DISCOVERY_RESTART_ENABLED;
		this.callChainMaximumExportThreads = callChainMaximumExportThreads != null ?
				callChainMaximumExportThreads.intValue() : CALL_CHAIN_MAXIMUM_EXPORT_THREADS;
		this.callChainMaximumCsvExportLines = callChainMaximumCsvExportLines > 0 ? callChainMaximumCsvExportLines : CALL_CHAIN_MAXIMUM_CSV_EXPORT_LINES;
		this.discoverySourceUploadMaximumSize = discoverySourceUploadMaximumSize != null ? discoverySourceUploadMaximumSize.longValue() :
			DISCOVERY_SOURCE_UPLOAD_MAXIMUM_SIZE;
		this.discoverySourceUploadMaximumRatio = discoverySourceUploadMaximumRatio != null  ? discoverySourceUploadMaximumRatio.doubleValue() :
				DISCOVERY_SOURCE_UPLOAD_MAXIMUM_RATIO;
		this.discoverySourceUploadMaxEntries = discoverySourceUploadMaximumEntries != null ? discoverySourceUploadMaximumEntries.intValue() :
				DISCOVERY_SOURCE_UPLOAD_MAXIMUM_ENTRIES;
		this.edgeReferenceStorageName = edgeReferenceStorageName != null ? edgeReferenceStorageName : EDGE_REFERENCE_STORAGE_NAME;
		this.discoveryDnaSimilarityPartitionSize = discoveryDnaSimilarityPartitionSize;
		this.discoveryDnaOptimalPersist = discoveryDnaOptimalPersist;
		this.miningAggregationCacheSize = miningAggregationCacheSize != null ?
				miningAggregationCacheSize.intValue() : MINING_AGGREGATION_CACHE_SIZE;
		this.miningHotspotCacheSize = miningHotspotCacheSize != null ?
				miningHotspotCacheSize.intValue() : MINING_HOTSPOT_CACHE_SIZE;
		this.miningModuleStatisticsCacheSize = miningModuleStatisticsCacheSize != null ?
				miningModuleStatisticsCacheSize.intValue() : MINING_MODULE_STATISTICS_CACHE_SIZE;
		this.discoveryModelDnaCacheSize = discoveryModelDnaCacheSize != null ?
				discoveryModelDnaCacheSize.intValue() : DISCOVERY_MODEL_DNA_CACHE_SIZE;
		this.savedSearchCacheSize = savedSearchCacheSize != null ? savedSearchCacheSize.intValue(): SAVED_SEARCH_CACHE_SIZE;
		this.miningTaxonomyAggregationCacheSize = miningTaxonomyAggregationCacheSize != null ?
				miningTaxonomyAggregationCacheSize.intValue() : MINING_TAXONOMY_AGGREGATION_CACHE_SIZE;
		this.miningTaxonomyCategoryAggregationCacheSize = miningTaxonomyCategoryAggregationCacheSize != null ?
				miningTaxonomyCategoryAggregationCacheSize.intValue() : MINING_TAXONOMY_CATEGORY_AGGREGATION_CACHE_SIZE;
		this.keycloakUserNameCacheDuration = keycloakUserNameCacheDuration != null ? keycloakUserNameCacheDuration : KEYCLOAK_USER_NAME_CACHE_DURATION;
		this.miningUtilityAggregationCacheSize = miningUtilityAggregationCacheSize != null ?
				miningUtilityAggregationCacheSize : MINING_UTILITY_AGGREGATION_CACHE_SIZE;
		this.genericMiningCacheSize = genericMiningCacheSize != null ? genericMiningCacheSize : GENERIC_MINING_CACHE_SIZE;
	}

	@Override
	public int getDiscoverCodeDbThreads() {
		return discoverCodeDbThreads;
	}

	@Override
	public int getDiscoverMetricsDbThreads() {
		return discoverMetricsDbThreads;
	}

	@Override
	public int getMaxStatementRetries() {
		return maxStatementRetries;
	}

	@Override
	public String getTemporaryFolder() {
		return tmpFolder;
	}

	@Override
	public int getDiscoveryExportSortThreshold() {
		return discoveryExportSortThreshold;
	}

	@Override
	public int getDiscoveryParseResultCacheSize() {
		return discoveryParseResultCacheSize;
	}

	@Override
	public int getDiscoveryLightweightParseResultCacheSize() {
		return discoveryLightweightParseResultCacheSize;
	}

	@Override
	public int getDiscoveryModuleRepositoryCacheSize() {
		return discoveryModuleRepositoryCacheSize;
	}

	@Override
	public int getDiscoverySourceObjectCacheSize() {
		return discoverySourceObjectCacheSize;
	}

	@Override
	public long getDiscoverySourceCodeCacheSize() {
		return discoverySourceCodeCacheSize;
	}

	@Override
	public boolean isDiscoveryCacheStrongReferences() {
		return discoveryCacheStrongReferences;
	}

	@Override
	public boolean isDiscoveryRestartEnabled() {
		return discoveryRestartEnabled;
	}

	@Override
	public int getCallChainMaximumExportThreads() {
		return callChainMaximumExportThreads;
	}

	@Override
	public int getCallChainMaximumCsvExportLines() {
		return callChainMaximumCsvExportLines;
	}

	@Override
	public long getDiscoverySourceUploadMaximumSize() {
		return discoverySourceUploadMaximumSize;
	}

	@Override
	public double getDiscoverySourceUploadMaximumRatio() {
		return discoverySourceUploadMaximumRatio;
	}

	@Override
	public int getDiscoverySourceUploadMaximumEntries() {
		return discoverySourceUploadMaxEntries;
	}

	@Override
	public String getEdgeReferenceStorageName() {
		return edgeReferenceStorageName;
	}

	@Override
	public int getDiscoveryDnaSimilarityPartitionSize() {
		return discoveryDnaSimilarityPartitionSize;
	}

	@Override
	public boolean isDiscoveryDnaOptimalPersist() {
		return discoveryDnaOptimalPersist;
	}

	@Nullable
	public String getGenAiURL() {
		return StringUtils.isNotBlank(genAiURL) ? genAiURL : GEN_AI_URL_DEFAULT;
	}
	
	@Nullable
	public String getResolvedGenAiURL() {
		return getResolvedURL(getGenAiURL());
	}

	@Nullable
	public String getGenAiApiKey() {
		return StringUtils.isNotBlank(genAiApiKey) ? genAiApiKey : GEN_AI_API_KEY_DEFAULT;
	}

	@Nullable
	public String getGenAiPlugin() {
		return StringUtils.isNotBlank(genAiPlugin) ? genAiPlugin : GEN_AI_PLUGIN_DEFAULT;
	}

	@Nullable
	public String getGenAiAnnotationKnowledgeCategory() {
		return StringUtils.isNotBlank(genAiAnnotationKnowledgeCategory) ? genAiAnnotationKnowledgeCategory : GEN_AI_ANNOTATION_KNOWLEDGE_CATEGORY_DEFAULT;
	}
	
	@Nullable
	public String getResolvedSemanticSearchURL() {
		final var url = StringUtils.isNotBlank(semanticSearchUrl) ? semanticSearchUrl : getGenAiURL();
		return getResolvedURL(url);
	}

	@Nullable
	public String getResolvedPromptServiceURL() {
		final var url = StringUtils.isNotBlank(promptServiceUrl) ? promptServiceUrl : getGenAiURL();
		return getResolvedURL(url);
	}
	
	@Nullable
	private String getResolvedURL(@Nullable final String url) {
		if (url != null && ! url.isBlank() && ! (url.startsWith("http://") || url.startsWith("https://"))) {
			return UriComponentsBuilder.fromPath(url).toUriString();
		}
		return url;
	}

	@Override
	public int getMiningAggregationCacheSize() {
		return miningAggregationCacheSize;
	}

	@Override
	public int getMiningHotspotCacheSize() {
		return miningHotspotCacheSize;
	}

	@Override
	public int getSavedSearchChacheSize() {
		return savedSearchCacheSize;
	}

	@Override
	public int getMiningModuleStatisticsCacheSize() {
		return miningModuleStatisticsCacheSize;
	}

	@Override
	public int getMiningTaxonomyAggregationCacheSize() {
		return miningTaxonomyAggregationCacheSize;
	}

	@Override
	public int getMiningTaxonomyCategoryAggregationCacheSize() {
		return miningTaxonomyCategoryAggregationCacheSize;
	}

	@Override
	public int getKeycloakUserNameCacheDuration() {
		return keycloakUserNameCacheDuration;
	}

	@Override
	public int getDiscoveryModelDnaCacheSize() {
		return discoveryModelDnaCacheSize;
	}

	@Override
	public int getMiningUtilityAggregationCacheSize() {
		return miningUtilityAggregationCacheSize;
	}

	@Override
	public long getPromptCacheSize() {
		return promptCacheSize;
	}

	@Override
	public long getPromptCacheDuration() {
		return promptCacheDuration;
	}

	@Override
	public int getGenericMiningCacheSize() {
		return genericMiningCacheSize;
	}

	public int getGenAiMaxNewToken() {
		return genAiMaxNewToken != null ? genAiMaxNewToken : GEN_AI_MAX_NEW_TOKEN_DEFAULT;
	}

	public double getGenAiTemperature() {
		return genAiTemperature != null ? genAiTemperature : GEN_AI_TEMPERATURE_DEFAULT;
	}

	public boolean getGenAiDoSample() {
		return StringUtils.isBlank(genAiPlugin) ? true: Boolean.parseBoolean(genAiDoSample);
	}

	public ResponseFormat getResponseFormat() {
		return Optional.ofNullable(responseFormat).orElse(GEN_AI_RESPONSE_FORMAT_CUSTOM_PROMPT_DEFAULT);
	}

	@Nullable
	public Integer getRetrieverTopK() {
		return retrieverTopK;
	}

	public Optional<Integer> getMiningGenAiTokenRateLimit() {
		return Optional.ofNullable(miningGenAiTokenRateLimit);
	}

	public Optional<Integer> getMaxParallelTasks() {
		return Optional.ofNullable(maxParallelTasks);
	}

	/**
	 * Returns the value for mining.genAI.pathSegment.moduleDescription in the application.yml
	 *
	 * @return Returns the value in mining.genAI.pathSegment.moduleDescription
	 */
	public String getGenAiModuleDescription() {
		final String genAiModuleDesc = genAiModuleDescription;
		if (genAiModuleDesc != null && ! StringUtils.isBlank(genAiModuleDesc)) {
			return genAiModuleDesc;
		}
		return GEN_AI_MODULE_DESCRIPTION_DEFAULT;
	}

	/**
	 * Returns the value for mining.genAI.pathSegment.annotationDescription in the application.yml
	 *
	 * @return Returns the value in mining.genAI.pathSegment.annotationDescription
	 */
	public String getGenAiAnnotationDescription() {
		final String genAiAnnotationDesc = genAiAnnotationDescription;
		if (genAiAnnotationDesc != null && ! StringUtils.isBlank(genAiAnnotationDesc)) {
			return genAiAnnotationDesc;
		}
		return GEN_AI_ANNOTATION_DESCRIPTION_DEFAULT;
	}

	@Override
	public Long getMetadataMaxZipSizeinMB() {
		return metadataMaxZipSizeInMB != null ? metadataMaxZipSizeInMB : METADATA_MAX_ZIP_SIZE_IN_MB;
	}

	/**
	 * Returns the default response format of the desired LLM response against the /custom_prompt endpoint.
	 *
	 * @return the default response format of the desired LLM response against the /custom_prompt endpoint
	 */
	public ResponseFormat getDefaultResponseFormatCustomPrompt() {
		return GEN_AI_RESPONSE_FORMAT_CUSTOM_PROMPT_DEFAULT;
	}
}
