/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.configuration;

/**
 * Provides generic application configuration properties. Set in application.yml file.
 */
public interface GenericConfiguration {

	/**
	 * @return the amount of threads to use for discover code DB operations
	 */
	int getDiscoverCodeDbThreads();

	/**
	 * @return the amount of threads to use for discovery metrics DB operations
	 */
	int getDiscoverMetricsDbThreads();

	/**
	 * @return the amount of retries for a database statement in case it failed due to concurrent modifications caused by orientDb optimistic locking
	 */
	int getMaxStatementRetries();

	/**
	 * @return the threshold value above which sorting will be disabled on discovery exported excel and csv worksheet entries
	 */
	int getDiscoveryExportSortThreshold();

	/**
	 * @return the maximum number of entries in the Discovery parse result cache
	 */
	int getDiscoveryParseResultCacheSize();

	/**
	 * @return the maximum number of entries in the Discovery lightweight parse result cache used source object dependency calculation
	 */
	int getDiscoveryLightweightParseResultCacheSize();

	/**
	 * @return the maximum number of entries in the Discovery module repository cache used in clustered mode
	 */
	int getDiscoveryModuleRepositoryCacheSize();

	/**
	 * @return the maximum number of entries in the Discovery source object cache
	 */
	int getDiscoverySourceObjectCacheSize();

	/**
	 * @return the maximum number of bytes in the Discovery source code cache
	 */
	long getDiscoverySourceCodeCacheSize();

	/**
	 * @return {@code true} uses strong keys and values in caches, {@code false} weak keys and soft values
	 */
	boolean isDiscoveryCacheStrongReferences();

	/**
	 * @return {@code true} uses the restart file discoverMetricsRestartFile in jobResults folder to restart from the state stored by the latest discovery run
	 */
	boolean isDiscoveryRestartEnabled();

	/**
	 * @return the maximum number of export tasks to be used for computing call chains. A value lesser than 1 means use all available processors. Default is {@code 8}.
	 */
	int getCallChainMaximumExportThreads();

	/**
	 * @return the maximum number of CSV lines that can be exported by each single call chain job. Default is {@code 5,000,000}.
	 */
	int getCallChainMaximumCsvExportLines();

	/**
	 * @return the maximum number of uncompressed bytes for the Discovery source upload.
	 */
	long getDiscoverySourceUploadMaximumSize();

	/**
	 * @return the maximum compression ratio of the source upload
	 */
	double getDiscoverySourceUploadMaximumRatio();

	/**
	 * @return the maximum entries allowed for discovery source upload
	 */
	int getDiscoverySourceUploadMaximumEntries();

	/**
	 * @return the name of the to be used optimized edge reference persisting implementation.
	 */
	String getEdgeReferenceStorageName();

	/**
	 * @return the partition size to split the DNA string list for DNA similarity computation
	 */
	int getDiscoveryDnaSimilarityPartitionSize();
	
	/**
	 * @return {@code true} persists minimal/required records during DNA computation.
	 */
	boolean isDiscoveryDnaOptimalPersist();
	
	/**
	 * @return the maximum number of entries in the Mining aggregation cache
	 */
	int getMiningAggregationCacheSize();
	
	/**
	 * @return the maximum number of entries in the Mining hotspot cache
	 */
	int getMiningHotspotCacheSize();
	
	/**
	 * @return the maximum number of entries in the Mining module statistics cache
	 */
	int getMiningModuleStatisticsCacheSize();
	
	/**
	 * @return the maximum number of entries in the Discovery model dna cache
	 */
	int getDiscoveryModelDnaCacheSize();
	
	/**
	 * @return the maximum number of entries in the Mining Taxonomy aggregation cache
	 */
	int getMiningTaxonomyAggregationCacheSize();
	
	/**
	 * @return the maximum number of entries in the Mining Taxonomy Category aggregation cache
	 */
	int getMiningTaxonomyCategoryAggregationCacheSize();

	/**
	 * @return the maximum number of seconds after which the keycloak user name cache is invalidated
	 */
	int getKeycloakUserNameCacheDuration();
	
	/**
	 * Returns the temporary folder that is used by jobs if temporary data needs to be written into the file system of the {@code mining-api-server}.<br><br>
	 * The default folder is: {@code System.getProperty("java.io.tmpdir")/discovery}.
	 *
	 * @return The temporary folder; not {@code null}
	 */
	default String getTemporaryFolder() {
		return System.getProperty("java.io.tmpdir") + "/discovery";
	}

	/**
	 * @return the maximum number of entries in the Mining hotspot cache
	 */
	int getSavedSearchChacheSize();

	/**
	 * @return the maximum number of entries in the Mining utility aggregation cache
	 */
	int getMiningUtilityAggregationCacheSize();

	/**
	 * @return the maximum number of entries in the Prompt cache
	 */
	long getPromptCacheSize();

	/**
	 * @return the time in seconds after which the Prompt cache is invalidated
	 */
	long getPromptCacheDuration();

	/**
	 * @return the maximum number of entries in the generic cache
	 */
	int getGenericMiningCacheSize();
	
	/**
	 * @return the maximum allowed ZIP size for Metadata in MB
	 */
	Long getMetadataMaxZipSizeinMB();
	
}
