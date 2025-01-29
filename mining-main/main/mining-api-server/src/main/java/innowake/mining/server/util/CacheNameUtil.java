/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.util;

import innowake.mining.shared.access.EntityId;

/**
 * Utility class for filling in cache name for project specific caches
 */
public class CacheNameUtil {

	private static final String CACHE_ARGS_TEMPLATE = "(%s)";
	private static final String PROJECT_ID_TEMPLATE = "projectId=%d";
	
	/**
	 * Creates project specific cache name
	 *
	 * @param cacheName the name of cache
	 * @param projectId the project Id for the cache
	 * @return project specific cache name
	 */
	public static String getProjectCacheName(final String cacheName, final EntityId projectId) {	
		return cacheName + String.format(CACHE_ARGS_TEMPLATE, String.format(PROJECT_ID_TEMPLATE, projectId.getNid()));
	}
	
}
