/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dawn.metrics.impl.cache;

import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.cache.CacheManager;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@EnableCaching
public class DawnCacheConfiguration {

	public static final String FIND_MODULES_CACHE = "findModules";
	public static final String FETCH_MODULES_CACHE = "fetchModules";
	public static final String FETCH_MODULES_LIGHTWEIGHT_CACHE = "fetchModulesLightweight";

	@Bean
	@Qualifier("discoveryCacheManager")
	public CacheManager discoveryCacheManager() {
		return new GuavaCacheManager();
	}
}
