/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

import java.util.Collection;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.shared.entities.SourcePojo;

/**
 * Specifies functions for caching and persisting source code data.
 */
public interface SourceCachingService extends SourceService {
	
	void invalidateCaches(Collection<EntityId> sourceIds);
	void resetCaches();

	SourcePojo cachingByProjectPath(Long projectId, String path);
	Collection<SourcePojo> cachingByProjectPath(Collection<Tuple2<Long, String>> sourcesByProjectAndPath);
	
}
