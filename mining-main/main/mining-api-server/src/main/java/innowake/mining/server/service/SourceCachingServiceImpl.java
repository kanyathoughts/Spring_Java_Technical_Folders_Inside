/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import java.util.Collection;
import java.util.Objects;

import javax.persistence.EntityNotFoundException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.entities.SourcePojo;

/**
 * Service for caching certain source object requests.
 * This may be used instead of the main {@link SourceService} for specific queries where caching is desired.
 */
@Service
public class SourceCachingServiceImpl extends SourceServiceImpl implements SourceCachingService {
	
	/*
	 * TODO
	 * 
	 * The previous caching approach has various pitfalls due to improper invalidation leading to stale entries.
	 * Its interface is also still based NIDs instead of UIDs.
	 * Instead of a global cache (which is still node specific) there should be process (e.g. job run) specific cache instances
	 * which allow a clean start for each new process and get invalidated as the process ends.
	 * 
	 * Below changes temporarily remove the caching functionality altogether.
	 * As the original caching was implemented on OrientDB, the actual performance impact with Postgres, should be reevaluated
	 * before a new caching approach gets implemented.
	 */
	
	@Autowired
	public SourceCachingServiceImpl(@Qualifier("postgres") final JdbcTemplate jdbcTemplate) {
		super(jdbcTemplate);
	}

	/**
	 * Resets the cached content of all source objects.
	 */
	@Override
	public void resetCaches() {
		// VOID
	}

	@Override
	public void invalidateCaches(final Collection<EntityId> sourceIds) {
		// VOID
	}

	@Override
	public SourcePojo cachingByProjectPath(final Long projectId, final String path) {
		return findOne(q -> q.ofProject(EntityId.of(projectId)).withPath(path))
				.orElseThrow(() -> new EntityNotFoundException("Source with path '" + path + "' in Project " + projectId));
	}
	
	@Override
	public Collection<SourcePojo> cachingByProjectPath(final Collection<Tuple2<Long, String>> sourcesByProjectAndPath) {
		return find(q -> q.ofProjectNIDsWithPaths(Objects.requireNonNull(sourcesByProjectAndPath), k -> k.a, k -> k.b));
	}
	
}
