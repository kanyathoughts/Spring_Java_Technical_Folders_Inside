/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.natural;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.UncategorizedSQLException;
import org.springframework.stereotype.Component;

import innowake.lib.core.log.Logger;
import innowake.lib.job.api.Job;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.discovery.cache.DiscoveryJobCache;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.ndt.core.parsing.IAst;
import innowake.ndt.core.parsing.IAstNode;
import innowake.ndt.core.parsing.natural.INaturalParseResult;
import innowake.ndt.core.parsing.natural.NaturalLightweightParsing;
import innowake.ndt.core.parsing.natural.dependency.DependencyNNodeType;
import innowake.ndt.core.parsing.natural.dependency.DependencyNStatementNode;
import innowake.ndt.core.parsing.natural.dependency.DependencyNaturalTokenParser;

/**
 * Creates and provides {@link SourcePojo} names for alias names.
 */
@Component
public class NaturalSourceObjectAliasManager {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.OBJECT_RESOLVING);
	private static final String JOB_CACHE_ALIAS_KEY = "source-object-alias_";
	private static final String JOB_CACHE_REPLACEMENT_KEY = "source-object-replacement_";
	private static final String PLACEHOLDER = "&";

	@Autowired
	private transient DiscoveryJobCache jobCache;
	@Autowired
	private transient SourceService sourceService;

	/**
	 * Parses and caches alias names for {@link SourcePojo}s which can be used by incoming references.
	 *
	 * @param jobId the {@link Job} ID
	 * @param projectId the project ID
	 * @param config the {@link Config}
	 */
	public void init(final String jobId, final EntityId projectId, final Config config) {
		final List<SourcePojo> subroutines;
		try {
			subroutines = sourceService.find(q -> q.ofProject(projectId).withTechnology(Technology.NATURAL).withType(Type.SUBROUTINE));
		} catch (MiningEntityNotFoundException | UncategorizedSQLException e) {
			LOG.error(() -> "Error while parsing aliases of external subroutines for project " + projectId, e);
			return;
		}

		subroutines.forEach(sourceObject -> {
			final INaturalParseResult parseResult = NaturalLightweightParsing.computeNaturalTokens(sourceObject.getContent().toString());
			final IAst<Object> ast = DependencyNaturalTokenParser.INSTANCE.parse(parseResult.getTokenPartitioning());
			/* A Natural external subroutine can contain only one main subroutine and several internal subroutines which cannot be called from outside
			 * getFirstNode() returns the outer most subroutine definition */
			final IAstNode subroutine = ast.getFirstNode(DependencyNNodeType.DEFINE_SUBROUTINE);
			if (subroutine != null) {
				final String name = normalize(((DependencyNStatementNode) subroutine).getArgument());
				if ( ! name.equals(sourceObject.getName())) {
					LOG.debug(() -> String.format("Found alias '%s' for external subroutine '%s' in project %s.", name, sourceObject.getPath(), projectId));
					jobCache.putMultiValue(jobId, JOB_CACHE_ALIAS_KEY + name, sourceObject.getName());
				}
			} else {
				LOG.error(() -> String.format("Error while parsing aliases of external subroutines for project %s. SourcePojo '%s' contains no subroutine.",
						projectId, sourceObject.getPath()));
			}
		});

		config.getNaturalLanguageCodes().stream().forEach(language -> jobCache.putMultiValue(jobId, JOB_CACHE_REPLACEMENT_KEY + PLACEHOLDER, language));
	}

	/**
	 * Returns all {@link SourcePojo} names for an {@code alias}.
	 *
	 * @param jobId the {@link Job} ID
	 * @param alias the alias of a {@link SourcePojo}
	 * @return all {@link SourcePojo} names for the given {@code alias}
	 */
	public Set<String> getSourceObjectNamesForAlias(final String jobId, final String alias) {
		if (alias.contains(PLACEHOLDER)) {
			return jobCache.getMultiValue(jobId, JOB_CACHE_REPLACEMENT_KEY + PLACEHOLDER)
					.stream()
					.map(Object::toString)
					.map(value -> StringUtils.replace(alias, PLACEHOLDER, value))
					.collect(Collectors.toSet());
		} else {
			return jobCache.getMultiValue(jobId, JOB_CACHE_ALIAS_KEY + normalize(alias))
					.stream()
					.map(Object::toString)
					.collect(Collectors.toSet());
		}
	}

	private static String normalize(final String alias) {
		return alias.trim().toLowerCase();
	}
}
