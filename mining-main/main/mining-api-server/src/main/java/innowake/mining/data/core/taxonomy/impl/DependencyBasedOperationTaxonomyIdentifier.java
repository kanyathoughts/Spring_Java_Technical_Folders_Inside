/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.data.core.taxonomy.impl;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.core.taxonomy.api.DependencyEdge;
import innowake.mining.data.core.taxonomy.api.DependencyModule;
import innowake.mining.data.core.taxonomy.api.TaxonomyIdentifier;
import innowake.mining.server.discovery.cache.DiscoveryJobCache;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.discovery.Tuple2;
import innowake.mining.shared.discovery.config.utility.UtilityList;
import innowake.mining.shared.model.TechnicalTaxonomies;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.ThreadContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

/**
 * Taxonomy identifier that identifies taxonomies based on utility and file invocations.
 */
@Component
public class DependencyBasedOperationTaxonomyIdentifier implements TaxonomyIdentifier<DependencyModule> {

	private static final String CACHE_KEY = "projectUtilityList";
	private static final Tuple2<TechnicalTaxonomies.Name, TechnicalTaxonomies.TypeName> FILE_TAXONOMY =
			TechnicalTaxonomies.tuple(TechnicalTaxonomies.Name.ACCESS_UNIX_FILES, TechnicalTaxonomies.TypeName.OPERATIONS);
	private static final Logger LOG = LoggerFactory.getLogger(DependencyBasedOperationTaxonomyIdentifier.class);

	private final ProjectService projectService;
	private final DiscoveryJobCache discoveryJobCache;

	@Autowired
	public DependencyBasedOperationTaxonomyIdentifier(final ProjectService projectService, final DiscoveryJobCache discoveryJobCache) {
		this.projectService = projectService;
		this.discoveryJobCache = discoveryJobCache;
	}

	@Override
	public List<Tuple2<TechnicalTaxonomies.Name, TechnicalTaxonomies.TypeName>> identify(final DependencyModule dependencyModule) {
		final List<Tuple2<TechnicalTaxonomies.Name, TechnicalTaxonomies.TypeName>> result = new ArrayList<>();
		final var project = dependencyModule.getProjectId();

		dependencyModule.getOutgoings().stream()
				.map(DependencyEdge::getOutgoingModule)
				.filter(outgoing -> outgoing.getType() == Type.FILE)
				.filter(outgoing -> outgoing.getTechnology() == Technology.RESOURCE)
				.filter(outgoing -> outgoing.getName().contains("/"))
				.findAny()
				.ifPresent(file -> result.add(FILE_TAXONOMY));

		final var jobId = StringUtils.trimToEmpty(ThreadContext.get("job-id"));
		dependencyModule.getOutgoings().stream()
				.map(DependencyEdge::getOutgoingModule)
				.filter(outgoing -> outgoing.getType() == Type.UTILITY || outgoing.getType() == Type.PROC)
				.findAny()
				.ifPresent(utility -> {
					try {
						final UtilityList utilityList = (UtilityList) discoveryJobCache.computeValueIfAbsent(jobId, CACHE_KEY,
								() -> UtilityList.loadUtilityList(projectService, project));
						final var utilityName = utility.getName();
						utilityList.findUtility(utilityName).ifPresent(utilityEntity ->
								utilityEntity.getTaxonomies().forEach(taxonomy -> {
									final var name = TechnicalTaxonomies.Name.fromName(taxonomy.getTaxonomy());
									final var typeName = TechnicalTaxonomies.TypeName.fromName(taxonomy.getType());

									if (name != TechnicalTaxonomies.Name.UNKNOWN && typeName != TechnicalTaxonomies.TypeName.UNKNOWN) {
										result.add(new Tuple2<>(name, typeName));
									} else if (name == TechnicalTaxonomies.Name.UNKNOWN) {
										throw new IllegalStateException(
												"Encountered Unknown taxonomy name " + taxonomy.getTaxonomy() + " for utility " + utilityName);
									} else {
										throw new IllegalStateException(
												"Encountered Unknown taxonomy type " + taxonomy.getType() + " for utility " + utilityName);
									}
								}));
					} catch (final Exception e) {
						LOG.warn("Technical taxonomy identification based on utility name is impacted on project " + project, e);
					}
				});

		return result;
	}
}
