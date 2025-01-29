/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery;

import static innowake.mining.shared.PatternConverter.convertAntToRegexPattern;

import java.util.List;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.shared.access.SourceService;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.entities.SourcePojo;

/**
 * A Class implementation for resolving the dependencies between the source objects.
 */
public class PersistingSourceObjectResolver implements SourceObjectResolver {

	private final SourceService sourceService;
	private final SearchOrders searchOrders;

	private static final Logger LOGGER = LoggerFactory.getLogger(Logging.OBJECT_RESOLVING);

	public PersistingSourceObjectResolver(final SourceService sourceService, final SearchOrders searchOrders) {
		this.sourceService = sourceService;
		this.searchOrders = searchOrders;
	}

	@Override
	@Nullable
	public SourcePojo resolveObject(final SourcePojo context, final String targetName) {
		return resolveObjectInternal(context, targetName, null);
	}

	@Override
	@Nullable
	public SourcePojo resolveObject(final SourcePojo context, final String targetName, final SourceObjectMatcher targetMatcher) {
		return resolveObjectInternal(context, targetName, targetMatcher);
	}

	@Nullable
	private SourcePojo resolveObjectInternal(final SourcePojo context, final String targetName, final @Nullable SourceObjectMatcher targetMatcher) {
		SourcePojo targetSourceObject = null;

		try {
			targetSourceObject = sourceService.get(q -> {
				q.withReferenceFrom(context.identity());
				q.withName(targetName);
				if (targetMatcher != null) {
					q.withTechnology(targetMatcher.getTechnology());
					q.withType(targetMatcher.getTypes());
				}
			});
		} catch (final MiningEntityNotFoundException e) {
			LOGGER.debug("Target source object is not found, Name: {}, Target: {} ",context.getName(), targetName);
		}

		if (targetSourceObject != null) {
			LOGGER.debug("Target source object resolved, Name: {}, Path: {} ",targetSourceObject.getName(), targetSourceObject.getPath());
			return targetSourceObject;
		}

		final String[] targetPatterns = searchOrders.resolvePattern(context.getPath());
		int count =  0;

		for (String targetPattern: targetPatterns) {
			count++;

			/* Update target pattern if object has to be resolved is in same folder */
			if (targetPattern.startsWith("./")) {
				targetPattern = FilenameUtils.getPath(context.getPath()) + targetPattern.substring(2);
			}

			final String targetPatternRegex = convertAntToRegexPattern(targetPattern);
			if (StringUtils.isNotBlank(targetPattern)) {
				final List<SourcePojo> targetSourceObjects = sourceService.find(q -> {
					q.ofProject(context.getProject())
						.withPathRegex(targetPatternRegex)
						.withName(targetName);
					if (targetMatcher != null) {
						q.withTechnology(targetMatcher.getTechnology());
						if ( ! targetMatcher.getTypes().isEmpty()) {
							q.withType(targetMatcher.getTypes());
						}
					}
				});

				if (targetSourceObjects.isEmpty()) {

					/* Check for last item in list */
					if (count == targetPatterns.length) {
						LOGGER.debug("Target source object was not found, Name: {}", targetName);
						return null;
					}
					continue;
				} else {
					targetSourceObject = targetSourceObjects.get(0);
				}

				if (targetSourceObjects.size() > 1) {
					final StringBuilder log = new StringBuilder()
							.append("[name: ").append(context.getName()).append(", path: ").append(context.getPath()).append("] ")
							.append("Found multiple target objects:");

					targetSourceObjects.forEach(
						sourceObject -> log.append(" [name: ").append(sourceObject.getName()).append(", path: ").append(sourceObject.getPath()).append("]"));
					log.append(". Reference will be created for first target object in the list");
					LOGGER.warn(log.toString());
				}

				sourceService.putReference(context.identity(), targetSourceObject.identity());
				LOGGER.debug("Reference created between source object {} and target object {} ", context.getName(), targetSourceObjects.get(0).getName());
				return targetSourceObject;
			}
		}

		return targetSourceObject;
	}
}
