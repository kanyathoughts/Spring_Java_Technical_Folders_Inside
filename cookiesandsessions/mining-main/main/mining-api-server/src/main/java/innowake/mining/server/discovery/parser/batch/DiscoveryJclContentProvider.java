/* Copyright (c) 2018 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.parser.batch;

import static innowake.mining.shared.PatternConverter.convertAntToRegexPattern;

import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import static innowake.lib.core.lang.Assert.assertNotNull;

import innowake.lib.core.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.server.discovery.Logging;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.SourceService;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.ndt.jcl.parser.assembling.IContentProvider;

public class DiscoveryJclContentProvider implements IContentProvider {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.JCL_PARSER);
	private static final Pattern DOT_PATTERN = Pattern.compile("\\.");
	
	private static final Comparator<SourcePojo> SOURCE_OBJECT_COMPARATOR = (so1, so2) -> {
		final int typeValue1 = getTypePriorityForIncludeMember(so1.getType());
		final int typeValue2 = getTypePriorityForIncludeMember(so2.getType());
		return typeValue1 - typeValue2;
	};
	
	private final SourceService sourceService;
	private final EntityId projectId;
	private final Map<String, String> resolvedProcs = new HashMap<>();

	@Nullable private final SearchOrders searchOrders;
	@Nullable private final String sourcePath;

	public DiscoveryJclContentProvider(final EntityId projectId, final SourceService sourceService) {
		this(projectId, sourceService, null, null);
	}

	public DiscoveryJclContentProvider(final EntityId projectId, final SourceService sourceService, @Nullable final SearchOrders searchOrders,
			@Nullable final String sourcePath) {
		this.projectId = projectId;
		this.sourceService = sourceService;
		this.searchOrders = searchOrders;
		this.sourcePath = sourcePath;
	}

	@Override
	@Nullable
	public String getProc(@Nullable final String procName, final @Nullable List<String> jclLibOrder) {
		if (procName == null) {
			throw new IllegalStateException("Proc name should be provided");
		}
		final List<SourcePojo> sourceObjects = sourceService.find(q -> q
				.ofProject(projectId)
				.withName(procName)
				.withTechnology(Technology.JCL)
				.withType(Type.PROC));
		if (sourceObjects.isEmpty()) {
			throw new IllegalStateException("Procedure not found with the provided name " + procName);
		}
		return getContent(sourceObjects, jclLibOrder);
	}

	@Override
	public @Nullable String getIncludeMember(final String memberName, final List<String> jclLibOrder) {
		final List<SourcePojo> sourceObjects = sourceService.find(q -> q
				.ofProject(projectId)
				.withName(memberName)
				.withTechnology(Technology.JCL));
		if (sourceObjects.isEmpty()) {
			return null;
		}
		return getContent(sourceObjects, jclLibOrder);
	}
	
	private @Nullable String getContent(final List<SourcePojo> sourceObjects, @Nullable final List<String> jclLibOrders) {
		final SourcePojo resolvedSourceObject = resolveSourceObject(sourceObjects);
		if (jclLibOrders == null || jclLibOrders.isEmpty()) {
			resolvedProcs.computeIfAbsent(resolvedSourceObject.getName(), path -> resolvedSourceObject.getPath());
			return resolvedSourceObject.getContent().toString();
		}
		for (final String jclLibOrder : jclLibOrders) {
			final String jclLibOrderWithSlash = DOT_PATTERN.matcher(jclLibOrder).replaceAll("/");
			final List<SourcePojo> validSourceObjects = sourceObjects.stream()
					.filter(sourceObject -> sourceObject.getPath().contains(jclLibOrder) || sourceObject.getPath().contains(jclLibOrderWithSlash))
					.sorted(SOURCE_OBJECT_COMPARATOR)
					.collect(Collectors.toList());

			if (validSourceObjects.isEmpty()) {
				LOG.error("No Source Object matched with the given JCLLIB ORDER: " + jclLibOrder);
			} else {
				if (validSourceObjects.size() > 1) {
					final String paths = validSourceObjects.stream().map(SourcePojo::getPath).collect(Collectors.joining(","));
					LOG.error("More than one Source Objects: ("+ paths +") matched with the given JCLLIB ORDER: " + jclLibOrder);
				}
				final var validSourceObject = resolveSourceObject(validSourceObjects);
				resolvedProcs.computeIfAbsent(validSourceObject.getName(), path -> validSourceObject.getPath());
				return validSourceObject.getContent().toString();
			}
		}
		resolvedProcs.computeIfAbsent(resolvedSourceObject.getName(), path -> resolvedSourceObject.getPath());
		return resolvedSourceObject.getContent().toString();
	}

	private SourcePojo resolveSourceObject(final List<SourcePojo> sourceObjects) {
		if (sourceObjects.size() == 1 || searchOrders == null || sourcePath == null) {
			return sourceObjects.get(0);
		}
		final String sourceObjectPath = sourcePath;
		final String[] targetPatterns = assertNotNull(searchOrders).resolvePattern(assertNotNull(sourceObjectPath));

		return resolveSourceObjectWithSearchOrders(sourceObjects, assertNotNull(sourceObjectPath), targetPatterns);
	}

	private SourcePojo resolveSourceObjectWithSearchOrders(final List<SourcePojo> sourceObjects, final String sourceObjectPath,
			final String[] targetPatterns) {
		int count =  0;
		for (String targetPattern: targetPatterns) {
			count++;

			/* Update target pattern if object has to be resolved is in same folder */
			if (targetPattern.startsWith("./")) {
				targetPattern = FilenameUtils.getPath(sourceObjectPath) + targetPattern.substring(2);
			}

			if (StringUtils.isNotBlank(targetPattern)) {
				final String pathRegex = convertAntToRegexPattern(targetPattern);
				final List<SourcePojo> targetSourceObjects = sourceObjects.stream().filter(sourceObject -> Pattern.matches(pathRegex, sourceObject.getPath()))
						.collect(Collectors.toList());
				if (targetSourceObjects.isEmpty()) {
					/* Check for last item in list */
					if (count == targetPatterns.length) {
						return sourceObjects.get(0);
					}
				} else {
					return targetSourceObjects.get(0);
				}
			}
		}

		return sourceObjects.get(0);
	}

	private static int getTypePriorityForIncludeMember(final Type type) {
		switch (type) {
			case INCLUDE:
				return 1;
			case PROC:
				return 2;
			case JOB:
				return 3;
			default:
				return 4;
		}
	}
	
	/**
	 * Gets the map with resolved JCL PROC name with its path.
	 * @return resolved JCL PROC name with its path
	 */
	public Map<String, String> getResolvedProcs() {
		return resolvedProcs;
	}
}
