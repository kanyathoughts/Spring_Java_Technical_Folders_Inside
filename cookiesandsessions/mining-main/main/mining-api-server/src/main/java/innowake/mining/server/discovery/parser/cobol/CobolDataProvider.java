/* Copyright (c) 2018 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.parser.cobol;

import java.util.Set;
import java.util.UUID;

import org.apache.commons.io.FilenameUtils;

import java.util.Optional;
import org.springframework.util.AntPathMatcher;

import innowake.lib.core.lang.Nullable;
import innowake.mining.data.discovery.metrics.DiscoveryCache;
import innowake.mining.server.discovery.SourceObjectMatcher;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.access.BinaryString;
import innowake.mining.shared.access.BinaryValue;
import innowake.mining.shared.access.CustomPropertiesMap;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.config.searchorder.SearchOrders;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.Technology;
import innowake.mining.shared.model.Type;
import innowake.ndt.core.assembling.IAssemblingDataProvider;
import innowake.ndt.core.assembling.IAssemblingObjectType;
import innowake.ndt.core.assembling.cobol.UnisysCobolCopyLibParser;
import innowake.ndt.core.assembling.cobol.CobolAssembler.CobolAssemblingObjectType;

/**
 * Data Provider for the cobol parser
 */
public final class CobolDataProvider implements IAssemblingDataProvider<SourcePojo> {

	private static final String COPYPROC_INVENTORY_BUILT = "CobolDataProviderCopyProcInventoryBuilt";
	private static final String COPYPROC_PATH_KEY = "CobolDataProviderCopyProcPath:";
	private static final String COPYPROC_CONTENT_KEY = "CobolDataProviderCopyProcContent:";

	private static final AntPathMatcher pathMatcher = new AntPathMatcher("/");

	private final EntityId projectId;
	private final String jobId;
	private final SourceCachingService sourceService;
	private final SearchOrders searchOrders;
	private final SourceObjectResolver sourceObjectResolver;
	private final DiscoveryCache discoveryCache;

	/**
	 * Constructor
	 * 
	 * @param projectId The project Id
	 * @param jobId the Job Id 
	 * @param sourceService Service for accessing source objects.
	 * @param searchOrders The searchOrders
	 * @param sourceObjectResolver The {@link SourceObjectResolver}
	 * @param discoveryCache {@link DiscoveryCache}
	 */
	public CobolDataProvider(final EntityId projectId, final String jobId, final SourceCachingService sourceService, final SearchOrders searchOrders,
			final SourceObjectResolver sourceObjectResolver, final DiscoveryCache discoveryCache) {
		this.projectId = projectId;
		this.sourceService = sourceService;
		this.searchOrders = searchOrders;
		this.sourceObjectResolver = sourceObjectResolver;
		this.jobId = jobId;
		this.discoveryCache = discoveryCache;
	}

	@Nullable
	@Override
	public SourcePojo find(final SourcePojo root, final String name, final IAssemblingObjectType expectedType) {
		if (CobolAssemblingObjectType.COPYLIB.equals(expectedType)) {
			return sourceObjectResolver.resolveObject(root, name, new SourceObjectMatcher(Technology.COBOL, Type.COPYLIB));
		} else {
			/* try COPYBOOK first */
			final SourcePojo resolvedObject = sourceObjectResolver.resolveObject(root, name, new SourceObjectMatcher(Technology.COBOL, Type.COPYBOOK));
			/* try COPYBOOK otherwise try to find COPYPROC with the matching name */
			return resolvedObject != null ? resolvedObject: findCopyProc(root, name);
		}
	}
	
	@Nullable
	@Override
	public SourcePojo find(final SourcePojo root, final String library, final String name, final IAssemblingObjectType expectedType) {
		/* Called when Proc with Library is used in the source */
		return sourceObjectResolver.resolveObject(root, name, new SourceObjectMatcher(Technology.COBOL, Type.COPYLIB));
	}

	@Nullable
	private SourcePojo findCopyProc(final SourcePojo root, final String name) {
		try {
			/* locateCopyProc() requires the "copyproc inventory" to be built, so ensure that this has been done */
			discoveryCache.computeValueIfAbsent(jobId, COPYPROC_INVENTORY_BUILT, () -> {
				computeCopyProcInventory();
				/* this simply puts a marker in the map, indicating that the "copyproc inventory" has been computed for this job */
				return Boolean.TRUE;
			});
			
			final Optional<String> copyProcPath = locateCopyProc(root, name);
			if( ! copyProcPath.isPresent()) {
				return null;
			}

			/*
			 * create and return "fake" SourcePojo containing the COPYPROC contents - metaDataRevision and contentRevision are arbitrarily set to 1 and we
			 * skip computing the proper contentHash by setting it to EMPTY_CONTENT_HASH
			 */
			final String content = loadCopyProcContent(copyProcPath.get(), name);
			return new SourcePojo(UUID.randomUUID(), -1l, projectId, name, copyProcPath.get(),
					Technology.COBOL, Type.COPYPROC, Long.valueOf(1), Long.valueOf(1), BinaryValue.EMPTY, () -> new BinaryString(content), CustomPropertiesMap.empty());

		} catch (final Exception e) {
			throw new IllegalStateException(String.format("Error while resolving potential COPYPROC include %s for %s", name, root.getPath()), e);
		}
	}

	private void computeCopyProcInventory() {
		sourceService.find(q -> q.ofProject(projectId).withTechnology(Technology.COBOL).withType(Type.COPYLIB)).forEach(copyLib -> {
			UnisysCobolCopyLibParser.parse(copyLib.getContent().toString()).keySet().forEach(copyProcName -> {
				/* we are using putMultiValue() to account for the possibility that multiple COPYLIBs contain a COPYPROC of the same name */
				discoveryCache.putMultiValue(jobId, COPYPROC_PATH_KEY + copyProcName, copyLib.getPath());
			});
		});
	}

	private Optional<String> locateCopyProc(final SourcePojo root, final String name) {
		/* locate a COPYPROC with the given name inside a COPYLIB that matches the SearchOrder */
		@SuppressWarnings("unchecked")
		final Set<String> pathCandidates = (Set<String>) (Set<?>) discoveryCache.getMultiValue(jobId, COPYPROC_PATH_KEY + name);
		for (final String targetPattern : searchOrders.resolvePattern(root.getPath())) {
			for (final String candidatePath : pathCandidates) {
				if (pathMatcher.match(targetPattern, candidatePath)) {
					return Optional.ofNullable(candidatePath);
				}
			}
		}
		/* no match found */
		return Optional.empty();
	}

	private String loadCopyProcContent(final String copyLibPath, final String copyProcName) throws Exception {
		return (String) discoveryCache.computeValueIfAbsent(jobId, COPYPROC_CONTENT_KEY + copyLibPath + copyProcName, () -> {
			return UnisysCobolCopyLibParser.parse(sourceService.cachingByProjectPath(projectId.getNid(), copyLibPath).getContent().toString()).get(copyProcName).getSection();
		});
	}

	@Nullable
	@Override
	public String getPath(final SourcePojo object) {
		return object.getPath();
	}

	@Nullable
	@Override
	public String getSource(final SourcePojo object) {
		return object.getContent().toString();
	}

	@Nullable
	@Override
	public IAssemblingObjectType getType(final SourcePojo object) {
		switch (object.getType()) {
			case COPYBOOK:
				return CobolAssemblingObjectType.COPYBOOK;
			case BMS_MAPSET:
			case PROGRAM:
			case FILE:
			default:
				return null;
		}
	}

	@Nullable
	@Override
	public String getName(final SourcePojo sourceObject) {
		return FilenameUtils.getBaseName(sourceObject.getPath());
	}

	@Nullable
	@Override
	public Object getHashable(final SourcePojo object) {
		return object;
	}

	@Override
	public boolean isObjectProxy(final SourcePojo object) {
		return false;
	}

}
