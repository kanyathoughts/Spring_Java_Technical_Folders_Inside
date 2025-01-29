/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.io.FilenameUtils;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.server.discovery.Logging;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.config.core.IdentificationMapper;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.ModuleLocation;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.mining.shared.model.discovery.ResolveTargetHelper;

/**
 * Abstract base class for resource collectors.
 */
public abstract class ResourceCollector {
	
	protected static final Logger LOG = LoggerFactory.getLogger(Logging.METRICS_MODULE_REPOSITORY);
	
	protected final ResolveTarget resolveTarget;
	protected final SourceCachingService sourceService;
	
	/**
	 * Constructor.
	 * 
	 * @param target the {@link ResolveTarget}
	 * @param sourceService Service providing access to source objects. 
	 */
	protected ResourceCollector(final ResolveTarget target, final SourceCachingService sourceService) {
		this.resolveTarget = target;
		this.sourceService = sourceService;
	}
		
	/**
	 * Collects the resources of the given {@code sourceObject}.
	 *
	 * @param sourceObject the {@link SourcePojo} to collect all resources from
	 * @return the collected {@linkplain ModelArtifact ModelArtifacts}
	 * @throws DiscoveryException if any error occurs
	 */
	public Set<ModelArtifact> collect(final SourcePojo sourceObject) throws DiscoveryException {
		if (isCollectable(sourceObject)) {
			return collectInternal(sourceObject).collect(Collectors.toCollection(LinkedHashSet::new));
		}
		return Collections.emptySet();
	}
	
	/**
	 * Returns {@code true} if the given {@code sourceObject} is collectable. By default it has to be located
	 * in a matching source directory with a matching extension based on the {@link ResolveTarget}.
	 * 
	 * @param sourceObject the {@link SourcePojo}
	 * @return {@code true} if collectable; {@code false} otherwise
	 */
	protected boolean isCollectable(final SourcePojo sourceObject) {
		return IdentificationMapper.matchesPath(resolveTarget, sourceObject) && matchExtension(sourceObject);
	}

	/**
	 * Collects the resources from the provided {@code sourceObject}
	 *
	 * @param sourceObject the {@link SourcePojo}
	 * @return binary resources contained in the file
	 * @throws DiscoveryException exception
	 */
	protected Stream<ModelArtifact> collectInternal(final SourcePojo sourceObject) throws DiscoveryException {
		LOG.trace(() -> "Collect " + resolveTarget.toString() + " resources in " + sourceObject.getName());
		/* Determine by extension alone */ 
		final ResolveTarget type = mapType(sourceObject);
		return Stream.of(new ModelArtifact()
							.setType(type)
							.setName(sourceObject.getName())
							.setPath(sourceObject.getPath())
							.setLocation(new ModuleLocation(0, sourceObject.getContent().toString().length()))
							.validate());
	}

	/**
	 * Map file extension to {@link ResolveTarget} type
	 *
	 * @param sourceObject The source object.
	 * @return the ResolveTarget type
	 */
	protected static ResolveTarget mapType(final SourcePojo sourceObject) {
		final String fileExtension = FilenameUtils.getExtension(sourceObject.getPath());
		final Set<ResolveTarget> types = ResolveTargetHelper.getResolveTargets(fileExtension);
		if (types.isEmpty()) {
			throw new IllegalStateException("Unknown file extension" + fileExtension);
		}
		if (types.size() == 1) {
			return types.iterator().next();
		}
		final Set<ResolveTarget> matchingTypes = types.stream()
				.filter(type -> IdentificationMapper.matchesPath(type, sourceObject))
				.collect(Collectors.toSet());
		if (matchingTypes.size() > 1) {
			throw new IllegalStateException("Multiple types found for the " + FilenameUtils.getPath(sourceObject.getPath()));
		} else if (matchingTypes.isEmpty()) {
			throw new IllegalStateException("No types found for the " + FilenameUtils.getPath(sourceObject.getPath()));
		} else {
			return matchingTypes.iterator().next();
		}				
	}
	
	/**
	 * @return Set of extensions that are applicable for any {@link SourcePojo} that should be processed by this collector
	 */
	protected abstract Set<String> getExtensions();
	
	/**
	 * Checks if the {@code sourceObject} matches the type extension.
	 *
	 * @param sourceObject the {@link SourcePojo} to check the extensions against
	 * @return {@code true} if the {@code sourceObject} matches any of the extensions; {@code false} otherwise
	 */
	protected boolean matchExtension(final SourcePojo sourceObject) {
		final String extension = FilenameUtils.getExtension(sourceObject.getPath()).toUpperCase();
		return getExtensions().stream().anyMatch(ext -> ext.toUpperCase().equals(extension));
	}
}
