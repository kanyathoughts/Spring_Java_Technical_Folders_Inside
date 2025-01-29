/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.metrics.natural;

import static innowake.mining.server.discovery.metrics.natural.NaturalProgrammingMode.REPORTING;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;
import org.apache.commons.io.FilenameUtils;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.server.discovery.SourceObjectResolver;
import innowake.mining.server.discovery.metrics.ResourceCollector;
import innowake.mining.server.discovery.parser.natural.NaturalParseResultProvider;
import innowake.mining.shared.access.SourceCachingService;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.mining.shared.model.discovery.ResolveTargetHelper;

/**
 * Collects all modules for Natural.
 */
public class NaturalModuleCollector extends ResourceCollector {
	
	private final SourceObjectResolver sourceObjectResolver;
	private final NaturalSourceObjectManager sourceObjectManager;
	private final NaturalParseResultProvider naturalParserResultProvider;
	
	public NaturalModuleCollector(final SourceObjectResolver sourceObjectResolver, final SourceCachingService sourceObjectDao,
			final NaturalSourceObjectManager sourceObjectManager, final NaturalParseResultProvider naturalParserResultProvider) {
		super(ResolveTarget.NATURAL, sourceObjectDao);
		this.sourceObjectResolver = sourceObjectResolver;
		this.sourceObjectManager = sourceObjectManager;
		this.naturalParserResultProvider = naturalParserResultProvider;
	}
	
	@Override
	protected Set<String> getExtensions() {
		return ResolveTargetHelper.getExtensionsByLanguage(ResolveTarget.NATURAL);
	}

	@Override
	protected Stream<ModelArtifact> collectInternal(final SourcePojo sourceObject) throws DiscoveryException {
		LOG.debug(() -> "Collecting resources in NATURAL module: " + sourceObject.getName());
		final List<ModelArtifact> result = new ArrayList<>();

		ResolveTarget type = null;
		try {
			type = mapTypeNatural(sourceObject);
			result.add(new ModelArtifact()
								.setName(sourceObject.getName())
								.setType(type)
								.setPath(sourceObject.getPath())
								.validate());
		} catch (final Exception e) {
			LOG.error(() -> String.format("Error while resolving type for object %s", sourceObject.getPath()), e);
		}
		
		if (type != null && type != ResolveTarget.NATURAL_TEXT) {
			final FieldValueResolver fieldValueResolver = new FieldValueResolver(sourceObject, naturalParserResultProvider);
			try {
				new LightweightParser(sourceObject, fieldValueResolver, sourceObjectResolver)
					.getOutgoingCalls()
					.stream()
					.forEach(name -> result.add(new ModelArtifact()
														 .setName(name)
														 .setType(ResolveTarget.PROGRAM) 
														 .validate()));
			} catch (final Exception e) {
				LOG.error(() -> String.format("Error while collecting virtual modules for object %s", sourceObject.getName()), e);
			}
		}
		return result.stream();
	}
	
	private ResolveTarget mapTypeNatural(final SourcePojo sourceObject) {
		final String extension = FilenameUtils.getExtension(sourceObject.getPath());
		final Set<ResolveTarget> resolveTargets = ResolveTargetHelper.getResolveTargets(extension);
		if (resolveTargets.isEmpty()) {
			throw new IllegalStateException("Unknown Natural file extension " + extension + " for object " + sourceObject.getPath());
		} else if (resolveTargets.size() > 1) {
			throw new IllegalStateException("Multiple types found for file extension " + extension + " object " + sourceObject.getPath());
		}
		
		ResolveTarget resolveTarget = resolveTargets.iterator().next();
		
		if (sourceObjectManager.getProgrammingMode(sourceObject) == REPORTING) {
			/* reporting mode types are child of structured mode types */ 
			resolveTarget = resolveTarget.getChildren().iterator().next();
		}
		
		return resolveTarget;
	}
}
