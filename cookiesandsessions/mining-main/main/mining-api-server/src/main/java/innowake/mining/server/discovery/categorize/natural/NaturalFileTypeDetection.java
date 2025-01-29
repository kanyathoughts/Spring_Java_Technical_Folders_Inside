/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.categorize.natural;

import java.util.Set;

import org.apache.commons.io.FilenameUtils;

import com.google.common.collect.Sets;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.categorize.AbstractFileTypeDetection;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.categorize.Identification.ID;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.mining.shared.model.discovery.ResolveTargetHelper;

/**
 * File type detection for Natural.
 */
public class NaturalFileTypeDetection extends AbstractFileTypeDetection {

	private static final Set<String> EXTENSIONS = ResolveTargetHelper.getExtensionsByLanguage(ResolveTarget.NATURAL);
	
	public NaturalFileTypeDetection() {
		super(getLanguage());
	}
		
	@Override
	@Nullable
	public Identification identifyMainObject(final SourcePojo resource) {
		/* Natural categorize presumes a SYSOBJH import with natclipse which ensures files having file extensions */
		final String extension = FilenameUtils.getExtension(resource.getPath());
		if (EXTENSIONS.contains(extension)) {
			final Set<ResolveTarget> resolveTargets = ResolveTargetHelper.getResolveTargets(extension);
			if (resolveTargets.isEmpty()) {
				throw new IllegalStateException("Unknown file extension " + extension);
			} else if (resolveTargets.size() > 1) {
				throw new IllegalStateException("Multiple types found for the file extension " + extension);
			}
			final ResolveTarget resolveTarget = resolveTargets.iterator().next();
			
			/* reporting mode cannot be detected here:
			 * - we need natclipse API to detect reporting mode
			 * - but we cannot presume automatic project configuration for natclipse at this time */
			
			return new Identification(ID.YES, resource.getId(), resolveTarget, getLanguage());
		}
		return null;
		
	}

	/**
	 * @return the {@link ResolveTarget} defining the language for this detector
	 */
	public static ResolveTarget getLanguage() {
		return ResolveTarget.NATURAL;
	}
	
	/**
	 * @return the {@linkplain innowake.mining.server.discovery.categorize.FileTypeDetection.DetectionPhase DetectionPhases} supported by this detector
	 */
	public static Set<DetectionPhase> getDetectionPhases() {
		return Sets.newHashSet(DetectionPhase.MAIN);
	}
}
