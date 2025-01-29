/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.categorize.basic;

import java.util.Set;

import com.google.common.collect.Sets;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.categorize.AbstractFileTypeDetection;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * File type detection for BASIC.
 */
public class BasicFileTypeDetection extends AbstractFileTypeDetection {
	
	/**
	 * Constructor to initialize {@link BasicFileTypeDetection}.
	 */
	public BasicFileTypeDetection() {
		super(getLanguage());
	}

	/**
	 * @return the {@link ResolveTarget} defining the language for this detector
	 */
	public static ResolveTarget getLanguage() {
		return ResolveTarget.BASIC;
	}

	/**
	 * @return the {@linkplain innowake.mining.server.discovery.categorize.FileTypeDetection.DetectionPhase DetectionPhases} supported by this detector
	 */
	public static Set<DetectionPhase> getDetectionPhases() {
		return Sets.newHashSet(DetectionPhase.MAIN);
	}

	@Override
	@Nullable
	public Identification identifyMainObject(final SourcePojo resource) {
		return null;
	}	
}
