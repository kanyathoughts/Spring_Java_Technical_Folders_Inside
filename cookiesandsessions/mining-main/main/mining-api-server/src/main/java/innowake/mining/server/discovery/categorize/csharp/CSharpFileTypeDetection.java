/* Copyright (c) 2024 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.categorize.csharp;

import com.google.common.collect.Sets;
import innowake.mining.server.discovery.categorize.AbstractFileTypeDetection;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;

import java.util.Set;

/**
 * Categorizer for C Sharp files.
 */
public class CSharpFileTypeDetection extends AbstractFileTypeDetection {

	public CSharpFileTypeDetection() {
		super(getLanguage());
	}

	@Override
	public Identification identifyMainObject(final SourcePojo sourceObject) throws DiscoveryException {
		return null;
	}

	/**
	 * @return the {@link ResolveTarget} defining the language for this detector
	 */
	public static ResolveTarget getLanguage() {
		return ResolveTarget.CSHARP;
	}

	/**
	 * @return the {@linkplain innowake.mining.server.discovery.categorize.FileTypeDetection.DetectionPhase DetectionPhases} supported by this detector
	 */
	public static Set<DetectionPhase> getDetectionPhases() {
		return Sets.newHashSet(DetectionPhase.MAIN);
	}
}
