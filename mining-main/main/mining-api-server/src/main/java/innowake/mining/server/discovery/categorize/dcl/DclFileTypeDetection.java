/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.categorize.dcl;

import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;

import com.google.common.collect.Sets;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.categorize.AbstractFileTypeDetection;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.categorize.Identification.ID;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * File type detection for DCL.
 */
public class DclFileTypeDetection extends AbstractFileTypeDetection {

	private static final Pattern DEFAULT_LB = Pattern.compile("\n");
	private final Config config;

	/**
	 * Create a new instance of the DCL file type detector.
	 * 
	 * @param config Project configuration file. 
	 */
	public DclFileTypeDetection(final Config config) {
		super(getLanguage());
		this.config = config;
	}

	@Override
	@Nullable
	public Identification identifyMainObject(final SourcePojo resource) {
		final String result = resource.getContent().toString();
		final String[] splitContent = DEFAULT_LB.split(result);
		
		final Identification identifyByFileExtension = identifyByExtension(resource);
		if (identifyByFileExtension != null && identifyByFileExtension.getId() == ID.YES) {
			return identifyByFileExtension;
		}
		
		if (discoverByThreshold(Arrays.asList(splitContent), Arrays.asList("$"), config.getDclPatternThreshold())) {
			return new Identification(ID.YES, resource.getId(), ResolveTarget.DCL, getLanguage());

		} else {
			return null;
		}
	}

	/**
	 * @return the {@link ResolveTarget} defining the language for this detector
	 */
	public static ResolveTarget getLanguage() {
		return ResolveTarget.VMS;
	}
	
	/**
	 * @return the {@linkplain innowake.mining.server.discovery.categorize.FileTypeDetection.DetectionPhase DetectionPhases} supported by this detector
	 */
	public static Set<DetectionPhase> getDetectionPhases() {
		return Sets.newHashSet(DetectionPhase.MAIN);
	}
	
	@Override
	@Nullable
	public Identification identifyByExtension(final SourcePojo resource) {
		final String extension = FilenameUtils.getExtension(resource.getPath());
		if (StringUtils.isBlank(extension)) {
			return null;
		}
		final List<String> dclExtensions = Arrays.asList("COM", "DCL");
		if (dclExtensions.contains(extension.toUpperCase())) {
			return new Identification(ID.YES, resource.getId(), ResolveTarget.DCL, getLanguage());
		}
		return null;
	}
}
