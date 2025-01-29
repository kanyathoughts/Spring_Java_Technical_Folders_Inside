/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.categorize.fms;

import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

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
 * File type detection for FMS.
 */
public class FmsFileTypeDetection extends AbstractFileTypeDetection {

	private static final Pattern DEFAULT_LB = Pattern.compile("\n");
	private final Config config;

	/**
	 * Create a new instance of the FMS file type detector.
	 * 
	 * @param config Project configuration file. 
	 */
	public FmsFileTypeDetection(final Config config) {
		super(getLanguage());
		this.config = config;
	}

	@Override
	@Nullable
	public Identification identifyMainObject(final SourcePojo resource) {
		final String result = resource.getContent().toString();
		final List<String> lines = Arrays.stream(DEFAULT_LB.split(result))
											.map(StringUtils::upperCase)
											.map(StringUtils::trim)
											.collect(Collectors.toList());
		if (containsFmsRequiredContents(lines)) {
			if (containsFmsOptionalContents(lines)) {
				return new Identification(ID.YES, resource.getId(), ResolveTarget.FMS_FORM, getLanguage());
			} else {
				return new Identification(ID.MAYBE, resource.getId(), ResolveTarget.FMS_FORM, getLanguage());
			}
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
	 * @return the {@link ResolveTarget} defining the language for this detector
	 */
	public static Set<DetectionPhase> getDetectionPhases() {
		return Sets.newHashSet(DetectionPhase.MAIN);
	}
	
	private boolean containsFmsRequiredContents(final List<String> lines) {
		final boolean containsFormName = lines.stream()
				.anyMatch(line -> line.startsWith("FORM NAME"));
		if ( ! containsFormName) {
			return false;
		}
		return lines.stream()
				.anyMatch(line -> line.contains("END_OF_FORM NAME"));			
	}
	
	private boolean containsFmsOptionalContents(final List<String> lines) {
		final List<String> pattern = Arrays.asList("TEXT", "FIELD NAME", "AREA_TO_CLEAR", "WIDTH", "BACKGROUND", "HIGHLIGHT");
		final List<String> content = lines.stream()
				.collect(Collectors.toList());
		return discoverByThreshold(content, pattern, config.getFmsPatternThreshold());
	}

}
