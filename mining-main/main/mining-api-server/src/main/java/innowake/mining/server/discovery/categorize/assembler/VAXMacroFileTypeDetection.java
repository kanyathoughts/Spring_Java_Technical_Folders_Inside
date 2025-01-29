/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.categorize.assembler;

import java.util.Set;
import java.util.regex.Pattern;

import com.google.common.collect.Sets;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.categorize.AbstractFileTypeDetection;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.categorize.Identification.ID;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * Detection for VAXMacro(assembler)-files.
 */
public class VAXMacroFileTypeDetection extends AbstractFileTypeDetection {

	public static final String TITLE_TOKEN = ".TITLE";
	private static final Pattern NEW_LINE_PTR = Pattern.compile("\n");
	
	public VAXMacroFileTypeDetection() {
		super(getLanguage());
	}
	
	@Override
	@Nullable
	public Identification identifyMainObject(final SourcePojo resource) {
		final String content = resource.getContent().toString();
		final String[] splitLine = NEW_LINE_PTR.split(content);
		int mandTokensFound = 0;
		int optionalTokensFound = 0;
		for (final String line : splitLine) {
			final String trimmedLine = line.trim();
			if (trimmedLine.startsWith(TITLE_TOKEN)) {
				mandTokensFound++;
			}
			if (trimmedLine.startsWith(".IDENT") || trimmedLine.startsWith("$SSDEF") || trimmedLine.startsWith(".END") || trimmedLine.startsWith(".LONG")) {
				optionalTokensFound++;
			}
		}
		if (mandTokensFound >= 1 && optionalTokensFound >= 1) {
			return new Identification(ID.YES, resource.getId(), ResolveTarget.VAX_MACRO, language);
		}

		return null;
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
}
