/* Copyright (c) 2018 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.categorize.binary;

import static innowake.mining.shared.model.discovery.ResolveTarget.BINARY;

import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.collect.Sets;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.categorize.AbstractFileTypeDetection;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.categorize.Identification.ID;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * Detects binary files by removing all non-printable ASCII characters from the input.
 * <p>
 * Printable also  includes common characters found in source code e.g. TABS, SPACES, NEWLINES...
 */
public class BinaryFileTypeDetection extends AbstractFileTypeDetection {

	private final Config config;

	/**
	 * This includes the standard set of printable ASCII characters and also characters
	 * which are common in source code like TABS, SPACES, NEWLINE CHARACTERS...
	 */
	private static final Pattern PRINTABLE_CHARS = Pattern.compile("[\\p{Graph}\\p{Space}]");

	/**
	 * Creates a new binary file type detection instance, given the configuration.
	 * 
	 * @param config the configuration to use for determining the threshold of printable characters
	 */
	public BinaryFileTypeDetection(final Config config) {
		super(getLanguage());
		this.config = config;
	}

	@Override
	@Nullable
	public Identification identifyMainObject(final SourcePojo resource) {
		return null;
	}

	@Override
	@Nullable
	public Identification identifyByContent(final SourcePojo resource) {
		final String content = resource.getContent().toString();
		final int contentLength = content.length();
		final int numberOfPrintableChars = countNumberOfPrintableChars(content);
		if (LOG.isDebugEnabled()) {
			printDebug(resource, contentLength, numberOfPrintableChars);
		}

		final Identification result;
		if (contentLength > 0 && (double) numberOfPrintableChars / contentLength < config.getPrintableCharThreshold()) {
			result = new Identification(ID.YES, resource.getId(), BINARY, getLanguage());
			if (LOG.isDebugEnabled()) {
				LOG.debug("Filetype detection Binary: {}", result);
			}
		} else {
			result = null;
		}

		return result;
	}

	/**
	 * @return the {@link ResolveTarget} defining the language for this detector
	 */
	public static ResolveTarget getLanguage() {
		return ResolveTarget.BINARY;
	}
	
	/**
	 * @return the {@linkplain innowake.mining.server.discovery.categorize.FileTypeDetection.DetectionPhase DetectionPhases} supported by this detector
	 */
	public static Set<DetectionPhase> getDetectionPhases() {
		return Sets.newHashSet(DetectionPhase.MAIN);
	}
	
	private int countNumberOfPrintableChars(final String content) {
		int count = 0;
		final Matcher matcher = PRINTABLE_CHARS.matcher(content);
		while (matcher.find()) {
			count++;
		}
		return count;
	}
	
	private void printDebug(final SourcePojo resource, final int contentLength, final int numberOfPrintableChars) {
		LOG.debug(() -> "File: " + resource.getPath());
		LOG.debug(() -> "Number of printable chars: " + numberOfPrintableChars);
		LOG.debug(() -> "Total number of chars: " + contentLength);
		LOG.debug(() -> "Ratio: " + (double) numberOfPrintableChars / contentLength);
		LOG.debug(() -> "Threshold: " + config.getPrintableCharThreshold());
	}

}
