/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.categorize.xml;

import java.util.List;
import java.util.Set;

import com.google.common.collect.Sets;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.categorize.AbstractFileTypeDetection;
import innowake.mining.server.discovery.categorize.Tokenizer;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.categorize.Identification.ID;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;

/**
 * File type detection for XML.
 * Only main types are supported. <a href="https://www.w3.org/TR/xinclude/">XInclude</a> is unsupported .
 */
public class XmlFileTypeDetection extends AbstractFileTypeDetection {

	public XmlFileTypeDetection() {
		super(getLanguage());
	}

	@Override
	@Nullable
	public Identification identifyMainObject(final SourcePojo resource) {
		final Identification result;
		final String content = resource.getContent().toString();
		final List<String> tokens = new Tokenizer(content).getTokens(" ");
		if (tokens.stream().limit(100).anyMatch(token -> token.contains("<?xml")) 
				&& tokens.stream().limit(100).anyMatch(token -> token.contains("version"))
				&& tokens.stream().limit(100).anyMatch(token -> token.contains("?>"))) {
			result = new Identification(ID.YES, resource.getId(), ResolveTarget.XML, getLanguage());
		} else {
			result = null;
		}

		if (result != null && result.getId() != ID.NO) {
			LOG.debug(() -> "Filetype detection XML: " + result);
		}
		return result;
	}

	/**
	 * @return the {@link ResolveTarget} defining the language for this detector
	 */
	public static ResolveTarget getLanguage() {
		return ResolveTarget.XML;
	}
	
	/**
	 * @return the {@linkplain innowake.mining.server.discovery.categorize.FileTypeDetection.DetectionPhase DetectionPhases} supported by this detector
	 */
	public static Set<DetectionPhase> getDetectionPhases() {
		return Sets.newHashSet(DetectionPhase.MAIN);
	}

}
