/* Copyright (c) 2018 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.categorize.csd;

import static innowake.mining.shared.model.discovery.ResolveTarget.CSD_EXTRACT;
import static innowake.mining.shared.model.discovery.ResolveTarget.CSD_LIST;

import java.util.Set;

import com.google.common.collect.Sets;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.categorize.AbstractFileTypeDetection;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.categorize.Identification.ID;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.parsing.parser.csd.CsdParser;
import innowake.ndt.parsing.parser.csd.CsdParserFactory;
import innowake.ndt.parsing.parser.csd.CsdParserFactory.CsdParserType;
import innowake.ndt.parsing.parser.csd.model.Csd;

/**
 * Detects CSD files.
 */
public class CSDFileTypeDetection extends AbstractFileTypeDetection {
	
	private static final int LIGHTWEIGHT_LEXING_LIMIT = 300;
	
	private final CsdParser csdListParser;
	private final CsdParser csdExtractParser;
	
	public CSDFileTypeDetection() {
		super(getLanguage());
		csdListParser = CsdParserFactory.createLightWeightParser(CsdParserType.LIST, LIGHTWEIGHT_LEXING_LIMIT).strictMode();
		csdExtractParser = CsdParserFactory.createLightWeightParser(CsdParserType.EXTRACT, LIGHTWEIGHT_LEXING_LIMIT).strictMode();
	}

	@Override
	@Nullable
	public Identification identifyMainObject(final SourcePojo resource) {
		final String content = resource.getContent().toString();
		Identification result = null;
		try {
			final Csd csdl = csdListParser.parse(content);
			if ( ! csdl.isEmpty()) {
				result = new Identification(ID.YES, resource.getId(), CSD_LIST, getLanguage());
				return result;
			}
		} catch (final Exception e) {
			result = null;
		} 
		try {
			final Csd csde = csdExtractParser.parse(content);
			if ( ! csde.isEmpty()) {
				result = new Identification(ID.YES, resource.getId(), CSD_EXTRACT, getLanguage());
				return result;
			}
		} catch (final Exception e) {
			result = null;
		}
		return result;
	}

	/**
	 * @return the {@link ResolveTarget} defining the language for this detector
	 */
	public static ResolveTarget getLanguage() {
		return ResolveTarget.CSD;
	}
	
	/**
	 * @return the {@linkplain innowake.mining.server.discovery.categorize.FileTypeDetection.DetectionPhase DetectionPhases} supported by this detector
	 */
	public static Set<DetectionPhase> getDetectionPhases() {
		return Sets.newHashSet(DetectionPhase.MAIN);
	}

}
