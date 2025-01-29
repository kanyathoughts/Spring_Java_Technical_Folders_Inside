/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.discovery.categorize.ecl;

import java.util.Arrays;
import java.util.Set;
import com.google.common.collect.Sets;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.discovery.categorize.AbstractFileTypeDetection;
import innowake.mining.shared.discovery.DiscoveryException;
import innowake.mining.shared.discovery.categorize.Identification;
import innowake.mining.shared.discovery.categorize.Identification.ID;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.entities.SourcePojo;
import innowake.mining.shared.model.discovery.ResolveTarget;
import innowake.ndt.core.parsing.IDocument;
import innowake.ndt.core.parsing.IToken;
import innowake.ndt.core.parsing.ITokenPartitioning;
import innowake.ndt.core.parsing.TokenPartitioner2;
import innowake.ndt.core.parsing.spi.Document;
import innowake.ndt.parsing.scanner.ecl.EclLexerFactory;
import innowake.ndt.parsing.scanner.ecl.EclRegionType;

/**
 * File type detection for Ecl.
 */
public class EclFileTypeDetection extends AbstractFileTypeDetection {

	private static final TokenPartitioner2 TOKEN_PARTITIONER = TokenPartitioner2.create(EclLexerFactory.get());
	private final Config config;

	/**
	 * Ecl File Detection based on extension and token pattern.
	 * 
	 * @param config Project configuration file
	 */
	public EclFileTypeDetection(final Config config) {
		super(getLanguage());
		this.config = config;
	}

	@Override
	@Nullable
	public Identification identifyMainObject(final SourcePojo sourceObject) throws DiscoveryException {
		return null;
	}

	/**
	 * @return the {@link ResolveTarget} defining the language for this detector
	 */
	public static ResolveTarget getLanguage() {
		return ResolveTarget.ECL;
	}

	/**
	 * @return the {@linkplain innowake.mining.server.discovery.categorize.FileTypeDetection.DetectionPhase DetectionPhases} supported by this detector
	 */
	public static Set<DetectionPhase> getDetectionPhases() {
		return Sets.newHashSet(DetectionPhase.MAIN);
	}

	@Override
	@Nullable
	public Identification identifyByContent(final SourcePojo resource) {
		final IDocument sourceDocument = new Document(resource.getContent().toString());
		final ITokenPartitioning partitioning = TOKEN_PARTITIONER.doPartitioning(sourceDocument);
		final IToken[] operationFields = partitioning.getTokens(EclRegionType.TYPE_OPERATION_FIELD);
		if (containsEclControlStatements(operationFields) || containsAtSignAsFirstCharacterInLine(sourceDocument)) {
			return new Identification(ID.YES, resource.getId(), ResolveTarget.ECL_JOB, getLanguage());
		}
		return null;
	}

	private boolean containsEclControlStatements(final IToken[] tokens) {
		for (final IToken token : tokens) {
			if (token.getColumn()==0 && EclControlStatementType.isValidControlStatement(token)) {
				return true;
			}
		}
		return false;
	}

	private boolean containsAtSignAsFirstCharacterInLine(final IDocument document) {
		final String[] splitContent = document.lines() ;
		return discoverByThreshold(Arrays.asList(splitContent), Arrays.asList("@"), config.getEclPatternThreshold());
	}
}
