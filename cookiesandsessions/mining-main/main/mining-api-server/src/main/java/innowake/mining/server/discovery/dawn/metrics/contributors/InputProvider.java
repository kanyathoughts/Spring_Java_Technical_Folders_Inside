/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors;

import innowake.lib.core.lang.Nullable;
import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.server.discovery.metrics.generic.input.SourceProvider;
import innowake.mining.server.discovery.metrics.generic.input.TokenProvider;
import innowake.mining.shared.entities.SourcePojo;
import innowake.ndt.core.parsing.ILexerFactory;
import innowake.ndt.core.parsing.ITokenPartitioner;
import innowake.ndt.core.parsing.ITokenPartitioning;
import innowake.ndt.core.parsing.TokenPartitioner2;
import innowake.ndt.core.parsing.spi.StringContent;

/**
 * Class for InputProvider to get source and tokens by ITokenPartitioning
 */
public class InputProvider implements SourceProvider, TokenProvider {
	
	private final SourcePojo sourceObject;
	
	private final ILexerFactory iLexerFactory;
	@Nullable
	private final ITokenPartitioner iTokenPartitioner;
	
	public InputProvider(final SourcePojo sourceObject, final ILexerFactory iLexerFactory, final ITokenPartitioner iTokenPartitioner) {
		this.sourceObject = sourceObject;
		this.iLexerFactory = iLexerFactory;
		this.iTokenPartitioner = iTokenPartitioner;
	}

	public InputProvider(final SourcePojo sourceObject, final ILexerFactory iLexerFactory) {
		this.sourceObject = sourceObject;
		this.iLexerFactory = iLexerFactory;
		this.iTokenPartitioner = null;
	}

	@Override
	public String getSource(final ModelArtifact artifact) {
		return sourceObject.getContent().toString();
	}

	@Override
	public ITokenPartitioning getTokens(final ModelArtifact artifact) {
		return (iTokenPartitioner != null)
		        ? iTokenPartitioner.doPartitioning(new StringContent(getSource(artifact)))
		        : TokenPartitioner2.create(iLexerFactory).doPartitioning(new StringContent(getSource(artifact)));
	}
}
