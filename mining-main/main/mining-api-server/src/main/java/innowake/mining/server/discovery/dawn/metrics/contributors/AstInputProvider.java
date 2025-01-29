/* Copyright (c) 2023 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dawn.metrics.contributors;

import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.mining.server.discovery.metrics.generic.input.AstModelProvider;
import innowake.mining.shared.entities.SourcePojo;
import innowake.ndt.core.parsing.ILexerFactory;
import innowake.ndt.core.parsing.ITokenPartitioner;
import innowake.ndt.core.parsing.ast.AstModel;

/**
 * Class for inputProvider to get source, ITokenPartitioning and AstModelProvider
 */
public class AstInputProvider extends InputProvider implements AstModelProvider {

	private final AstModel astModel;

	/**
	 * Constructor.
	 *
	 * @param sourceObject the sourceObject
	 * @param iLexerFactory the lexerFactory
	 * @param astModel the {@link AstModel} of the sourceObject
	 */
	public AstInputProvider(final SourcePojo sourceObject, final ILexerFactory iLexerFactory, final AstModel astModel) {
		super(sourceObject, iLexerFactory);
		this.astModel = astModel;
	}
	
	public AstInputProvider(final SourcePojo sourceObject, final ILexerFactory iLexerFactory, final AstModel astModel,
			final ITokenPartitioner iTokenPartitioner) {
		super(sourceObject, iLexerFactory, iTokenPartitioner);
		this.astModel = astModel;
	}

	@Override
	public AstModel getAstModel(final ModelArtifact artifact) {
		return this.astModel;
	}
}
