package innowake.mining.server.discovery.metrics.generic.input;

import innowake.lib.core.lang.Assert;
import innowake.mining.data.model.discovery.ModelArtifact;

/**
 * Provides access to all available input types.  
 */
public interface MetricInputProvider {

	/**
	 * Maps a given {@link InputType} to the concrete input 
	 * 
	 * @param <T> the concrete java type of the input
	 * @param type the {@link InputType} to get the java type for
	 * @param artifact the artifact to get the input for 
	 * @return the concrete input 
	 */
	public default <T> T getInput(final InputType<T> type, final ModelArtifact artifact) {
		if (type == InputType.AST_MODEL) {
			final AstModelProvider modelProvider = Assert.assertInstanceOf(this, AstModelProvider.class);
			return type.getTypeClass().cast(modelProvider.getAstModel(artifact));
		} else if (type == InputType.SOURCE) {
			final SourceProvider sourceProvider = Assert.assertInstanceOf(this, SourceProvider.class);
			return type.getTypeClass().cast(sourceProvider.getSource(artifact));
		} else if (type == InputType.TOKEN) {
			final TokenProvider tokenProvider = Assert.assertInstanceOf(this, TokenProvider.class);
			return type.getTypeClass().cast(tokenProvider.getTokens(artifact));
		} else {
			throw new IllegalStateException("unhandled input type: " + type);
		}
	}
	
}
