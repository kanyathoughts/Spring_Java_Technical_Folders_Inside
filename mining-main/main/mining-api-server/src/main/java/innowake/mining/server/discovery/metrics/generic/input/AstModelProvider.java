package innowake.mining.server.discovery.metrics.generic.input;


import innowake.mining.data.model.discovery.ModelArtifact;
import innowake.ndt.core.parsing.ast.AstModel;

/**
 * Interface for getting the {@link AstModel} for a given {@link ModelArtifact}.
 * This interface should be implemented directly by a language contributor.
 */
public interface AstModelProvider extends MetricInputProvider {
	
	/**
	 * Return instance of {@link AstModel} for the given artifact. 
	 * 
	 * @param artifact the entry to get the model for
	 * @return {@link AstModel} for the given artifact
	 */
	public AstModel getAstModel(ModelArtifact artifact);
	
}
