package innowake.mining.server.service.semanticsearch;

import java.beans.ConstructorProperties;
import java.util.Map;

/**
 * Represents a semantic search document, f.e. one module code snippet.
 */
public class SemanticSearchDocumentPojo {

	private final double score;
	private final String context;
	private final Map<String, String> meta;

	/**
	 * Constructs a SemanticSearchDocumentPojo instance
	 *
	 * @param score measures how well the document matches the query
	 * @param context description of the individual document
	 * @param meta additional meta information
	 */
	@ConstructorProperties({"score", "context", "meta"})
	public SemanticSearchDocumentPojo(final double score, final String context, final Map<String, String> meta) {
		this.score = score;
		this.context = context;
		this.meta = meta;
	}

	/**
	 * @return the score measuring how well this document fits the query
	 */
	@SuppressWarnings("unused") /* Required for JSON serialization */
	public double getScore() {
		return score;
	}

	/**
	 * @return the context information for this document
	 */
	@SuppressWarnings("unused") /* Required for JSON serialization */
	public String getContext() {
		return context;
	}

	/**
	 * @return additional meta information
	 */
	public Map<String, String> getMeta() {
		return meta;
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder("SemanticSearchDocumentPojo{");
		sb.append("score=").append(score);
		sb.append(", context='").append(context).append('\'');
		sb.append(", meta=").append(meta);
		sb.append('}');
		return sb.toString();
	}

}
