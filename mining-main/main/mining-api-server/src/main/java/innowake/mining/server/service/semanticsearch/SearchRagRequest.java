package innowake.mining.server.service.semanticsearch;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.genai.requestresponse.GenAiRequest;
import innowake.mining.server.properties.GenericConfigProperties;

/**
 * Represents the JSON payload for the semantic-search-rag endpoint of the semantic-search service.
 */
public class SearchRagRequest implements GenAiRequest {

	private static final String ENDPOINT = "/semantic-search-rag";

	@JsonProperty("plugin")
	private final String plugin;
	@JsonProperty("max_new_tokens")
	private final Integer maxNewTokens;
	@JsonProperty("temperature")
	private final double temperature;
	@JsonProperty("do_sample")
	private final boolean doSample;
	@JsonProperty("query")
	private final String query;
	@Nullable
	@JsonInclude(JsonInclude.Include.NON_NULL)
	@JsonProperty("retriever_top_k")
	private final Integer retrieverTopK;

	/**
	 * Constructs a new SearchRagRequest instance.
	 *
	 * @param plugin the GenAI plugin, f.e. "azure gpt 3.5"
	 * @param maxNewTokens the amount of new tokens to be generated
	 * @param temperature the temperature to be used by the model
	 * @param doSample whether sampling is enabled for the model
	 * @param query the search query
	 * @param retrieverTopK top K value passed to the retriever
	 */
	public SearchRagRequest(final String plugin, final Integer maxNewTokens, final double temperature, final boolean doSample, final String query,
			@Nullable final Integer retrieverTopK) {
		this.plugin = plugin;
		this.maxNewTokens = maxNewTokens;
		this.temperature = temperature;
		this.doSample = doSample;
		this.query = query;
		this.retrieverTopK = retrieverTopK;
	}

	@Override
	public String toString() {
		return "{plugin=" + plugin + ", query=" + query + ", max_new_tokens=" + maxNewTokens + ", temperature=" + temperature + ", do_sample=" + doSample
				+ ", retriever_top_k=" + retrieverTopK + "}";
	}

	@Override
	public String getUrlPath(final GenericConfigProperties configProperties) {
		return 	SearchRagRequest.ENDPOINT;
	}
}
