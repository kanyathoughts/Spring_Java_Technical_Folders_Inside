package innowake.mining.server.genai.requestresponse;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import innowake.mining.server.properties.GenericConfigProperties;

import java.util.Map;

public class DeduceRequestModel implements GenAiRequest {

	@JsonProperty("plugin")
	private final String plugin;

	@JsonProperty("code")
	private final String code;

	@JsonProperty("max_new_tokens")
	private final int maxNewTokens;

	@JsonProperty("temperature")
	private final double temperature;

	@JsonProperty("do_sample")
	private final boolean doSample;

	@JsonProperty("language")
	private final String language;

	@JsonProperty("request_metadata")
	private final Map<String, String> requestMetadata;

	/**
	 * Constructs a new DeduceRequest object.
	 * @param plugin The plugin value.
	 * @param code The source code to be translated.
	 * @param maxNewTokens The maximum number of new tokens to generate during translation.
	 * @param temperature The temperature parameter for controlling the randomness of the translation.
	 * @param doSample Specifies whether to perform sampling during translation.
	 * @param language
	 * @param requestMetadata Metadata used for cost-tracking.
	 */
	@JsonCreator
	public DeduceRequestModel(final String plugin, final String code, final int maxNewTokens, final double temperature, final boolean doSample,
			final String language, final Map<String, String> requestMetadata) {
		this.plugin = plugin;
		this.code = code;
		this.maxNewTokens = maxNewTokens;
		this.temperature = temperature;
		this.doSample = doSample;
		this.language = language;
		this.requestMetadata = requestMetadata;
	}

	@Override
	public String toString() {
		return "{plugin=" + plugin + ", code=" + code + ", max_new_tokens=" + maxNewTokens + ", temperature=" + temperature + ", do_sample=" + doSample + ",language=" + language
				+ "}";
	}

	@Override
	public String getUrlPath(final GenericConfigProperties configProperties) {
		return configProperties.getGenAiModuleDescription();
	}
}
