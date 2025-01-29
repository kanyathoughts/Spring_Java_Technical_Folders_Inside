/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.genai.requestresponse;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import innowake.mining.server.properties.GenericConfigProperties;

import java.util.Map;

/**
 * This class represents the JSON payload for the custom_prompt endpoint of the GenAI backend.
 */
public class CustomPromptRequest implements GenAiRequest {
	
	private static final String ENDPOINT = "custom_prompt";
	
	@JsonProperty("plugin")
	private final String plugin;

	@JsonProperty("max_new_tokens")
	private final int maxNewTokens;

	@JsonProperty("temperature")
	private final double temperature;

	@JsonProperty("do_sample")
	private final boolean doSample;
	
	@JsonProperty("prompt")
	private final String prompt;

	@JsonProperty("response_format")
	private final String responseFormat;

	@JsonProperty("request_metadata")
	private final Map<String, String> requestMetadata;

	/**
	 * Constructs a new CustomPromptRequest object.
	 *
	 * @param plugin The GenAI plugin to use, f.e. "azure gpt 4".
	 * @param prompt The prompt to be run.
	 * @param maxNewTokens The maximum number of new tokens to generate.
	 * @param temperature The temperature parameter for controlling the randomness.
	 * @param doSample Specifies whether to perform sampling.
	 * @param responseFormat The desired response format of the LLM response.
	 * @param requestMetadata Metadata used for cost-tracking.
	 */
	@JsonCreator
	public CustomPromptRequest(final String plugin, final String prompt, final int maxNewTokens, final double temperature, final boolean doSample,
							   final String responseFormat, final Map<String, String> requestMetadata) {
		this.plugin = plugin;
		this.maxNewTokens = maxNewTokens;
		this.temperature = temperature;
		this.doSample = doSample;
		this.prompt = prompt;
		this.responseFormat = responseFormat;
		this.requestMetadata = requestMetadata;
	}

	@Override
	public String toString() {
		return "{plugin=" + plugin + ", prompt=" + prompt + ", max_new_tokens=" +
					maxNewTokens + ", temperature=" + temperature +", do_sample=" + doSample +", response_format=" + responseFormat + "}";
	}

	@Override
	public String getUrlPath(final GenericConfigProperties configProperties) {
		return CustomPromptRequest.ENDPOINT;
	}

	public Map<String, String> getRequestMetadata() {
		return requestMetadata;
	}
}
