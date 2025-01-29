package innowake.mining.server.genai.requestresponse;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Map;
public class BaseResponseModel {

	@JsonProperty("model_response")
	private final String modelResponse;

	@JsonProperty("prompt")
	private final String prompt;

	@JsonProperty("response_metadata")
	private final Map<String, String> responseMetadata;

	public String getModelResponse() {
		return modelResponse;
	}

	public String getPrompt() {
		return prompt;
	}

	public Map<String, String> getResponseMetadata() {
		return responseMetadata;
	}

	@JsonCreator
	public BaseResponseModel(@JsonProperty("model_response") final String modelResponse,
			@JsonProperty("prompt") final String prompt,
			@JsonProperty("response_metadata") final Map<String, String> responseMetadata) {
		this.modelResponse = modelResponse;
		this.prompt = prompt;
		this.responseMetadata = responseMetadata;
	}

}
