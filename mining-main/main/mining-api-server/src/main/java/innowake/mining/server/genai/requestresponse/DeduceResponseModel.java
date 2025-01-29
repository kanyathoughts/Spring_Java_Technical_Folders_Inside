package innowake.mining.server.genai.requestresponse;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;
import java.util.Map;


/**
 * This class represents the JSON payload for the endpoint that generate module description using Generative AI.
 */
public class DeduceResponseModel {
	@JsonProperty("purpose")
	private final String purpose;

	@JsonProperty("partitions")
	private final List<ExplainedPartitionModel> partitions;

	@JsonProperty("response_metadata")
	private final Map<String, String> responseMetadata;

	public String getPurpose() {
		return purpose;
	}

	public List<ExplainedPartitionModel> getPartitions() {
		return partitions;
	}

	public Map<String, String> getResponseMetadata() {
		return responseMetadata;
	}

	@JsonCreator
	public DeduceResponseModel(@JsonProperty("purpose") final String purpose,
			@JsonProperty("partitions") final List<ExplainedPartitionModel> partitions,
			@JsonProperty("response_metadata") final Map<String, String> responseMetadata) {
		this.purpose = purpose;
		this.partitions = partitions;
		this.responseMetadata = responseMetadata;
	}

	@Override
	public String toString() {
		return "DeduceResponseModel{" + "purpose='" + purpose + '\'' + ", partitions=" + partitions + ", responseMetadata=" + responseMetadata + '}';
	}

}
