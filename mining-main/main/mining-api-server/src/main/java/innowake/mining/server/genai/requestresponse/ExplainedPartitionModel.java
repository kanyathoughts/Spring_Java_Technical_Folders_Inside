package innowake.mining.server.genai.requestresponse;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;

public class ExplainedPartitionModel {

	@JsonProperty("purpose")
	private final String purpose;

	@JsonProperty("name")
	private final String name;

	@JsonProperty("length")
	private final int length;

	@JsonProperty("offset")
	private final int offset;

	@JsonProperty("code")
	private final String code;

	@JsonProperty("entities")
	private final List<EntityModel> entities;

	@JsonCreator
	public ExplainedPartitionModel(@JsonProperty("purpose") final String purpose, @JsonProperty("name") final String name,
			@JsonProperty("length") final int length, @JsonProperty("offset") final int offset, @JsonProperty("code") final String code,
			@JsonProperty("entities") final List<EntityModel> entities) {
		this.purpose = purpose;
		this.name = name;
		this.length = length;
		this.offset = offset;
		this.code = code;
		this.entities = entities;
	}
}
