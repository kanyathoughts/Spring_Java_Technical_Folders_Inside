package innowake.mining.server.genai.requestresponse;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class EntityModel {

	@JsonProperty("type")
	private final String type;

	@JsonProperty("name")
	private final String name;

	@JsonCreator
	private EntityModel(@JsonProperty("type") final String type, @JsonProperty("name") final String name) {
		this.type = type;
		this.name = name;
	}

}
