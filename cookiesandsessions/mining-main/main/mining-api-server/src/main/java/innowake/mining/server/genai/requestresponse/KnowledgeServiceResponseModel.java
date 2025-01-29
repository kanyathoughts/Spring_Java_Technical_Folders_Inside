/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.genai.requestresponse;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import innowake.mining.server.genai.knowledgeservice.KnowledgeServiceResultObject;

import java.util.List;

/**
 * This class represents the JSON response payload of the GenAI Knowledge Service.
 */
public class KnowledgeServiceResponseModel {

	@JsonProperty("result")
	private final List<KnowledgeServiceResultObject> result;

	public List<KnowledgeServiceResultObject> getResult() {
		return result;
	}

	@JsonCreator
	public KnowledgeServiceResponseModel(@JsonProperty("result") final List<KnowledgeServiceResultObject> result) {
		this.result = result;
	}

	@Override
	public String toString() {
		return "{result=" + result + "}";
	}

}
