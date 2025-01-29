/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.genai.requestresponse;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import innowake.mining.server.properties.GenericConfigProperties;

import java.util.Map;

/**
 * This class represents the JSON payload for the /documents endpoint of the GenAI Knowledge Service.
 */
public class KnowledgeServiceDocumentsRequest implements GenAiRequest {

	public static final String ENDPOINT = "/documents";

	@JsonProperty("category")
	private final String category;

	@JsonProperty("payload")
	private final String payload;

	@JsonProperty("request_metadata")
	private final Map<String, String> requestMetadata;


	/**
	 * Constructs a new KnowledgeServiceDocumentsRequest object.
	 *
	 * @param category The category to be queried.
	 * @param payload The payload to be used for querying documents.
	 * @param requestMetadata Metadata used for cost-tracking.
	 */
	@JsonCreator
	public KnowledgeServiceDocumentsRequest(final String category, final String payload, final Map<String, String> requestMetadata) {
		this.category = category;
		this.payload = payload;
		this.requestMetadata = requestMetadata;
	}

	@Override
	public String toString() {
		return "{category=" + category + ", payload='" + payload + "'}";
	}

	@Override
	public String getUrlPath(final GenericConfigProperties configProperties) {
		return KnowledgeServiceDocumentsRequest.ENDPOINT;
	}
}
