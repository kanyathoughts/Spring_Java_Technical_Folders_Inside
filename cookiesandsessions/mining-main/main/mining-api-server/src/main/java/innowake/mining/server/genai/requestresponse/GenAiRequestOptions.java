/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.genai.requestresponse;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * This class represents the JSON payload for the endpoint that generates an annotation description using Generative AI.
 */
public class GenAiRequestOptions {

	private final String context;

	/**
	 * Constructs a new GenAiRequestOptions object.
	 *
	 * @param context The context value.
	 */
	@JsonCreator
	public GenAiRequestOptions(@JsonProperty("context") final String context) {
		this.context = context;
	}

	/**
	 * Returns the context value.
	 *
	 * @return The context value.
	 */
	public String getContext() {
		return context;
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder("GenAiRequestOptions{");
		sb.append("context='").append(context).append('\'');
		sb.append('}');
		return sb.toString();
	}
}
