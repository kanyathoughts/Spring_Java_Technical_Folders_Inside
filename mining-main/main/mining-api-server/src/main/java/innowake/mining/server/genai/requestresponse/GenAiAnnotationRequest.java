/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.genai.requestresponse;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import innowake.mining.shared.entities.AnnotationPojoPrototype;

/**
 * This class represents the JSON payload for the endpoint that generates an annotation description using Generative AI.
 */
public class GenAiAnnotationRequest {
	@JsonProperty("annotation")
	private final AnnotationPojoPrototype annotation;

	@JsonProperty("options")
	private final GenAiRequestOptions options;

	/**
	 * Constructs a new GenAiAnnotationRequest object.
	 *
	 * @param annotation The annotation value.
	 * @param options The options value.
	 */
	@JsonCreator
	public GenAiAnnotationRequest(@JsonProperty("annotation") final AnnotationPojoPrototype annotation,
			@JsonProperty("options") final GenAiRequestOptions options) {
		this.annotation = annotation;
		this.options = options;
	}

	/**
	 * Returns the annotation value.
	 * @return The annotation value.
	 */
	public AnnotationPojoPrototype getAnnotation() {
		return annotation;
	}

	/**
	 * Returns the options value.
	 *
	 * @return The options value.
	 */
	public GenAiRequestOptions getOptions() {
		return options;
	}
}
