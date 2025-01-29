/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.genai;

import innowake.mining.shared.entities.AnnotationPojoPrototype;

/**
 * Information for retrieving prompt context information for a given annotation.
 */
public class AnnotationContext {
	private final AnnotationPojoPrototype annotation;
	private AiGenerationContext generationContext;

	/**
	 * Constructor.
	 *
	 * @param annotation
	 * 		the annotation
	 * @param generationContext
	 * 		the previous context, if this is part of a chain
	 */
	public AnnotationContext(final AnnotationPojoPrototype annotation, final AiGenerationContext generationContext) {
		this.annotation = annotation;
		this.generationContext = generationContext;
	}

	/**
	 * Get the annotation.
	 *
	 * @return the annotation
	 */
	public AnnotationPojoPrototype getAnnotation() {
		return annotation;
	}

	/**
	 * Get the previous context.
	 *
	 * @return the context
	 */
	public AiGenerationContext getGenerationContext() {
		return generationContext;
	}

	/**
	 * Set the context.
	 *
	 * @param aiGenerationContext the context
	 */
	public void setGenerationContext(final AiGenerationContext aiGenerationContext) {
		this.generationContext = aiGenerationContext;
	}
}
