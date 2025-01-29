/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.genai;

/**
 * Additional prompt context information.
 */
public class AiGenerationContext {
	/**
	 * Empty context.
	 */
	public static final AiGenerationContext EMPTY = new AiGenerationContext();

	private final String additionalPromptContext;

	/**
	 * Constructor.
	 */
	public AiGenerationContext() {
		this.additionalPromptContext = "";
	}

	/**
	 * Constructor.
	 *
	 * @param additionalPromptContext the context
	 */
	public AiGenerationContext(final String additionalPromptContext) {
		this.additionalPromptContext = additionalPromptContext;
	}

	/**
	 * Get the context.
	 *
	 * @return the context
	 */
	public String getAdditionalPromptContext() {
		return additionalPromptContext;
	}

}
