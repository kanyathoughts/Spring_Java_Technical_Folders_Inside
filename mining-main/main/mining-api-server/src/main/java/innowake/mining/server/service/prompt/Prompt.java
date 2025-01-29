/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.service.prompt;

import java.beans.ConstructorProperties;
import java.util.List;

/**
 * POJO Class representing a prompt as returned by the prompt service.
 */
class Prompt {
	private final String template;
	private final List<String> placeholders;
	private final String llmType;
	private final String useCase;

	@ConstructorProperties({"template", "placeholders", "llmType", "useCase"})
	public Prompt(final String template, final List<String> placeholders, final String llmType, final String useCase) {
		this.template = template;
		this.placeholders = placeholders;
		this.llmType = llmType;
		this.useCase = useCase;
	}

	public String getTemplate() {
		return template;
	}

	public List<String> getPlaceholders() {
		return placeholders;
	}

	public String getLlmType() {
		return llmType;
	}

	public String getUseCase() {
		return useCase;
	}
}
