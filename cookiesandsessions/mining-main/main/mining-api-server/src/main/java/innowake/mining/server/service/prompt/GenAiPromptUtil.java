/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.service.prompt;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Optional;

import com.fasterxml.jackson.databind.ObjectMapper;
import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.ModulePojo;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import org.apache.commons.io.IOUtils;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;

/**
 * Utility for building GenAI prompts.
 */
class GenAiPromptUtil {

	private static final Logger LOG = LoggerFactory.getLogger(GenAiPromptUtil.class);

	private GenAiPromptUtil() {
		/* Hide implicit constructor */
	}

	/**
	 * Builds a list of annotations for a prompt.
	 *
	 * @param annotations list of annotations
	 * @return the list of annotations, each entry containing the annotation description.
	 */
	public static String buildAnnotationList(final List<AnnotationPojo> annotations) {
		final var promptSb = new StringBuilder();
		var currentNum = 1;
		for (final AnnotationPojo annotation : annotations) {
			promptSb.append("## Annotation ")
					.append(currentNum++)
					.append("\n\n")
					.append(annotation.getName())
					.append("\n\n");
		}
		return promptSb.toString();
	}

	/**
	 * Builds a list of modules with descriptions for a prompt.
	 *
	 * @param modules list of modules, each module should have a description
	 * @return list of modules in string representation with each entry containing the module name and the module description
	 */
	public static String buildModulesListWithDescription(final List<ModulePojo> modules) {
		final var sb = new StringBuilder();
		for (final ModulePojo module : modules) {
			final Optional<String> description = module.getDescription();
			if (description.isPresent()) {
				sb.append("## ")
						.append(module.getName())
						.append("\n\n")
						.append(description.get())
						.append("\n\n");
			}
		}
		return sb.toString();
	}

	/**
	 * Builds a list of modules without descriptions for a prompt.
	 *
	 * @param modules list of modules
	 * @return list of modules in string representation with each entry containing the module name
	 */
	public static String buildModulesListWithoutDescription(final List<ModulePojo> modules) {
		final var sb = new StringBuilder();
		for (final ModulePojo module : modules) {
			sb.append("- ")
					.append(module.getName())
					.append("\n");
		}
		return sb.toString();
	}

	/**
	 *
	 * Loads a prompt text file from the /resources/genai/prompts directory and builds
	 * a {@linkplain Prompt} object.
	 *
	 * @param fileName the name of the prompt text file to load
	 * @return the {@linkplain Prompt} object
	 * @throws IOException if the specified file cannot be loaded
	 */
	public static Prompt loadPrompt(final String fileName) throws IOException {
		try {
			final Resource promptTextFile = new ClassPathResource("/genai/prompts/" + fileName);
			String promptText = "";
			if (promptTextFile.exists()) {
				LOG.debug(() -> "Loading prompt text file from /genai/prompts/" + fileName);
				try (final InputStream in = promptTextFile.getInputStream()) {
					promptText = IOUtils.toString(in, StandardCharsets.UTF_8);
				}
				return new ObjectMapper().readValue(promptText, Prompt.class);
			} else {
				throw new IOException("Prompt text file not found in /genai/prompts/" + fileName);
			}
		} catch (final IOException e) {
			LOG.error(() -> "Error loading prompt text file from /genai/prompts/" + fileName, e);
			throw e;
		}
	}
}
