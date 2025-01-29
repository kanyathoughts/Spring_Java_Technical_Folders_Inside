/*
 * Copyright (c) 2024 Deloitte. All rights reserved.
 */
package innowake.mining.server.service.prompt;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import innowake.mining.shared.entities.AnnotationPojo;
import innowake.mining.shared.entities.AnnotationPojoPrototype;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ModulePojoPrototype;
import innowake.mining.shared.entities.testing.AnnotationPojoDummy;
import innowake.mining.shared.entities.testing.ModulePojoDummy;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import java.io.IOException;

import java.util.Arrays;
import java.util.List;

/**
 * Test for {@linkplain GenAiPromptUtil}.
 */
class GenAiPromptUtilTest {

	@Test
	void testAnnotationList() {
		final AnnotationPojo annotation1 = AnnotationPojoDummy.build(new AnnotationPojoPrototype()
				.setName("This code checks if A is greater than B"));
		final AnnotationPojo annotation2 = AnnotationPojoDummy.build(new AnnotationPojoPrototype()
				.setName("This code checks if B is greater than A"));
		final String list = GenAiPromptUtil.buildAnnotationList(Arrays.asList(annotation1, annotation2));
		assertEquals("""
				## Annotation 1

				This code checks if A is greater than B

				## Annotation 2

				This code checks if B is greater than A

				""", list);
	}

	@Test
	void testModuleWithDescriptionList() {
		final ModulePojo moduleA = ModulePojoDummy.build(new ModulePojoPrototype()
				.setName("Module A")
				.setDescription("This module does stuff")
		);
		final ModulePojo moduleB = ModulePojoDummy.build(new ModulePojoPrototype()
				.setName("Module B")
				.setDescription("This module does different stuff")
		);

		final String list = GenAiPromptUtil.buildModulesListWithDescription(Arrays.asList(moduleA, moduleB));
		assertEquals("""
				## Module A

				This module does stuff

				## Module B

				This module does different stuff

				""", list);
	}

	@Test
	void testModuleWithoutDescriptionList() {
		final ModulePojo moduleC = ModulePojoDummy.build(new ModulePojoPrototype()
				.setName("Module C")
		);

		final String list = GenAiPromptUtil.buildModulesListWithoutDescription(List.of(moduleC));
		assertEquals("- Module C\n", list);
	}

	/**
	 * Tests that the prompts as defined in
	 * {@linkplain PromptType PromptType} are loaded correctly and contain all the placeholders.
	 * @param promptType the prompt type to test
	 * @throws IOException if the prompt file cannot be read
	 */
	@ParameterizedTest
	@EnumSource(PromptType.class)
	void testLoadPrompt(final PromptType promptType) throws IOException {
		final Prompt prompt = GenAiPromptUtil.loadPrompt(promptType.getFileName());
		assertEquals(promptType.getUseCase(), prompt.getUseCase());
		final String template = prompt.getTemplate();
		final List<String> placeholders = prompt.getPlaceholders();
		for (final String placeholder : placeholders) {
			assertTrue(template.contains("{" + placeholder + "}"));
		}
	}

	@Test
	void testLoadPromptFileNotFound() {
		final String fileName = "nonExistentFile.txt";

		assertThrows(IOException.class, () -> GenAiPromptUtil.loadPrompt(fileName));
	}

}
