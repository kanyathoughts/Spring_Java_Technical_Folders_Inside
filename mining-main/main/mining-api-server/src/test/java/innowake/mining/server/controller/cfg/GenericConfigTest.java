/*
 * Copyright (c) 2024 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.controller.cfg;


import static org.junit.jupiter.api.Assertions.assertEquals;

import innowake.mining.server.genai.ResponseFormat;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.properties.ConfigurationProperties;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.server.properties.GenericConfigProperties;

/**
 * Tests whether the GenericProperties provides the correct data for the GenAI fields set in the application.yml
 */
@ConfigurationProperties(prefix = "configuration")
class GenericConfigTest extends DatabaseRelatedTest{
		
	@Autowired
	private GenericConfigProperties genericConfigProperties;
	
	@Nullable
	@Value("${mining.genAI.pathSegment.moduleDescription}")
	private String genAiModuleDescription;
	@Nullable
	@Value("${mining.genAI.pathSegment.annotationDescription}")
	private String genAiAnnotationDescription;
	
	/** 
	 * Tests the value saved in mining.genAI.pathSegment.moduleDescription 
	 */
	@Test
	void testGenAiModuleDefault() {
		String expectedString = "";
		if (genAiModuleDescription != null) {
			if ("".equals(genAiModuleDescription)) {
				expectedString = "/deduce";
			} else {
				expectedString = genAiModuleDescription;
			}
		}
		assertEquals(genericConfigProperties.getGenAiModuleDescription(), expectedString);
	}
	
	/**
	 * Tests the value saved in mining.genAI.pathSegment.annotationDescription 
	 */
	@Test
	void testGenAiAnnotationDefault() {
		String expectedString = "";
		if (genAiAnnotationDescription != null) {
			if ("".equals(genAiAnnotationDescription)) {
				expectedString = "/custom_prompt";
			} else {
				expectedString = genAiAnnotationDescription;
			}
		}
		assertEquals(genericConfigProperties.getGenAiAnnotationDescription(), expectedString);
	}

	/**
	 * Validates that the default value of GEN_AI_RESPONSE_FORMAT_CUSTOM_PROMPT_DEFAULT is set to ResponseFormat.TEXT
	 */
	@Test
	void testDefaultResponseFormatCustomPrompt() {
		assertEquals(genericConfigProperties.getDefaultResponseFormatCustomPrompt(), ResponseFormat.JSON);
	}

}
