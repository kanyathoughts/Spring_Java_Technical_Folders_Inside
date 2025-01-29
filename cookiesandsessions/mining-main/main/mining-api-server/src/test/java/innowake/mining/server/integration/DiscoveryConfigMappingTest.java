/*
 * Copyright (c) 2018 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.integration;

import static org.junit.Assert.assertEquals;

import java.net.URL;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.discovery.config.core.Config;
import innowake.mining.shared.discovery.config.core.KeyValuePair;
import innowake.mining.shared.discovery.config.core.Mapping;
import innowake.mining.shared.discovery.config.core.Property;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.ProjectPojoPrototype;


/**
 * Tests the XML representation of the Discovery Config.
 */
public class DiscoveryConfigMappingTest extends DatabaseResettingTest {
	
	private final Long ONE = Long.valueOf(1);
	
	private static final Charset WORKSPACE_CHARSET = Charset.forName("cp1252");
	
	private DiscoveryConfigMappingTest() {}
	
	/**
	 * This test checks if expected configuration is equals to default configuration.
	 *
	 * @throws Exception will be thrown in case of resource not found or serialization
	 */
	@Test
	void testSerialization() throws Exception {
		final URL resource = DiscoveryConfigMappingTest.class.getResource("/expected-config.xml");
		final Path path = Paths.get(resource.toURI());
		
		final String expected = new String(Files.readAllBytes(path), WORKSPACE_CHARSET);
		
		assertEquals(expected, Config.serializeConfig(Config.getDefaultConfig()));
	}
	
	/**
	 * This tests checks whether the default config that gets written to the project, is the
	 * same as {@link Config#getDefaultConfig()}.
	 *
	 * @throws Exception if something throws in the test
	 */
	@Test
	void testDeserialization() throws Exception {	
		final ProjectPojo TEST_PROJECT_1 = projectService.create(new ProjectPojoPrototype()
				.setName("TEST PROJECT 1")
				.setClient(EntityId.of(ONE))
				.setNatures(Collections.emptySet()));
		
		final Config loadConfig = Config.loadConfig(projectService, TEST_PROJECT_1.identity());
		final List<Mapping> defaultMappings = Config.getDefaultConfig().getMappings();
		final List<Mapping> actualMappings = loadConfig.getMappings();
	
		/* Collect default properties from the properties Map */
		final List<Property> defaultProperties = Config.getDefaultConfig().getProperties().entrySet().stream()
				.map(p -> new Property( p.getKey(),  /* assign the Property type with the key from the properties map */
						p.getValue().entrySet().stream()  /* Each value (a <String, String> map) is used to create one KeyValuePair List */
						.flatMap(k -> k.getValue().stream()
								.map(n ->new KeyValuePair(k.getKey(),n)))  /* An entry in the value map corresponding to one KeyValuePair */
						.collect(Collectors.toList())
						))
				.collect(Collectors.toList());
		
		final List<Property> actualProperties = loadConfig.getProperties().entrySet().stream()
				.map(p -> new Property( p.getKey(),
						p.getValue().entrySet().stream()
						.flatMap(k -> k.getValue().stream()
								.map(n -> new KeyValuePair(k.getKey(),n)))
						.collect(Collectors.toList())
						))
				.collect(Collectors.toList());
		assertEquals(defaultMappings.size(), actualMappings.size());
		assertEquals(defaultProperties.size(), actualProperties.size());
		
		for (int i = 0; i < defaultMappings.size(); i++) {
			final Mapping expected = defaultMappings.get(i);
			final Mapping actual = actualMappings.get(i);
			
			assertEquals(expected.pattern, actual.pattern);
			assertEquals(expected.type, actual.type);
			assertEquals(expected.folder, actual.folder);
		}
		for (int i = 0; i < defaultProperties.size(); i++) {
			final Property expected = defaultProperties.get(i);
			final Property actual = actualProperties.get(i);
			
			assertEquals(expected.getLanguage(), actual.getLanguage());
			assertEquals(expected.getSettings(), actual.getSettings());
		}
		
	}

}
