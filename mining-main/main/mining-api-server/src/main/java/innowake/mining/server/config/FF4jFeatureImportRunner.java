/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.config;

import java.io.InputStream;

import org.ff4j.FF4j;
import org.ff4j.conf.XmlConfig;
import org.ff4j.core.Feature;
import org.ff4j.property.Property;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

/**
 * Used to initialize the ff4j feature and property store with predefined features from the file 'ff4j.xml'.
 * This xml file should be the central place to define available prototype / mvp features.
 * The initialization of these predefined features should only be done once the application starts.
 */
@Component
public class FF4jFeatureImportRunner implements CommandLineRunner {

	private static final String FF4J_XML = "ff4j.xml";
	
	@Autowired
	private FF4j ff4j;
	
	@Override
	public void run(final String... args) throws Exception {
		final InputStream xmlIN = getClass().getClassLoader().getResourceAsStream(FF4J_XML);
		if (xmlIN != null) {
			final XmlConfig xmlConfig = ff4j.parseXmlConfig(FF4J_XML);

			for (final String fuid : ff4j.getFeatures().keySet()) {
				if ( ! xmlConfig.getFeatures().containsKey(fuid)) {
					ff4j.delete(fuid);
				}
			}

			for (final Feature featureConfig : xmlConfig.getFeatures().values()) {
				if ( ! ff4j.exist(featureConfig.getUid())) {
					ff4j.createFeature(featureConfig);
				}
			}

			for (final Property<?> propertyConfig : xmlConfig.getProperties().values()) {
				if ( ! ff4j.getPropertiesStore().existProperty(propertyConfig.getName())) {
					ff4j.createProperty(propertyConfig);
				}
			}
		}
	}

}
