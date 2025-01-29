/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dna;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

import innowake.mining.data.io.discovery.config.Configurable;
import innowake.mining.data.io.discovery.config.Configuration;

/**
 * Test if the DNA Configurations are applied properly.
 */
public class DnaDiscoveryConfigTest {
	
	protected static class ConfigurableTester extends Configurable {

		private int intTypeField = 1;
		private String stringTypeField = "DNA Discovery";
		private double doubleTypeField = 0.5;
		private boolean booleanTypeField = true;

		/**
		 * Create a new instance of the ConfigurableTester.
		 * 
		 * @param configuration The configuration to use.
		 */
		public ConfigurableTester(final String configuration) {
			super(() -> configuration);
		}

		/**
		 * Defines a field for testing configuration of integer type.
		 * 
		 * @param intTypeField field to test configuration of integer type
		 */
		@Configuration(name = "intTypeField", defaultValue = "1",
				comment = "An integer type field for testing configuration", title = "Integer type field for testing")
		public void setIntTypeField(final int intTypeField) {
			this.intTypeField = intTypeField;
		}

		/**
		 * Defines a field for testing configuration of string type.
		 * 
		 * @param stringTypeField field to test configuration of string type
		 */
		@Configuration(name = "stringTypeField", defaultValue = "DNA Discovery",
				comment = "A String type field for testing configuration", title = "String type field for testing")
		public void setStringTypeField(final String stringTypeField) {
			this.stringTypeField = stringTypeField;
		}

		/**
		 * Defines a field for testing configuration of double type.
		 * 
		 * @param doubleTypeField field to test configuration of double type
		 */
		@Configuration(name = "doubleTypeField", defaultValue = "0.5",
				comment = "A double type field for testing configuration", title = "Double type field for testing")
		public void setDoubleTypeField(final double doubleTypeField) {
			this.doubleTypeField = doubleTypeField;
		}

		/**
		 * Defines a field for testing configuration of boolean type.
		 * 
		 * @param booleanTypeField field to test configuration of boolean type
		 */
		@Configuration(name = "booleanTypeField", defaultValue = "true",
				comment = "A boolean type field for testing configuration", title = "Boolean type field for testing")
		public void setBooleanTypeField(final boolean booleanTypeField) {
			this.booleanTypeField = booleanTypeField;
		}

		/**
		 * Returns the integer type field to test configuration.
		 *
		 * @return the intTypeField
		 */
		public int getIntTypeField() {
			return intTypeField;
		}
		
		/**
		 * Returns the String type field to test configuration.
		 *
		 * @return the stringTypeField
		 */
		public String getStringTypeField() {
			return stringTypeField;
		}
		
		/**
		 * Returns the double type field to test configuration.
		 *
		 * @return the doubleTypeField
		 */
		public double getDoubleTypeField() {
			return doubleTypeField;
		}
		
		/**
		 * Returns the boolean type field to test configuration.
		 *
		 * @return the booleanTypeField
		 */
		public boolean getBooleanTypeField() {
			return booleanTypeField;
		}
	}
	
	/**
	 * Test case if configurations are loaded and applied also verifies the loaded field values.
	 */
	@Test
	public void testConfigurable() {
		final ConfigurableTester tester = new ConfigurableTester(
				"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\r\n" + 
				"<properties>\r\n" + 
				"    <property key=\"intTypeField\" value=\"20\" title=\"Integer type field for testing\">\r\n" + 
				"        <comment>An integer type field for testing configuration</comment>\r\n" + 
				"    </property>\r\n" + 
				"    <property key=\"stringTypeField\" value=\"Value set from configuration\" title=\"String type field for testing\">\r\n" + 
				"        <comment>A string type field for testing configuration</comment>\r\n" + 
				"    </property>\r\n" + 
				"    <property key=\"doubleTypeField\" value=\"0.75\" title=\"Double type field for testing\">\r\n" + 
				"        <comment>A double type field for testing configuration</comment>\r\n" + 
				"    </property>\r\n" + 
				"    <property key=\"booleanTypeField\" value=\"false\" title=\"Boolean type field for testing\">\r\n" + 
				"        <comment>A boolean type field for testing configuration</comment>\r\n" + 
				"    </property>\r\n" + 
				"</properties>");
		tester.loadAndApplyConfiguration();
		assertEquals(20, tester.getIntTypeField());
		assertEquals("Value set from configuration", tester.getStringTypeField());
		assertEquals(0.75, tester.getDoubleTypeField(), 0);
		assertEquals("false", Boolean.toString(tester.getBooleanTypeField()));
	}
	
	/**
	 * Test case if default configurations are applied when no properties are set it Xml.
	 */
	@Test
	public void testDefaultConfigurable() {
		final ConfigurableTester tester = new ConfigurableTester(
				"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\r\n" + 
				"<properties>\r\n" + 
				"</properties>");
		tester.loadAndApplyConfiguration();
		assertEquals(1, tester.getIntTypeField());
		assertEquals("DNA Discovery", tester.getStringTypeField());
		assertEquals(0.5, tester.getDoubleTypeField(), 0);
		assertEquals("true", Boolean.toString(tester.getBooleanTypeField()));
	}
	
	/**
	 * Test case to verify applied configuration combinations of Xml properties and default properties.
	 */
	@Test
	public void testCombinedConfigurable() {
		final ConfigurableTester tester = new ConfigurableTester(
				"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\r\n" +
				"<properties>\r\n" + 
				"    <property key=\"intTypeField\" value=\"20\" title=\"Integer type field for testing\">\r\n" + 
				"        <comment>An integer type field for testing configuration</comment>\r\n" + 
				"    </property>\r\n" + 
				"    <property key=\"booleanTypeField\" value=\"false\" title=\"Boolean type field for testing\">\r\n" + 
				"        <comment>A boolean type field for testing configuration</comment>\r\n" + 
				"    </property>\r\n" + 
				"</properties>");
		tester.loadAndApplyConfiguration();
		assertEquals(20, tester.getIntTypeField());
		assertEquals("DNA Discovery", tester.getStringTypeField());
		assertEquals(0.5, tester.getDoubleTypeField(), 0);
		assertEquals("false", Boolean.toString(tester.getBooleanTypeField()));
	}
}
