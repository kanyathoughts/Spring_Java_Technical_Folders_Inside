/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.ui.handler;

import static org.mockito.Mockito.times;

import org.junit.Test;
import org.mockito.Mockito;

import innowake.mining.plugin.discovery.categorize.AbstractDiscoverCodeTest;
import innowake.mining.plugin.module.importer.DiscoveryConfigurationsImporter;
/**
 * Tests for configuration file upload
 */
public class UploadConfigurationHandlerTest extends AbstractDiscoverCodeTest {
	
	@Override
	protected String getTestFolder() {
		return "undiscovered";
	}

	/**
	 * Test to check the config file upload happens all the time when the upload() is called.
	 *
	 */
	@Test
	public void testApiCallOnEveryUpload() {
		final DiscoveryConfigurationsImporter  discoveryConfigurationsImporter  = Mockito.spy(new DiscoveryConfigurationsImporter());
		final UploadConfigurationHandler uploadConfigurationHandler = new UploadConfigurationHandler(discoveryConfigurationsImporter);
		
		Mockito.doNothing().when(discoveryConfigurationsImporter).uploadConfigurations();
		uploadConfigurationHandler.upload(getTargetProject());
		uploadConfigurationHandler.upload(getTargetProject());
		/* Since we called the upload twice, and both the time api call is made */
		Mockito.verify(discoveryConfigurationsImporter, times(2)).uploadConfigurations();
	}
}

