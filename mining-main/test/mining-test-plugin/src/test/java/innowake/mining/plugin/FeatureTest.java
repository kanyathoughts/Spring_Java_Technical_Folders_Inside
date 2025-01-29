/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin;

import static innowake.mining.client.MiningApiClient.featureService;
import static innowake.mining.shared.model.FeatureId.ECLIPSE_DEEP_LINK;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Test;

import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.MiningServiceExecutor;

/**
 * Tests enabling and disabling feature and refreshing the feature cache.
 */
public class FeatureTest extends IntegrationBaseTest {
	
	/**
	 * Tests enabling and disabling feature and refreshing the feature cache.
	 *
	 * @throws InterruptedException if an error occurs
	 */
	@Test
	public void testEnableAndDisableFeature() throws InterruptedException {
		final ConnectionInfo connectionInfo = getConnectionInfo();
		/* enable feature */
		MiningServiceExecutor.create(() -> featureService(connectionInfo).toggleFeature().setFeatureId(ECLIPSE_DEEP_LINK).setState(Boolean.TRUE))
			.setInvalidResultConsumer(result -> fail(result.getExtendedStatusMessage()))
			.setExceptionConsumer(exception -> fail(exception.toString()))
			.execute();

		/* refresh cache */
		Features.INSTANCE.refreshCache(connectionInfo);
		/* wait until refresh job is done */
		Thread.sleep(10000);
		assertTrue(Features.INSTANCE.isEnabled(connectionInfo, ECLIPSE_DEEP_LINK));

		/* disable feature */
		MiningServiceExecutor.create(() -> featureService(connectionInfo).toggleFeature().setFeatureId(ECLIPSE_DEEP_LINK).setState(Boolean.FALSE))
			.setInvalidResultConsumer(result -> fail(result.getExtendedStatusMessage()))
			.setExceptionConsumer(exception -> fail(exception.toString()))
			.execute();
		
		/* cached feature is still enabled */
		assertTrue(Features.INSTANCE.isEnabled(connectionInfo, ECLIPSE_DEEP_LINK));
		
		/* force cache to load current (disabled) feature state */
		Features.INSTANCE.refreshCache(connectionInfo);
		/* wait until refresh job is done */
		Thread.sleep(10000);

		/* now cached feature is disabled */
		assertFalse(Features.INSTANCE.isEnabled(connectionInfo, ECLIPSE_DEEP_LINK));
	}
}
