/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.data.core;

import static java.nio.charset.StandardCharsets.UTF_8;

import java.io.IOException;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;

import innowake.mining.shared.TestResourceUtil;

/**
 * Base class for tests, capable of loading a resource as string.
 */
public class TestWithResource {

	private static final Logger LOG = LoggerFactory.getLogger(TestWithResource.class);

	/**
	 * Load a resource with a given name (Must be in the same package) and return the content as string.
	 *
	 * @param name The resource name (including extension)
	 * @return The content as string.
	 */
	protected String getResource(final String name) {
		try {
			return TestResourceUtil.getContentNormalized(getClass().getPackageName().replace('.', '/') + "/" + name, UTF_8);
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}
	}

	protected void writeExpected(final String fileName, final String content) {
		try {
			final String name = getClass().getPackageName().replace('.', '/') + "/" + fileName;
			LOG.info(() -> String.format("Wrinting expected file: %s", fileName));
			TestResourceUtil.write(name, content, UTF_8);
		} catch (final IOException e) {
			throw new IllegalStateException(e);
		}
	}
}
