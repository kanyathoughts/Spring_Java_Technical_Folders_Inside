/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.ui.handler;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Optional;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.time.StopWatch;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.plugin.Logging;
import innowake.mining.plugin.base.ValidationException;
import innowake.mining.plugin.client.ApiClient;
import innowake.mining.shared.io.SecuredZipInputStream;


/**
 * Executes DownloadConfiguration on the Mining server.
 */
public class DownloadConfigurationHandler extends AbstractBaseHandler {
	
	public static final String CHECKSUM_FILE = "DiscoveryConfigurationChecksum";

	@Nullable
	@Override
	public Object execute(@Nullable ExecutionEvent event) throws ExecutionException {
		try {
			download(getProject());
		} catch (final ValidationException e) {
			throw new IllegalStateException(e);
		}
		return null;
	}
	
	/**
	 * Performs the actual download configuration.
	 * 
	 * @param project - Selected project.
	 */
	public void download(final IProject project) {
		final StopWatch watch = new StopWatch();
		watch.start();
		Logging.info("Downloading configuration");
		try {
			final Optional<byte[]> optionalByteArr = MiningServiceExecutor
					.create(() -> ApiClient.discoveryService(project).downloadConfiguration())
					.setExceptionConsumer(exception -> Logging.error(exception.getLocalizedMessage(), exception))
					.execute();
			if (optionalByteArr.isPresent()) {
				final InputStream targetStream = new ByteArrayInputStream(optionalByteArr.get());
				final ZipInputStream zis = new SecuredZipInputStream(targetStream);
				ZipEntry zipEntry = zis.getNextEntry();
				while (zipEntry != null) {
					final String configFileName = zipEntry.getName();
					final File configFile = new File(project.getLocation().toFile(), configFileName);
					try (final FileOutputStream fos = new FileOutputStream(configFile)) {
						IOUtils.copy(zis, fos);
					}
					zipEntry = zis.getNextEntry();
				}
				project.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
			}

		} catch (final IOException | CoreException e) {
			Logging.error(e.getLocalizedMessage(), e);
		}
		watch.stop();
		Logging.info(String.format("Overall downloading configuration took %s (H:mm:ss.SSS)", watch.toString()));
	}
}
