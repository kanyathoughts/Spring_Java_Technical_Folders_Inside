/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.job.plugin.manager;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;
import java.util.concurrent.RunnableFuture;

import org.apache.commons.lang.StringUtils;
import org.apache.http.Header;
import org.apache.http.HttpEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;

import innowake.base.eclipse.common.ui.util.WorkbenchUtil;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.client.service.CustomResponseHandler;
import innowake.mining.job.plugin.MiningJobPlugin;
import innowake.mining.shared.model.job.ResultContainer;

/**
 * {@link CustomResponseHandler} implementation that will present a save dialog to enable the
 * user to locally save the file based result of the job.
 * <p>
 * This expects a {@code Content-Disposition} response header providing the file attachment to be downloaded.
 */
public class JobResultHandler implements CustomResponseHandler<ResultContainer> {
	
	private static final String CONTENT_DISPOSITION = "Content-Disposition";
	private static final String FILENAME_KEY = "filename=";
	private static final String ERROR_TITLE = "Unable to handle job result";
	
	@Nullable
	@Override
	public ResultContainer handleResponse(final CloseableHttpResponse response) {
		final HttpEntity entity = response.getEntity();
		if (entity == null) {
			MiningJobPlugin.handleError(ERROR_TITLE, "The server response doesn't contain a valid entity to process.", true);
			return null;
		}
		
		final String fileName = resolveFileName(response.getAllHeaders());
		if (StringUtils.isBlank(fileName)) {
			MiningJobPlugin.handleError(ERROR_TITLE, "Unable to resolve the filename of the job result.", true);
			return null;
		}
		
		final String fileLocation = getFileLocation(Assert.assertNotNull(fileName));
	    if (StringUtils.isNotBlank(fileLocation)) {
	    	try (final InputStream stream = Assert.assertNotNull(entity).getContent()) {
				Files.copy(stream, Paths.get(fileLocation), StandardCopyOption.REPLACE_EXISTING);
			} catch (final UnsupportedOperationException | IOException e) {
				MiningJobPlugin.handleError(ERROR_TITLE, e, true);
			}
	    }
		
		return null;
	}
	
	@Nullable
	private String resolveFileName(final Header[] headers) {
		for (final Header header : headers) {
			/* Content-Disposition: attachment;filename=<fileName> */
			if (header.getName().equalsIgnoreCase(CONTENT_DISPOSITION)) {
				final String headerVal = header.getValue();
				if (headerVal.contains(FILENAME_KEY)) {
					return headerVal.split("=")[1];
				}
			}
		}
		
		return null;
	}
	
	@Nullable
	private String getFileLocation(final String fileName) {
		/* As this code is executed inside an eclipse job, we have to show the file dialog explicitly in the UI thread. */
		final RunnableFuture<String> runnable = new FutureTask<>(() -> {
			final FileDialog saveDialog = new FileDialog(WorkbenchUtil.getActiveShell(), SWT.SAVE);
			saveDialog.setFileName(fileName);
			return saveDialog.open();
		});
		WorkbenchUtil.syncExec(WorkbenchUtil.getWorkbench().getDisplay(), runnable);

		String fileLocation = null;
		try {
			fileLocation = runnable.get();
		} catch (final ExecutionException e) {
			MiningJobPlugin.handleError(ERROR_TITLE, e.getCause(), true);
		} catch (final InterruptedException e) {
			MiningJobPlugin.handleError(ERROR_TITLE, e, true);
			Thread.currentThread().interrupt();
		}

		return fileLocation;
	}

}
