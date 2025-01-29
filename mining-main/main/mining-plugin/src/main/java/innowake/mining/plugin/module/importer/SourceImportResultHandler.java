/*
 * Copyright (c) 2020 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.module.importer;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import org.apache.http.HttpEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import innowake.base.eclipse.common.ui.util.WorkbenchUtil;
import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.lang.Assert;
import innowake.mining.client.service.CustomResponseHandler;
import innowake.mining.plugin.Logging;
import innowake.mining.shared.PojoMapper;
import innowake.mining.shared.io.MiningFileIndex;
import innowake.mining.shared.io.SecuredZipInputStream;
import innowake.mining.shared.model.job.ResultContainer;

/**
 * {@link CustomResponseHandler} that deserializes a zipped {@link MiningFileIndex} JSON.
 * <p>
 * This expects a {@code Content-Disposition} response header providing the file attachment to be downloaded.
 */
public class SourceImportResultHandler implements CustomResponseHandler<ResultContainer> {
	
	@Nullable
	@Override
	public ResultContainer handleResponse(final CloseableHttpResponse response) {
		final HttpEntity entity = response.getEntity();
		if (entity != null) {
			try (final InputStream content = Assert.assertNotNull(entity).getContent();
				final ZipInputStream zipIn = new SecuredZipInputStream(content, StandardCharsets.UTF_8)) {
				final ZipEntry zipEntry = zipIn.getNextEntry();
				if (zipEntry == null) {
					Logging.error("MiningFileIndex zip was empty. Unable to process local source files.");
				} else {
					return new ResultContainer(PojoMapper.jsonReaderFor(MiningFileIndex.class).readValue(zipIn));
				}
			} catch (final Exception exception) {
				final String error = "Error while processing SourceObject import result";
				Logging.error(error, exception);
				Display.getDefault().asyncExec(() -> MessageDialog.openError(WorkbenchUtil.getActiveShell(), "Source import",
														error + System.lineSeparator() + System.lineSeparator() + exception.getMessage()));
			}
		} else {
			final String error = "Response entity for SourceObject import was null";
			Logging.error(error);
			Display.getDefault().asyncExec(() -> MessageDialog.openError(WorkbenchUtil.getActiveShell(), "Source import", error));
		}
		
		return null;
	}
}
