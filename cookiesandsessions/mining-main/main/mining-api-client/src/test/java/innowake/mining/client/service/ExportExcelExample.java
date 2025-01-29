/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client.service;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.Logging;
import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.io.ExportExcel;
import innowake.mining.client.service.io.IoServiceProvider;
import innowake.mining.shared.access.EntityId;

/**
 * Command line runner to test {@link ExportExcel}.
 */
public class ExportExcelExample {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.MINING_CLIENT_SERVICE);
	private static final String MARK = " >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>";
	private static final String ACCESS_TOKEN = "5COgpjv7HdUiRQrj0LwEsyiJnhE";
	
	public static void main(final String[] args) throws IOException {
		final ConnectionInfo connectionInfo = new ConnectionInfo("http://localhost:8080", ACCESS_TOKEN);
		final IoServiceProvider service = MiningApiClient.ioService(connectionInfo);

		LOG.info("export project with ID 1" + MARK);
		final Result<Tuple2<String, byte[]>> result = service.exportExcel().setProjectId(EntityId.of(1l)).execute();
		final Tuple2<String, byte[]> nameAndBytes = result.getValue().orElse(null);
		LOG.info("Status message: " + result.getStatusMessage());
		FileUtils.writeByteArrayToFile(new File(System.getProperty("user.dir"), nameAndBytes.a), nameAndBytes.b);
	}
}
