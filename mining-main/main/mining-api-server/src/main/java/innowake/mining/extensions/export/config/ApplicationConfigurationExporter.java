package innowake.mining.extensions.export.config;

/* Copyright (c) 2022 Deloitte. All rights reserved. */

import innowake.mining.server.util.ApplicationConfigurationUtil;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.extensions.MiningExportExtension;
import innowake.mining.shared.io.ShowOnExportPage;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.stereotype.Service;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.List;
import java.util.Map;

import static java.nio.charset.StandardCharsets.UTF_8;

/**
 * Export extension which allows downloading the spring application configuration.
 */
@Service
public class ApplicationConfigurationExporter implements MiningExportExtension {

	@Autowired
	private ApplicationContext applicationContext;

	@Override
	public NatureType getRequiredNature() {
		return NatureType.MINING;
	}

	@Override
	public RoleType getRequiredRole() {
		return RoleType.ADMIN;
	}

	@Override
	public String getFormatIdentifier() {
		return "app-config-txt";
	}

	@Override
	public String getDescription() {
		return "Application configuration (Text format)";
	}

	@Override
	public ShowOnExportPage getShowOnExportPage() {
		return new ShowOnExportPage(true, "Development", getDescription());
	}

	@Override
	public ExportValue export(final EntityId projectId, final Map<String, List<String>> parameters) throws ExportException {
		final ConfigurableEnvironment env = (ConfigurableEnvironment) applicationContext.getEnvironment();
		final String config = ApplicationConfigurationUtil.getConfigAsString(env);

		return new ExportValue() {

			@Override
			public String getFileName() {
				return "application-configuration.txt";
			}

			@Override
			public String getContentType() {
				return "text/plain";
			}

			@Override
			public InputStream getInputStream() {
				return new ByteArrayInputStream(config.getBytes(UTF_8));
			}
		};
	}
}