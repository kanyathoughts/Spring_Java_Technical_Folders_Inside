/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.extensions.export.profiling;

import innowake.lib.core.api.profiling.ProfilingFactory;
import innowake.mining.server.config.Profiles;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.extensions.MiningExportExtension;
import innowake.mining.shared.io.ShowOnExportPage;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;

/**
 * Export extension which allows downloading Profiling metrics in plain text format.
 */
@Component
@Profile(Profiles.PROFILING)
public class ProfilingMetricsTextExporter implements MiningExportExtension {

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
		return "profiling-metrics-text";
	}

	@Override
	public String getDescription() {
		return "Profiling Metrics (Text format)";
	}

	@Override
	public ShowOnExportPage getShowOnExportPage() {
		return new ShowOnExportPage(true, "DEVELOPMENT", getDescription());
	}

	@Override
	public ExportValue export(final EntityId projectId, final Map<String, List<String>> parameters) throws ExportException {
		return new ExportValue() {
			@Override
			public String getFileName() {
				return "metrics.txt";
			}

			@Override
			public String getContentType() {
				return "text/plain";
			}

			@Override
			public InputStream getInputStream() {
				return new ByteArrayInputStream(ProfilingFactory.getProfilingSession().getGlobalMetrics().toString().getBytes(StandardCharsets.UTF_8));
			}
		};
	}
}
