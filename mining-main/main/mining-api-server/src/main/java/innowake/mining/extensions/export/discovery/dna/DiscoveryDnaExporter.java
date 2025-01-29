/* Copyright (c) 2022 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.extensions.export.discovery.dna;

import java.util.List;
import java.util.Map;
import org.springframework.http.HttpEntity;
import innowake.lib.job.api.FileSystemResult;
import innowake.lib.job.api.Job;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.io.ShowOnExportPage;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;

/**
 * Allows to execute Discovery Dna export as a Job.
 */
abstract class DiscoveryDnaExporter implements MiningJobExtension<FileSystemResult> {
		
	@Override
	public NatureType getRequiredNature() {
		return NatureType.DISCOVERY;
	}

	@Override
	public ShowOnExportPage getShowOnExportPage() {
		return new ShowOnExportPage(true, "Discovery DNA", getLabel());
	}

	@Override
	public RoleType getRequiredRole() {
		return RoleType.VIEWER;
	}
	
	/**
	 * Gets the files to be exported.
	 *
	 * @return the files to be exported
	 */
	protected abstract File[] getFiles();
	
	/**
	 * Gets the label to be shown on the export page.
	 *
	 * @return the label
	 */
	protected abstract String getLabel();

	@Override
	public Job<FileSystemResult> createJob(final EntityId projectId, final Map<String, List<String>> parameters, final HttpEntity<byte[]> inputData) {
		final File[] file = getFiles();
		return new DiscoveryDnaExporterJob(projectId, file);
	}

	enum File {

		NEIGHBORING_MODULES("neighboring modules", "Download Modules in cluster", "Produces a zip file containing the Discover Dna Modules in cluster");

		private final String name;
		private final String label;
		private final String description;

		File(final String name, final String label, final String description) {
			this.name = name;
			this.label = label;
			this.description = description;
		}
		
		/**
		 * Gets the name of the job.
		 *
		 * @return the name
		 */
		public String getName() {
			return name;
		}
		
		/**
		 * Gets the label to be shown on the export page.
		 *
		 * @return the label
		 */
		public String getLabel() {
			return label;
		}
		
		/**
		 * Gets the short description of the export job.
		 *
		 * @return the description
		 */
		public String getDescription() {
			return description;
		}
	}
}
