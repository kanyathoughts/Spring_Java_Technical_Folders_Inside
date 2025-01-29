/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.extensions.example;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Component;

import innowake.lib.job.api.Job;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.mining.extensions.MiningJobExtension;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.io.ParameterDescription;
import innowake.mining.shared.io.ShowOnExportPage;
import innowake.mining.shared.io.UploadDescription;
import innowake.mining.shared.security.NatureType;
import innowake.mining.shared.security.RoleType;

/**
 * Example implementation of {@link MiningJobExtension}
 */
@Component
public class ExampleJobExtension implements MiningJobExtension<HashMap<String, Serializable>> {

	private static class ExampleJob extends Job<HashMap<String, Serializable>> {

		private HashMap<String, Serializable> data;

		public ExampleJob(final HashMap<String, Serializable> data) {
			this.data = data;
		}

		@Override
		protected Result<HashMap<String, Serializable>> run(ProgressMonitor progressMonitor) {
			return new Result<>(data);
		}
	}

	@Override
	public NatureType getRequiredNature() {
		return NatureType.MINING;
	}

	@Override
	public RoleType getRequiredRole() {
		return RoleType.EDITOR;
	}

	@Override
	public ShowOnExportPage getShowOnExportPage() {
		return new ShowOnExportPage(true, "Example-Extension", "Example-Job-Extension");
	}

	@Override
	public String getIdentifier() {
		return "example-job";
	}

	@Override
	public String getDescription() {
		return "Example Job Extension";
	}

	@Override
	public Job<HashMap<String, Serializable>> createJob(final EntityId projectId, final Map<String, List<String>> parameters, final HttpEntity<byte[]> inputData) {
		final HashMap<String, Serializable> data = new HashMap<>();
		data.put("parameters", (Serializable) parameters);
		final String fileData;
		if (inputData.hasBody()) {
			fileData = new String(inputData.getBody());
		} else {
			fileData = "EMPTY FILE";
		}
		data.put("fileData", fileData);
		return new ExampleJob(data);
	}

	@Override
	public List<ParameterDescription> getParameterDescriptions() {
		return ParametersData.getAllParameterData();
	}

	@Override
	public UploadDescription getUploadDescription() {
		return UploadDescription.with("File",
				"JSON file containing data to be imported",
				false,
				".json,application/json,.txt");
	}

}
