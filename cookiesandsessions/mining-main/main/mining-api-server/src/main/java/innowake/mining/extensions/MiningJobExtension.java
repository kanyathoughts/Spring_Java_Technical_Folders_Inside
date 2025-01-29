/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.extensions;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import innowake.mining.shared.access.EntityId;
import org.apache.commons.lang.StringUtils;
import org.springframework.http.HttpEntity;

import innowake.lib.job.api.Job;
import innowake.mining.server.controller.job.JobController;
import innowake.mining.shared.extensions.AccessRestrictedExtension;
import innowake.mining.shared.extensions.MiningExportExtension;
import innowake.mining.shared.extensions.ParameterizedExtension;
import innowake.mining.shared.extensions.ShowOnExportPageExtension;
import innowake.mining.shared.extensions.UploadExtension;
import innowake.mining.shared.io.ParameterDescription;
import innowake.mining.shared.io.ShowOnExportPage;
import innowake.mining.shared.io.UploadDescription;

/**
 * Interface for Mining extensions that contribute executable jobs.
 * <p>
 * Such an extension provides a job that runs within mining. The job can perform arbitrary
 * computations and may generate a result. The job will run within the mining-api-server's job framework,
 * so its status can be inspected via {@link JobController}. This kind of extension is therefore well suited for
 * long-running jobs. Extensions that generate a result more quickly should implement {@link MiningExportExtension} instead.
 * <p>
 * Classes that implement this interface must be put within (or underneath) the package
 * "innowake.mining.extensions". That way they will be picked up by Spring's component scan mechanism
 * and registered automatically.
 * @param <R> the result type of the created job
 */
public interface MiningJobExtension<R extends Serializable> extends AccessRestrictedExtension, ParameterizedExtension, UploadExtension,
		ShowOnExportPageExtension {

	/**
	 * An identifier for this extension.
	 * <p>
	 * This identifier must be unique because it is used when invoking the extension
	 * via the rest-api.
	 *
	 * @return a unique identifier
	 */
	String getIdentifier();

	/**
	 * Short descriptive text of this extension. This could be displayed on the UI where the user triggers the job.
	 *
	 * @return a short description or name of the extension
	 */
	String getDescription();

	/**
	 * Creates an executable Job with the given parameters. The job will be scheduled by the job framework.
	 * 
	 * @param projectId id of the project the job operates on
	 * @param parameters additional parameters for the job
	 * @return an executable Job
	 */
	@SuppressWarnings("unchecked")
	default Job<R> createJob(final EntityId projectId, final Map<String, List<String>> parameters) {
		return createJob(projectId, parameters, (HttpEntity<byte[]>) HttpEntity.EMPTY);
	}
	
	/**
	 * Creates an executable Job with the given parameters. The job will be scheduled by the job framework.
	 * <p>
	 * Supplying input data is optional, call {@linkplain HttpEntity#hasBody() inputData.hasBody()} to check
	 * if input data was supplied.
	 * 
	 * @param projectId id of the project the job operates on
	 * @param parameters additional parameters for the job
	 * @param inputData object containing optional input data for the job
	 * @return an executable Job
	 */
	Job<R> createJob(EntityId projectId, Map<String, List<String>> parameters, HttpEntity<byte[]> inputData);
	
	@Override
	default List<ParameterDescription> getParameterDescriptions() {
		return Collections.emptyList();
	}
	
	@Override
	default ShowOnExportPage getShowOnExportPage() {
		return new ShowOnExportPage(false, StringUtils.EMPTY, getDescription());
	}

	@Override
	default UploadDescription getUploadDescription() {
		return UploadDescription.notSupported();
	}
}
