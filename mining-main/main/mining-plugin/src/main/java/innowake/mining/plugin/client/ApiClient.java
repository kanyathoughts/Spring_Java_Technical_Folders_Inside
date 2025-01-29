/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.plugin.client;

import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.equinox.security.storage.StorageException;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.client.ConnectionInfo;
import innowake.mining.client.MiningServiceExecutor;
import innowake.mining.client.exceptions.StaleTokenException;
import innowake.mining.client.service.client.ClientServiceProvider;
import innowake.mining.client.service.feature.FeatureServiceProvider;
import innowake.mining.client.service.info.InfoServiceProvider;
import innowake.mining.client.service.job.JobServiceProvider;
import innowake.mining.client.service.project.ProjectServiceProvider;
import innowake.mining.client.service.version.VersionServiceProvider;
import innowake.mining.plugin.MiningPlugin;
import innowake.mining.plugin.client.VersionChecker.VersionResult;
import innowake.mining.plugin.preferences.MiningPreferences;
import innowake.mining.plugin.preferences.ProjectData;

/**
 * Access to {@link innowake.mining.client.MiningApiClient}.
 * The {@link MiningServiceExecutor} should be used preferably since it triggers re-login and automatically retries the service calls in case user session
 * is expired or revoked. Otherwise manual re-login must be done before re-triggering the service call if {@link StaleTokenException} occurs.
 */
public final class ApiClient {

	private ApiClient() {}

	/**
	 * Access to {@link InfoServiceProvider}.
	 * 
	 * @param project the project to receive the connection properties
	 * @return the service provider instance
	 * @throws CoreException if the preference store or project access failed or if the required mining preferences are not set
	 * @throws StorageException if the preference store or project access failed or if the required mining preferences are not set
	 */
	public static InfoServiceProvider infoService(@Nullable final IProject project) throws CoreException, StorageException {
		checkVersion(project);
		return innowake.mining.client.MiningApiClient.infoService(getConnectionInfo(project));
	}

	/**
	 * Access to {@link InfoServiceProvider}.
	 * 
	 * @param connectionInfo the connection properties to use
	 * @return the service provider instance
	 */
	public static InfoServiceProvider infoService(final ConnectionInfo connectionInfo) {
		checkVersion(connectionInfo);
		return innowake.mining.client.MiningApiClient.infoService(connectionInfo);
	}

	/**
	 * Access to {@link VersionServiceProvider}.
	 * 
	 * @param project the project to receive the connection properties
	 * @return the service provider instance
	 * @throws CoreException if the preference store or project access failed or if the required mining preferences are not set
	 * @throws StorageException if the preference store or project access failed or if the required mining preferences are not set
	 */
	public static VersionServiceProvider versionService(@Nullable final IProject project) throws CoreException, StorageException {
		return versionService(getConnectionInfo(project));
	}

	/**
	 * Access to {@link VersionServiceProvider}.
	 * 
	 * @param connectionInfo the connection properties to use
	 * @return the service provider instance
	 */
	public static VersionServiceProvider versionService(final ConnectionInfo connectionInfo) {
		/* Explicitly not doing a version check in the version service as this could potentially lead to an endless loop */
		return innowake.mining.client.MiningApiClient.versionService(connectionInfo);
	}

	/**
	 * Access to {@link ClientServiceProvider}.
	 * 
	 * @param project the project to receive the connection properties
	 * @return the service provider instance
	 * @throws CoreException if the preference store or project access failed or if the required mining preferences are not set
	 * @throws StorageException if the secure preference store or project access failed or if the required mining preferences are not set
	 */
	public static ClientServiceProvider clientService(@Nullable final IProject project) throws CoreException, StorageException {
		checkVersion(project);
		return innowake.mining.client.MiningApiClient.clientService(getConnectionInfo(project));
	}
	
	/**
	 * Access to {@link ClientServiceProvider}.
	 * 
	 * @param connectionInfo the connection properties to use
	 * @return the service provider instance
	 */
	public static ClientServiceProvider clientService(final ConnectionInfo connectionInfo) {
		checkVersion(connectionInfo);
		return innowake.mining.client.MiningApiClient.clientService(connectionInfo);
	}

	/**
	 * Access to {@link ProjectServiceProvider}.
	 * 
	 * @param project the project to receive the connection properties
	 * @return the service provider instance
	 * @throws CoreException if the preference store or project access failed or if the required mining preferences are not set
	 * @throws StorageException if the secure preference store or project access failed or if the required mining preferences are not set
	 */
	public static ProjectServiceProvider projectService(@Nullable final IProject project) throws CoreException, StorageException {
		checkVersion(project);
		return innowake.mining.client.MiningApiClient.projectService(getConnectionInfo(project));
	}
	
	/**
	 * Access to {@link ProjectServiceProvider}.
	 * 
	 * @param connectionInfo the connection properties to use
	 * @return the service provider instance
	 */
	public static ProjectServiceProvider projectService(final ConnectionInfo connectionInfo) {
		checkVersion(connectionInfo);
		return innowake.mining.client.MiningApiClient.projectService(connectionInfo);
	}

	/**
	 * Access to {@link ModuleServiceProvider}.
	 *
	 * @param project the project to receive the connection properties
	 * @return the service provider instance
	 * @throws CoreException if the preference store or project access failed or if the required mining preferences are not set
	 * @throws StorageException if the secure preference store or project access failed or if the required mining preferences are not set
	 */
	public static ModuleServiceProvider moduleService(final IProject project) throws CoreException, StorageException {
		checkVersion(project);
		return new ModuleServiceProvider(getConnectionInfo(project), getApiProject(project));
	}

	/**
	 * Access to {@link ModuleServiceProvider}.
	 *
	 * @param connectionInfo the connection properties to use
	 * @return the service provider instance
	 */
	public static ModuleServiceProvider moduleService(final ConnectionInfo connectionInfo) {
		checkVersion(connectionInfo);
		return new ModuleServiceProvider(connectionInfo);
	}

	/**
	 * Access to {@link AnnotationCategoryServiceProvider}.
	 *
	 * @param project the project to receive the connection properties
	 * @return the service provider instance
	 * @throws CoreException if the preference store or project access failed or if the required mining preferences are not set
	 * @throws StorageException if the secure preference store or project access failed or if the required mining preferences are not set
	 */
	public static AnnotationCategoryServiceProvider annotationCategoryService(final IProject project) throws CoreException, StorageException {
		checkVersion(project);
		return new AnnotationCategoryServiceProvider(getConnectionInfo(project), getApiProject(project));
	}
	
	/**
	 * Access to {@link AnnotationServiceProvider}.
	 *
	 * @param project the project to receive the connection properties
	 * @return the service provider instance
	 * @throws CoreException if the preference store or project access failed or if the required mining preferences are not set
	 * @throws StorageException if the secure preference store or project access failed or if the required mining preferences are not set
	 */
	public static AnnotationServiceProvider annotationService(final IProject project) throws CoreException, StorageException {
		checkVersion(project);
		return new AnnotationServiceProvider(getConnectionInfo(project), getApiProject(project));
	}
	
	/**
	 * Access to {@link AnnotationServiceProvider}.
	 *
	 * @param connectionInfo the connection properties to use
	 * @return the service provider instance
	 */
	public static AnnotationServiceProvider annotationService(final ConnectionInfo connectionInfo) {
		checkVersion(connectionInfo);
		return new AnnotationServiceProvider(connectionInfo);
	}
	
	/**
	 * Access to {@link TaxonomyServiceProvider}.
	 *
	 * @param project the project to receive the connection properties
	 * @return the service provider instance
	 * @throws CoreException if the preference store or project access failed or if the required mining preferences are not set
	 * @throws StorageException if the secure preference store or project access failed or if the required mining preferences are not set
	 */
	public static TaxonomyServiceProvider taxonomyService(final IProject project) throws CoreException, StorageException {
		checkVersion(project);
		return new TaxonomyServiceProvider(getConnectionInfo(project), getApiProject(project));
	}
	
	/**
	 * Access to {@link TaxonomyTypeServiceProvider}.
	 *
	 * @param project the project to receive the connection properties
	 * @return the service provider instance
	 * @throws CoreException if the preference store or project access failed or if the required mining preferences are not set
	 * @throws StorageException if the secure preference store or project access failed or if the required mining preferences are not set
	 */
	public static TaxonomyTypeServiceProvider taxonomyTypeService(final IProject project) throws CoreException, StorageException {
		checkVersion(project);
		return new TaxonomyTypeServiceProvider(getConnectionInfo(project), getApiProject(project));
	}
	
	/**
	 * Access to {@link ReferenceServiceProvider}.
	 *
	 * @param project the project to receive the connection properties
	 * @return the service provider instance
	 * @throws CoreException if the preference store or project access failed or if the required mining preferences are not set
	 * @throws StorageException if the secure preference store or project access failed or if the required mining preferences are not set
	 */
	public static ReferenceServiceProvider referenceService(final IProject project) throws CoreException, StorageException {
		checkVersion(project);
		return new ReferenceServiceProvider(getConnectionInfo(project), getApiProject(project));
	}

	/**
	 * Access to {@link ReferenceServiceProvider}.
	 *
	 * @param connectionInfo the connection properties to use
	 * @return the service provider instance
	 */
	public static ReferenceServiceProvider referenceService(final ConnectionInfo connectionInfo){
		checkVersion(connectionInfo);
		return new ReferenceServiceProvider(connectionInfo);
	}

	/**
	 * Access to {@link DataDictionaryServiceProvider}.
	 *
	 * @param project the project to receive the connection properties
	 * @return the service provider instance
	 * @throws CoreException if the preference store or project access failed or if the required mining preferences are not set
	 * @throws StorageException if the secure preference store or project access failed or if the required mining preferences are not set
	 */
	public static DataDictionaryServiceProvider dataDictionaryService(final IProject project) throws CoreException, StorageException {
		checkVersion(project);
		return new DataDictionaryServiceProvider(getConnectionInfo(project), getApiProject(project));
	}
	
	/**
	 * Access to {@link CandidateServiceProvider}.
	 *
	 * @param project the project to extract the connection properties from
	 * 
	 * @return the service provider instance
	 * 
	 * @throws CoreException if the preference store or project access failed or if the required mining preferences are not set
	 * @throws StorageException if the secure preference store or project access failed or if the required mining preferences are not set
	 */
	public static CandidateServiceProvider candidateService(final IProject project) throws CoreException, StorageException {
		checkVersion(project);
		return new CandidateServiceProvider(getConnectionInfo(project), getApiProject(project));
	}
	
	/**
	 * Access to {@link JobServiceProvider}.
	 * 
	 * @param project the project to extract the connection properties from or {@code null} to use the global workspace connection properties
	 * @return the service provider instance
	 * @throws CoreException if the preference store or project access failed or if the required mining preferences are not set
	 * @throws StorageException if the secure preference store or project access failed or if the required mining preferences are not set
	 */
	public static JobServiceProvider jobService(@Nullable final IProject project) throws CoreException, StorageException {
		checkVersion(project);
		return new JobServiceProvider(getConnectionInfo(project));
	}
	
	/**
	 * Access to {@link IoServiceProvider}.
	 * 
	 * @param project the project to extract the connection properties from or {@code null} to use the global workspace connection properties
	 * @return the service provider instance
	 * @throws CoreException if the preference store or project access failed or if the required mining preferences are not set
	 * @throws StorageException if the secure preference store or project access failed or if the required mining preferences are not set
	 */
	public static IoServiceProvider ioService(final IProject project) throws CoreException, StorageException {
		checkVersion(project);
		return new IoServiceProvider(getConnectionInfo(project), getApiProject(project));
	}
	
	/**
	 * Access to {@link IoServiceProvider}.
	 * 
	 * @param connectionInfo the connection properties to use
	 * @return the service provider instance
	 */
	public static IoServiceProvider ioService(final ConnectionInfo connectionInfo) {
		checkVersion(connectionInfo);
		return new IoServiceProvider(connectionInfo);
	}
	
	/**
	 * Access to {@link ControlFlowServiceProvider}.
	 * 
	 * @param project the project to extract the connection properties from or {@code null} to use the global workspace connection properties
	 * @return the service provider instance
	 * @throws CoreException if the preference store or project access failed or if the required mining preferences are not set
	 * @throws StorageException if the secure preference store or project access failed or if the required mining preferences are not set
	 */
	public static ControlFlowServiceProvider controlFlowService(final IProject project) throws CoreException, StorageException {
		checkVersion(project);
		return new ControlFlowServiceProvider(getConnectionInfo(project), getApiProject(project));
	}
	
	/**
	 * Access to {@link DiscoveryServiceProvider}.
	 * 
	 * @param project the project to extract the connection properties from or {@code null} to use the global workspace connection properties
	 * @return the service provider instance
	 * @throws CoreException if the preference store or project access failed or if the required mining preferences are not set
	 * @throws StorageException if the secure preference store or project access failed or if the required mining preferences are not set
	 */
	public static DiscoveryServiceProvider discoveryService(final IProject project) throws CoreException, StorageException {
		checkVersion(project);
		return new DiscoveryServiceProvider(getConnectionInfo(project), getApiProject(project));
	}
	
	/**
	 * Access to {@link DiscoveryServiceProvider}.
	 * 
	 * @param connectionInfo the connection properties to use
	 * @return the service provider instance
	 */
	public static DiscoveryServiceProvider discoveryService(final ConnectionInfo connectionInfo) {
		checkVersion(connectionInfo);
		return new DiscoveryServiceProvider(connectionInfo);
	}

	/**
	 * Access to {@link FeatureServiceProvider}.
	 * 
	 * @param connectionInfo the connection properties to use
	 * @return the service provider instance
	 */
	public static FeatureServiceProvider featureService(final ConnectionInfo connectionInfo) {
		checkVersion(connectionInfo);
		return new FeatureServiceProvider(connectionInfo);
	}

	/**
	 * Checks connection to server by trying to access info endpoint.
	 *
	 * @param project the project to receive the connection properties
	 * @return true if a connection could be established; false otherwise
	 */
	public static boolean checkConnection(@Nullable final IProject project) {
		checkVersion(project);
		final AtomicBoolean connected = new AtomicBoolean(false);
		MiningServiceExecutor
			.create(() -> infoService(project).info())
			.setValidResultConsumer(result -> connected.set(true))
			.execute();
		return connected.get();
	}

	/**
	 * Returns the {@link ConnectionInfo} either project specific or the workspace one.
	 * 
	 * @param project the {@link IProject} if available
	 * @return the {@link ConnectionInfo}
	 * @throws CoreException if no connection info could be resolved
	 * @throws StorageException if the preference store could not be accessed
	 */
	public static ConnectionInfo getConnectionInfo(@Nullable final IProject project) throws CoreException, StorageException {
		final Optional<ConnectionInfo> connectionInfo = project != null ? MiningPreferences.getConnectionInfo(project) : MiningPreferences.getConnectionInfo();
		if ( ! connectionInfo.isPresent()) {
			throw new CoreException(new Status(IStatus.ERROR, MiningPlugin.ID, "The required mining API server connection preferences are not set."));
		}
		return connectionInfo.get();
	}
	
	private static ProjectData getApiProject(final IProject project) throws CoreException {
		return MiningPreferences.getApiProject(project).orElseThrow(() -> new CoreException(new Status(IStatus.ERROR, MiningPlugin.ID, "The required mining project preferences are not set.")));
	}
	
	private static void checkVersion(final ConnectionInfo connectionInfo) {
		checkVersion(null, connectionInfo);
	}

	private static void checkVersion(@Nullable final IProject project) {
		checkVersion(project, null);
	}

	private static void checkVersion(@Nullable final IProject project, @Nullable final ConnectionInfo connectionInfo) {
		final VersionResult result = VersionChecker.checkVersion(project, connectionInfo, /* force check */ false);
		VersionResultHandler.handle(project, result.serverUrl, result);
	}
}
