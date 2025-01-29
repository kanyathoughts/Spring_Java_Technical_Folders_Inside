/*
 * Copyright (c) 2019 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.client;

import innowake.mining.client.exceptions.StaleTokenException;
import innowake.mining.client.service.annotation.AnnotationServiceProvider;
import innowake.mining.client.service.annotationcategory.AnnotationCategoryServiceProvider;
import innowake.mining.client.service.candidate.CandidateServiceProvider;
import innowake.mining.client.service.client.ClientServiceProvider;
import innowake.mining.client.service.controlflow.ControlFlowServiceProvider;
import innowake.mining.client.service.datadictionary.DataDictionaryServiceProvider;
import innowake.mining.client.service.discovery.DiscoveryServiceProvider;
import innowake.mining.client.service.feature.FeatureServiceProvider;
import innowake.mining.client.service.info.InfoServiceProvider;
import innowake.mining.client.service.io.IoServiceProvider;
import innowake.mining.client.service.job.JobServiceProvider;
import innowake.mining.client.service.metamodel.MetamodelServiceProvider;
import innowake.mining.client.service.module.ModuleServiceProvider;
import innowake.mining.client.service.project.ProjectServiceProvider;
import innowake.mining.client.service.reference.ReferenceServiceProvider;
import innowake.mining.client.service.taxonomy.TaxonomyServiceProvider;
import innowake.mining.client.service.taxonomytype.TaxonomyTypeServiceProvider;
import innowake.mining.client.service.version.VersionServiceProvider;

/**
 * Access to {@code mining-api-server} REST API.
 * The {@link MiningServiceExecutor} should be used preferably since it triggers re-login and automatically retries the service calls in case user session
 * is expired or revoked. Otherwise manual re-login must be done before re-triggering the service call if {@link StaleTokenException} occurs.
 */
public final class MiningApiClient {

	private MiningApiClient() {}

	/**
	 * Access to {@link InfoServiceProvider}.
	 * 
	 * @param connectionInfo the connection info to use
	 * @return the service provider instance
	 */
	public static InfoServiceProvider infoService(final ConnectionInfo connectionInfo) {
		return new InfoServiceProvider(connectionInfo);
	}
	
	/**
	 * Access to {@link VersionServiceProvider}.
	 * 
	 * @param connectionInfo the connection info to use
	 * @return the service provider instance
	 */
	public static VersionServiceProvider versionService(final ConnectionInfo connectionInfo) {
		return new VersionServiceProvider(connectionInfo);
	}

	/**
	 * Access to {@link ClientServiceProvider}.
	 * 
	 * @param connectionInfo the connection info to use
	 * @return the service provider instance
	 */
	public static ClientServiceProvider clientService(final ConnectionInfo connectionInfo) {
		return new ClientServiceProvider(connectionInfo);
	}
	
	/**
	 * Access to {@link ProjectServiceProvider}.
	 * 
	 * @param connectionInfo the connection info to use
	 * @return the service provider instance
	 */
	public static ProjectServiceProvider projectService(final ConnectionInfo connectionInfo) {
		return new ProjectServiceProvider(connectionInfo);
	}
	
	/**
	 * Access to {@link ModuleServiceProvider}.
	 *
	 * @param connectionInfo the connection info to use
	 * @return the service provider instance
	 */
	public static ModuleServiceProvider moduleService(final ConnectionInfo connectionInfo) {
		return new ModuleServiceProvider(connectionInfo);
	}
	
	/**
	 * Access to {@link AnnotationCategoryServiceProvider}.
	 *
	 * @param connectionInfo the connection info to use
	 * @return the service provider instance
	 */
	public static AnnotationCategoryServiceProvider annotationCategoryService(final ConnectionInfo connectionInfo) {
		return new AnnotationCategoryServiceProvider(connectionInfo);
	}
	
	/**
	 * Access to {@link AnnotationServiceProvider}.
	 *
	 * @param connectionInfo the connection info to use
	 * @return the service provider instance
	 */
	public static AnnotationServiceProvider annotationService(final ConnectionInfo connectionInfo) {
		return new AnnotationServiceProvider(connectionInfo);
	}
	
	/**
	 * Access to {@link TaxonomyServiceProvider}.
	 *
	 * @param connectionInfo the connection info to use
	 * @return the service provider instance
	 */
	public static TaxonomyServiceProvider taxonomyService(final ConnectionInfo connectionInfo) {
		return new TaxonomyServiceProvider(connectionInfo);
	}
	
	/**
	 * Access to {@link TaxonomyTypeServiceProvider}.
	 *
	 * @param connectionInfo the connection info to use
	 * @return the service provider instance
	 */
	public static TaxonomyTypeServiceProvider taxonomyTypeService(final ConnectionInfo connectionInfo) {
		return new TaxonomyTypeServiceProvider(connectionInfo);
	}
	
	/**
	 * Access to {@link ReferenceServiceProvider}.
	 *
	 * @param connectionInfo the connection info to use
	 * @return the service provider instance
	 */
	public static ReferenceServiceProvider referenceService(final ConnectionInfo connectionInfo) {
		return new ReferenceServiceProvider(connectionInfo);
	}

	/**
	 * Access to {@link DataDictionaryServiceProvider}.
	 *
	 * @param connectionInfo the connection info to use
	 * @return the service provider instance
	 */
	public static DataDictionaryServiceProvider dataDictionaryService(final ConnectionInfo connectionInfo) {
		return new DataDictionaryServiceProvider(connectionInfo);
	}

	/**
	 * Access to {@link MetamodelServiceProvider}.
	 *
	 * @param connectionInfo the connection info to use
	 * @return the service provider instance
	 */
	public static MetamodelServiceProvider metaModelService(final ConnectionInfo connectionInfo) {
		return new MetamodelServiceProvider(connectionInfo);
	}
	
	/**
	 * Access to {@link CandidateServiceProvider}.
	 * 
	 * @param connectionInfo the connection info to use
	 * @return the service provider instance
	 */
	public static CandidateServiceProvider candidateService(final ConnectionInfo connectionInfo) {
		return new CandidateServiceProvider(connectionInfo);
	}
	
	/**
	 * Access to {@link JobServiceProvider}.
	 *
	 * @param connectionInfo the connection info to use
	 * @return the service provider instance
	 */
	public static JobServiceProvider jobService(final ConnectionInfo connectionInfo) {
		return new JobServiceProvider(connectionInfo);
	}
	
	/**
	 * Access to {@link IoServiceProvider}.
	 *
	 * @param connectionInfo the connection info to use
	 * @return the service provider instance
	 */
	public static IoServiceProvider ioService(final ConnectionInfo connectionInfo) {
		return new IoServiceProvider(connectionInfo);
	}
	
	/**
	 * Access to {@link FeatureServiceProvider}.
	 *
	 * @param connectionInfo the connection info to use
	 * @return the service provider instance
	 */
	public static FeatureServiceProvider featureService(final ConnectionInfo connectionInfo) {
		return new FeatureServiceProvider(connectionInfo);
	}
	
	/**
	 * Access to {@link ControlFlowServiceProvider}.
	 *
	 * @param connectionInfo the connection info to use
	 * @return the service provider instance
	 */
	public static ControlFlowServiceProvider controlFlowService(final ConnectionInfo connectionInfo) {
		return new ControlFlowServiceProvider(connectionInfo);
	}
	
	/**
	 * Access to {@link DiscoveryServiceProvider}.
	 *
	 * @param connectionInfo the connection info to use
	 * @return the service provider instance
	 */
	public static DiscoveryServiceProvider discoveryService(final ConnectionInfo connectionInfo) {
		return new DiscoveryServiceProvider(connectionInfo);
	}

}
