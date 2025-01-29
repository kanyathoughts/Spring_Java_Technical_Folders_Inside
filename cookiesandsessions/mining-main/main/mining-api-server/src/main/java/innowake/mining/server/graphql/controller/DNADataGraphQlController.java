/*
 * Copyright (c) 2021 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.graphql.controller;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.shared.security.NatureType.DISCOVERY;
import static innowake.mining.shared.security.RoleType.VIEWER;

import java.util.List;
import java.util.Map;
import java.util.Optional;

import innowake.mining.shared.access.ProjectService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.SchemaMapping;
import org.springframework.stereotype.Controller;

import graphql.execution.DataFetcherResult;
import graphql.schema.DataFetchingEnvironment;
import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.datapoints.FilterObjectService;
import innowake.mining.data.datapoints.SortObjectService;
import innowake.mining.data.model.discovery.dna.ModelCluster;
import innowake.mining.data.model.discovery.dna.ModelClustering;
import innowake.mining.data.model.discovery.dna.ModelDna;
import innowake.mining.data.model.discovery.dna.ModuleInDNACluster;
import innowake.mining.server.config.security.Nature;
import innowake.mining.server.config.security.Role;
import innowake.mining.server.graphql.MiningQueryMapping;
import innowake.mining.server.service.DnaModelService;
import innowake.mining.server.util.PaginationUtil;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.mining.shared.service.UserRoleService;

/**
 * Controller for the "dnaData" and "dnaModulesInCluster" GraphQL queries.
 */
@Controller
public class DNADataGraphQlController {

	@Autowired
	private DnaModelService dnaModelService;
	@Autowired
	private UserRoleService userRoleService;
	@Autowired
	private SortObjectService sortObjectService;
	@Autowired
	private FilterObjectService filterObjectService;
	@Autowired
	private ProjectService projectService;
	
	public static final String QUERY_NAME = "dnaModulesInCluster";

	/**
	 * Class to represent local context data for DNA data.
	 */
	private static class DnaDataLocalContext {
		public final DnaSequencer algorithm;

		public DnaDataLocalContext(final DnaSequencer algorithm) {
			this.algorithm = algorithm;
		}
	}
	
	/**
	 * Returns the DNA data for the provided project ID and time stamp
	 *
	 * @param projectId the Project ID
	 * @param snapshotId the snapshot ID to be retrieved
	 * @return the {@linkplain ModelDna DNA data}
	 */
	@MiningQueryMapping
	@Nature({DISCOVERY})
	@Role({VIEWER})
	@Nullable
	public DataFetcherResult<ModelDna> dnaData(@Argument final EntityId projectId, @Argument @Nullable final String snapshotId) {
		final Long projectNid = projectService.getNid(projectId);
		final ControllerLocalContext localContext  = new ControllerLocalContext(projectNid, userRoleService.getProjectIds(),
				userRoleService.getClientAdminIds(), userRoleService.isAdmin());
		final ModelDna modelDna;
		if (snapshotId == null) {
			modelDna = dnaModelService.getLatestDna(projectId).orElse(null);
		} else {
			modelDna = dnaModelService.getDnaForSelectedTimestamp(projectId, snapshotId).orElse(null);
		}
		return new DataFetcherResult.Builder<>(modelDna)
				.localContext(localContext)
				.build();
	}
	
	/**
	 * Returns the {@linkplain Page} of {@linkplain ModuleInDNACluster} for provided algorithm and cluster index.
	 *
	 * @param projectId the ID of the project the cluster belongs to
	 * @param algorithm the algorithm
	 * @param clusterIndex the cluster index
	 * @param page the page number
	 * @param size the page size
	 * @param filterObject the GraphQl filter object to apply to the query
	 * @param sortObject the sorting conditions
	 * @param updatedTime timestamp of snapshot
	 * @return the {@linkplain Page} of {@linkplain ModuleInDNACluster}
	 */
	@MiningQueryMapping
	@Nature({DISCOVERY})
	@Role({VIEWER})
	@Nullable
	public DataFetcherResult<Paged<ModuleInDNACluster>> dnaModulesInCluster(
			@Argument final EntityId projectId,
			@Argument final String algorithm,
			@Argument @Nullable final Integer clusterIndex, 
			@Argument @Nullable final Integer page, 
			@Argument @Nullable final Integer size,
			@Argument(name = "filterObject") @Nullable final Map<String, Object> filterObject,
			@Argument(name = "sortObject") @Nullable final List<Map<String, String>> sortObject,
			@Argument final String updatedTime) {
		final Long projectNid = projectService.getNid(projectId);
		final ControllerLocalContext localContext  = new ControllerLocalContext(projectNid, userRoleService.getProjectIds(),
				userRoleService.getClientAdminIds(), userRoleService.isAdmin());

		final Pagination pageable = size == null || size == 0 ? new Pagination(0, 0) : new Pagination((long) Optional.ofNullable(page).orElse(0) * size, size);
		final Paged<ModuleInDNACluster> modules = dnaModelService.getModulesInClusters(projectId, DnaSequencer.parse(algorithm),
				clusterIndex, pageable, updatedTime, q -> {
					if (filterObject != null && ! filterObject.keySet().isEmpty()) {
						filterObjectService.applyFilterObject(projectNid, "dnaModulesInCluster", filterObject, q);
					}
					final List<Map<String, String>> sortObjectNonNull = sortObject != null ? sortObject : List.of(Map.of("content_module_name", "ASC"));
					sortObjectService.applySortObject(projectNid, QUERY_NAME, sortObjectNonNull, q);
				});
		return new DataFetcherResult.Builder<>(modules)
				.localContext(localContext)
				.build();
	}

	/**
	 * Returns the DNA clusters for the provided {@linkplain ModelClustering clustering details}
	 *
	 * @param modelClustering the {@linkplain ModelClustering clustering details}
	 * @param env the environment for local context
	 * @return the DNA clusters
	 */
	@SchemaMapping(typeName = "ModelClustering")
	public DataFetcherResult<List<ModelCluster>> clusters(final ModelClustering modelClustering, final DataFetchingEnvironment env) {
		final ControllerLocalContext localContext = env.getLocalContext();
		return new DataFetcherResult.Builder<>(modelClustering.getClusters())
				.localContext(localContext.withLocalContext(
						new DnaDataLocalContext(DnaSequencer.parse(modelClustering.getAlgorithm().get("Sequencer")))))
				.build();
	}
	
	/**
	 * Returns the cluster index for the provided DNA cluster
	 *
	 * @param cluster the DNA cluster
	 * @param env the local context environment
	 * @return the cluster index
	 */
	@SchemaMapping(typeName = "ModelCluster")
	public Integer clusterIndex(final ModelCluster cluster, final DataFetchingEnvironment env) {
		return Integer.valueOf(cluster.getClusterIndex());
	}
	
	/**
	 * Query to return list of modules in provided cluster
	 *
	 * @param cluster the DNA cluster
	 * @param env the local context environment
	 * @param page The page number
	 * @param size The page size
	 * @param sortBy The sorting conditions
	 * @return {@linkplain List} of {@linkplain ModuleInDNACluster}
	 */
	@SchemaMapping(typeName = "ModelCluster")
	public Paged<ModuleInDNACluster> modules(
			final ModelCluster cluster, 
			final DataFetchingEnvironment env,
			@Argument @Nullable final Integer page, 
			@Argument @Nullable final Integer size, 
			@Argument @Nullable final List<String> sortBy) {
		final ControllerLocalContext localContext = env.getLocalContext();
		final DnaDataLocalContext dnaContext = assertNotNull(localContext.getLocalContext(DnaDataLocalContext.class));
		final Long projectId = localContext.getQueriedProjectId();
		final DnaSequencer algorithm = dnaContext.algorithm;
		final Integer clusterIndex = Integer.valueOf(cluster.getClusterIndex());
		
		final Pageable pageable = PaginationUtil.constructPageable(Optional.ofNullable(page).orElse(0), Optional.ofNullable(size).orElse(0),
				sortBy == null ? null : sortBy.toArray(new String[0]));
		final Pagination pagination = pageable.isUnpaged() ? new Pagination(0, 0) : new Pagination(pageable.getOffset(), pageable.getPageSize());
		return dnaModelService.getModulesInClusters(EntityId.of(projectId), algorithm, clusterIndex, pagination, null, q -> {});
	}
}
