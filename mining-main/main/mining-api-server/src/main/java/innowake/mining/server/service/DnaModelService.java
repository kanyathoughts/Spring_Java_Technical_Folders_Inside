/* Copyright (c) 2021 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.service;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.server.cache.MiningCacheConfig.PROJECT_KEY_GENERATOR;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import innowake.lib.core.api.lang.Nullable;
import innowake.mining.data.annotation.ProjectIdArgument;
import innowake.mining.data.model.discovery.dna.ModelBelongsToClusters;
import innowake.mining.data.model.discovery.dna.ModelClustering;
import innowake.mining.data.model.discovery.dna.ModelDna;
import innowake.mining.data.model.discovery.dna.ModelDnaAlgorithm;
import innowake.mining.data.model.discovery.dna.ModuleInDNACluster;
import innowake.mining.server.discovery.dna.export.DnaModelBuilder;
import innowake.mining.shared.access.DnaDataService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.EntityMap;
import innowake.mining.shared.access.ModuleService;
import innowake.mining.shared.access.ModuleService.ModuleInquiryBuilder;
import innowake.mining.shared.access.Paged;
import innowake.mining.shared.access.Pagination;
import innowake.mining.shared.access.ProjectService;
import innowake.mining.shared.access.SortDirection;
import innowake.mining.shared.entities.ModulePojo;
import innowake.mining.shared.entities.ProjectPojo;
import innowake.mining.shared.entities.dna.DnaClusterAlgorithm;
import innowake.mining.shared.entities.dna.DnaCommunityPojo;
import innowake.mining.shared.entities.dna.DnaCommunityPojoPrototype;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.mining.shared.entities.dna.DnaSimilarityAlgorithm;
import innowake.mining.shared.entities.dna.DnaSnapshotPojo;

/**
 * Converts DNA data to the respective API model classes.
 */
@Service
public class DnaModelService {

	@Lazy
	@Autowired
	private ProjectService projectService;
	
	@Autowired
	private ModuleService moduleService;
	
	@Autowired
	private DnaDataService dnaData;

	/**
	 * Whenever "Discover DNA" is run, a new DNA Snapshot will be created or the existing DNA Snapshot will be updated with updatedOn date for the
	 * given project. Above DNA Snapshot will be linked with DNA Communities. A DNA Community will be used to create a {@link ModelDna}
	 * for the given project. This method retrieves the list of {@code updatedOn} dates of DNA Snapshots in the form of timestamp.
	 * Example: 2022-07-05 15:18:27 as 1657014507000
	 *
	 * @param projectId the id of the project for which to check the DNA results
	 * @return a list of updatedOn in DNA Snapshot in the form of timestamp
	 */
	public List<String> getListOfDnaSnapshots(final EntityId projectId) {
		return dnaData.findSnapshots(q -> q.ofProject(projectId)
											.withUpdateAfterDiscoverMetrics(projectId)
											.sortUpdated(SortDirection.DESCENDING))
						.stream()
						.map(s -> String.valueOf(s.getUpdated().toEpochMilli()))
						.collect(Collectors.toList());
	}

	/**
	 * Retrieves the DNA result with the given timestamp. The list of available timestamps can be obtained with {@link #getListOfDnaSnapshots(EntityId)}.
	 *
	 * @param projectId id of the project from which to retrieve the DNA result
	 * @param timestamp the updatedOn field in DNA Snapshot in the from of timestamp
	 * @return the {@link ModelDna} that was found for the given timestamp, or {@link Optional#empty()} if no such result exists
	 */
	public Optional<ModelDna> getDnaForSelectedTimestamp(final EntityId projectId, final String timestamp) {
		return getModelDna(dnaData.findSnapshots(q -> q.ofProject(projectId)
														.withUpdateAt(Instant.ofEpochMilli(Long.parseLong(timestamp)))
														.sortUpdated(SortDirection.DESCENDING)).stream().findFirst());
	}

	/**
	 * Update the Title and Description for DnaCommunity based on UUID.
	 *
	 * @param id the UUID of the {@link DnaCommunityPojo}
	 * @param title the title of the {@link DnaCommunityPojo}
	 * @param description the description of the {@link DnaCommunityPojo}
	 * @return UUID of updated DNA Community
	 */
	public UUID updateDnaCommunityTitleDescription(final String id, final String title, final Optional<String> description) {	
		final var dnaCommunity = new DnaCommunityPojoPrototype();
		dnaCommunity.setId(UUID.fromString(id));
		dnaCommunity.setTitle(title);
		if (description.isPresent()) {
			dnaCommunity.setDescription(description.get());
		}
		return dnaData.createCommunity(dnaCommunity, false);
	}

	/**
	 * Retrieves the latest DNA result. This method is a shortcut for calling {@link #getListOfDnaSnapshots(EntityId)} and then calling
	 * {@link #getDnaForSelectedTimestamp(EntityId, String)} with the latest timestamp.
	 * Additionally it checks if the DNA is executed after the last Discover metrics run. If DNA is not executed after the latest Discover metrics run, then
	 * it would not return DNA result.
	 *
	 * @param projectId id of the project from which to retrieve the DNA result
	 * @return the latest {@link ModelDna} or {@link Optional#empty()} is no DNA result exists for the project
	 */
	@Cacheable(cacheNames= "latestModelDna", cacheResolver = "cacheResolver", unless="#result == null", keyGenerator = PROJECT_KEY_GENERATOR)
	public Optional<ModelDna> getLatestDna(@ProjectIdArgument final EntityId projectId) {
		final ProjectPojo project = projectService.get(projectId);
		if (project.getMetricsDate() == null) {
			return Optional.empty();
		}
		final Optional<DnaSnapshotPojo> dnaSnapshot = dnaData.latestSnapshot(projectId);
		return dnaSnapshot.isPresent() && assertNotNull(project.getMetricsDate()).isBefore(dnaSnapshot.get().getUpdated())
				? getModelDna(dnaSnapshot) : Optional.empty();
	}

	/**
	 * Returns a page of {@link ModuleInDNACluster} by id of Project, {@code clusterIndex} , which is the index of the cluster in the
	 * {@linkplain ModelClustering#getClusters() list of clusters} that can be retrieved with {@link #getDnaForSelectedTimestamp(EntityId, String)} or
	 * {@link #getLatestDna(EntityId)} and {@code sequencerId} which is the {@code algorithm} that was used to produce the clustering
	 * (see {@link ModelClustering#getAlgorithm()}).
	 * <p>
	 * Since one DNA result can contain multiple {@linkplain ModelDna#getClusterings() clusterings}, the clustering must be identified via the
	 * {@code algorithm} that was used to produce the clustering (see {@link ModelClustering#getAlgorithm()}).
	 * <p>
	 * If the clusterIndex is null, then we will fetch the clusterIndex and record id of {@code module} from DNA Community by Project's id and
	 * {@link DnaSequencer} and after that fetch the {@code module} by using record ids and form {@link ModuleInDNACluster}s.
	 *
	 * @param projectId id of the project from which to retrieve the DNA result
	 * @param sequencerId the {@link DnaSequencer} of the algorithm that created the clustering for which to retrieve the module-in-cluster mapping
	 * @param clusterIndex id of the cluster
	 * @param pagination the {@link Pagination} entity contains the page number and size of page information and sorting conditions
	 * @param updatedTime the Updated time of snapshot
	 * @param builder builder that applies the GraphQl filter and sorting to the query
	 * @return a page of {@link ModuleInDNACluster} for the given algorithm and cluster's id
	 *         or {@link Collections#emptyList()} if no such clustering exists
	 */
	public Paged<ModuleInDNACluster> getModulesInClusters(
			final EntityId projectId, 
			final DnaSequencer sequencerId, 
			@Nullable final Integer clusterIndex,
			final Pagination pagination, 
			@Nullable final String updatedTime, 
			final Consumer<ModuleInquiryBuilder> builder) {
		final EntityMap<Set<DnaCommunityPojo>> mapOfModulesToClusters = dnaData.findModuleClusters(q -> {
			q.ofProject(projectId);
			q.ofSequencer(sequencerId);
			
			if (StringUtils.isNotBlank(updatedTime)) {
				q.withUpdateAt(Instant.ofEpochMilli(Long.parseLong(updatedTime)));
			}
			if (clusterIndex != null) {
				q.withClusterIndex(clusterIndex);
			}
		});
		if (mapOfModulesToClusters.hasNids()) {
			final Collection<Long> moduleNids = mapOfModulesToClusters.nidMap().keySet();
			final Paged<ModuleInDNACluster> modulesInDNACluster = moduleService.findModules(pagination, q -> {
					q.byNids(moduleNids);
					builder.accept(q);
				}).map(module -> {
					final DnaCommunityPojo dnaCommunity = mapOfModulesToClusters.get(assertNotNull(module.getUid())).get()
							.stream().findFirst().get();
					return new ModuleInDNACluster(module, Integer.valueOf(dnaCommunity.getClusterIndex()), dnaCommunity.getId());
				});
			
			return new Paged<>(modulesInDNACluster.getContent(), modulesInDNACluster.getLimit(), modulesInDNACluster.getOffset(),
								(long) moduleNids.size(),
								modulesInDNACluster.getFirstElement(), modulesInDNACluster.getLastElement());
		}

		return Paged.empty();
	}
	
	/**
	 * Retrieves the {@link ModelBelongsToClusters} for the given module. It returns
	 * based on the latest DnaSnapshot of the given Project.
	 *
	 * @param projectId the id of Project
	 * @param moduleId the id of {@link ModulePojo}
	 * @return the {@link ModelBelongsToClusters} that was found for the given
	 *         module, or {@link Optional#empty()} if no such result exists.
	 */
	public Optional<ModelBelongsToClusters> getModelBelongsToCluster(final EntityId projectId, final EntityId moduleId) {
		final List<ModelBelongsToClusters.ModelDnaCluster> modelDnaClusterList = new ArrayList<>();
		final Optional<DnaSnapshotPojo> snapshot = dnaData.latestSnapshot(projectId);
		if (moduleService.findAnyModule(b -> b.ofProject(projectId).byId(moduleId)).isPresent() && snapshot.isPresent()) {
			for (final DnaSimilarityAlgorithm similarityId : DnaSimilarityAlgorithm.values()) {
				for (final DnaSequencer sequencerId : DnaSequencer.values()) {
					dnaData.findCommunities(q -> q.ofSnapshot(snapshot.get().getId()).ofSequencer(sequencerId)
							.withSimilarityAlgorithm(similarityId).containingModules(Collections.singletonList(moduleId)))
						.forEach(c -> {
							final ModelDnaAlgorithm modelDnaAlgorithm = new ModelDnaAlgorithm(sequencerId, similarityId, DnaClusterAlgorithm.LOUVAIN);
							final ModelBelongsToClusters.ModelDnaCluster modelDnaCluster = new ModelBelongsToClusters()
									.new ModelDnaCluster(Integer.valueOf(c.getClusterIndex()), modelDnaAlgorithm);
							modelDnaClusterList.add(modelDnaCluster);
						});
				}
			}
		}
		if ( ! modelDnaClusterList.isEmpty()) {	
			return Optional.of(new ModelBelongsToClusters(moduleId, modelDnaClusterList));
		}
		return Optional.empty();
	}

	private Optional<ModelDna> getModelDna(final Optional<DnaSnapshotPojo> dnaSnapshot) {
		return dnaSnapshot.map(snapshot -> new DnaModelBuilder(dnaData).buildModel(snapshot));
	}
}
