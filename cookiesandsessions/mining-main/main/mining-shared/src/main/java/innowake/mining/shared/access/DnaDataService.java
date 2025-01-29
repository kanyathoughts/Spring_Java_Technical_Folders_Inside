 /*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.shared.access;

import java.time.Instant;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Consumer;

import org.apache.commons.lang3.tuple.Triple;

import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.shared.entities.dna.DnaClusterAlgorithm;
import innowake.mining.shared.entities.dna.DnaCommunityPojo;
import innowake.mining.shared.entities.dna.DnaCommunityPojoPrototype;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.mining.shared.entities.dna.DnaSimilarityAlgorithm;
import innowake.mining.shared.entities.dna.DnaSimilarityPojo;
import innowake.mining.shared.entities.dna.DnaSimilarityPojoPrototype;
import innowake.mining.shared.entities.dna.DnaSnapshotPojo;
import innowake.mining.shared.entities.dna.DnaSnapshotPojoPrototype;
import innowake.mining.shared.entities.dna.DnaStringElementPojo;
import innowake.mining.shared.entities.dna.DnaStringElementPojoPrototype;
import innowake.mining.shared.entities.dna.DnaStringPojo;
import innowake.mining.shared.entities.dna.DnaStringPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Functions for accessing DNA related database entities.
 */
public interface DnaDataService {
	
	public interface DnaSnapshotOrderBuilder {
		/**
		 * Order DNA Snapshots by when they were updated.
		 * @param direction Ordering direction.
		 * @return This builder.
		 */
		DnaSnapshotOrderBuilder sortUpdated(SortDirection direction);
	}
	
	public interface DnaSnapshotInquiryBuilder extends DnaSnapshotOrderBuilder {
		/**
		 * Include DNA Snapshots of a certain Project.
		 * @param projectId ID of the Project the Snapshots belong to.
		 * @return This builder.
		 */
		DnaSnapshotInquiryBuilder ofProject(EntityId projectId);
		
		/**
		 * Include DNA Snapshots updated after a certain point in time.
		 * @param t Update time after which to include snapshots.
		 * @return This builder.
		 */
		DnaSnapshotInquiryBuilder withUpdateAfter(Instant t);
		
		/**
		 * Include DNA Snapshots updated at a certain point in time.
		 * @param t Update time of the snapshots to include.
		 * @return This builder.
		 */
		DnaSnapshotInquiryBuilder withUpdateAt(Instant t);
		
		/**
		 * Include DNA Snapshots updated after the Project#metrics_date
		 * @param projectId ID of the Project the Snapshots belong to.
		 * @return This builder.
		 */
		DnaSnapshotInquiryBuilder withUpdateAfterDiscoverMetrics(EntityId projectId);
	}

	public interface DnaCommunityOrderBuilder {
		/**
		 * Order DNA Communities by number of linked community modules.
		 * @param direction Ordering direction.
		 * @return This builder.
		 */
		DnaCommunityInquiryBuilder sortCount(final SortDirection direction);
	}

	public interface DnaCommunityInquiryBuilder extends DnaCommunityOrderBuilder {
		/**
		 * Include DNA Communities of a certain Project.
		 * @param projectId ID of the Project the Communities belong to.
		 * @return This builder.
		 */
		DnaCommunityInquiryBuilder ofProject(EntityId projectId);
		
		/**
		 * Include DNA Communities of a certain Snapshot.
		 * @param snapshot ID of the Snapshot the Communities belong to.
		 * @return This builder.
		 */
		DnaCommunityInquiryBuilder ofSnapshot(UUID snapshot);
		
		/**
		 * Include DNA Communities for a certain Sequencer.
		 * @param sequencer ID of the Sequencer the Communities were generated for.
		 * @return This builder.
		 */
		DnaCommunityInquiryBuilder ofSequencer(DnaSequencer sequencer);
		
		/**
		 * Include DNA Communities based on the similarity algorithm.
		 * @param similarityAlgo Similarity algorithm used in the generation of the Communities.
		 * @return This builder.
		 */
		DnaCommunityInquiryBuilder withSimilarityAlgorithm(DnaSimilarityAlgorithm similarityAlgo);
		
		/**
		 * Include DNA Communities based on the clustering algorithm.
		 * @param clusterAlgo Clustering algorithm used in the generation of the Communities.
		 * @return This builder.
		 */
		DnaCommunityInquiryBuilder withClusterAlgorithm(DnaClusterAlgorithm clusterAlgo);
		
		/**
		 * Include DNA Communities based on the cluster index.
		 * @param clusterIndex Index for clusters to include.
		 * @return This builder.
		 */
		DnaCommunityInquiryBuilder withClusterIndex(Integer clusterIndex);
		
		/**
		 * Include DNA Communities based on specific IDs.
		 * @param ids IDs of Communities to include.
		 * @return This builder.
		 */
		DnaCommunityInquiryBuilder withIds(Collection<UUID> ids);
		
		/**
		 * Include DNA Communities containing certain Modules.
		 * @param ids EntityIds of Modules.
		 * @return This builder.
		 */
		DnaCommunityInquiryBuilder containingModules(Collection<EntityId> ids);

		/**
		 * Include DNA Communities containing certain Modules.
		 * @param uids UIDs of Modules.
		 * @return This builder.
		 */
		DnaCommunityInquiryBuilder containingModulesByUid(Collection<UUID> uids);

		/**
		 * Include DNA Communities containing certain Modules.
		 * @param nids NIDs of Modules
		 * @return This builder.
		 */
		DnaCommunityInquiryBuilder containingModulesByNid(Collection<Long> nids);
		
		/**
		 * Include DNA Communities containing certain Modules.
		 * @param pattern Module name pattern.
		 * @return This builder.
		 */
		DnaCommunityInquiryBuilder containingModuleWithName(String pattern);
		
		/**
		 * Include DNA Communities containing certain Modules.
		 * @param t Update time of the snapshots to include.
		 * @return This builder.
		 */
		DnaCommunityInquiryBuilder withUpdateAt(Instant t);
	}
	
	/**
	 * Creates a new Snapshot.
	 * @param snapshotBuilder Prototype of the Snapshot.
	 * @return UUID of the new Snapshot.
	 */
	public UUID createSnapshot(DnaSnapshotPojoPrototype snapshotBuilder);
	
	/**
	 * Updates a DNA Snapshot.
	 * @param snapshotBuilder Prototype of the Snapshot to update.
	 * @return UUID of the Snapshot.
	 */
	public UUID updateSnapshot(DnaSnapshotPojoPrototype snapshotBuilder);
	
	/**
	 * Retrieved a DNA Snapshot by it's UUID.
	 * @param id Unique ID of the Snapshot.
	 * @return Respective DNA Snapshot.
	 */
	public DnaSnapshotPojo getSnapshot(UUID id);
	
	/**
	 * Retrieves DNA Snapshots.
	 * @param builder Criteria for DNA Snapshots.
	 * @return Matching DNA Snapshots.
	 */
	public List<DnaSnapshotPojo> findSnapshots(BuildingConsumer<DnaSnapshotInquiryBuilder> builder);
	
	/**
	 * Retrieves the latest DNA Snapshot for a Project.
	 * @param projectId ID of the Project.
	 * @return Latest DNA Snapshot, if any.
	 */
	public Optional<DnaSnapshotPojo> latestSnapshot(EntityId projectId);
	
	/**
	 * Retrieves DNA communities. 
	 * @param builder Criteria for DNA Communities.
	 * @return Matching DNA Communities. 
	 */
	public List<DnaCommunityPojo> findCommunities(BuildingConsumer<DnaCommunityInquiryBuilder> builder);
	
	/**
	 * Retrieves the association of Modules with DNA Communities.
	 * @param builder Criteria for DNA Communities.
	 * @return Mapping of Module IDs to Communities they belong to. 
	 */
	public EntityMap<Set<DnaCommunityPojo>> findModuleClusters(BuildingConsumer<DnaCommunityInquiryBuilder> builder);
	
	/**
	 * Creates a DNA Community.
	 * @param community Prototype of the Community to create.
	 * @param isNew to check if dnaCommunity should be created or updated.
	 * @return ID of the new Community.
	 */
	public UUID createCommunity(DnaCommunityPojoPrototype community, boolean isNew);
	
	/**
	 * Associates Modules with a DNA Community.
	 * @param communityId ID of the Community.
	 * @param modules IDs of the Modules to add.
	 */
	public void putCommunityModules(UUID communityId, List<UUID> modules);
	
	/**
	 * Deletes a DNA snapshot with it's Communities.
	 * @param id ID of the Snapshot.
	 */
	public void deleteSnapshot(UUID id);
	
	/**
	 * Deletes a DNA Community.
	 * @param id ID of the Community.
	 */
	public void deleteCommunity(UUID id);

	/**
	 * Query builder interface for fetching {@linkplain DnaStringPojo DnaStringPojos} or {@code dna_string} record counts.
	 */
	public interface DnaStringInquiryBuilder {

		/**
		 * Filter {@code dna_string} records by {@linkplain DnaSequencer}.
		 *
		 * @param sequencer the sequencer filter
		 * @return this instance for method chaining
		 */
		DnaStringInquiryBuilder bySequencer(DnaSequencer sequencer);

		/**
		 * Filter {@code dna_string_element} records by module.
		 *
		 * @param moduleId the module filter
		 * @return this instance for method chaining
		 */
		DnaStringInquiryBuilder byModule(EntityId moduleId);

		/**
		 * Filter {@code dna_string} records by project.
		 *
		 * @param projectId the project filter
		 * @return this instance for method chaining
		 */
		DnaStringInquiryBuilder ofProject(EntityId projectId);

		/**
		 * Filter {@code dna_string} records by generation timestamp.
		 *
		 * @param generated the generation {@link Instant}
		 * @return this instance for method chaining
		 */
		DnaStringInquiryBuilder withGenerated(Instant generated);

		/**
		 * Filter {@code dna_string} records by minimum amount of {@code dna_string_element} records.
		 *
		 * @param minimumAmount the minimum amount of {@code dna_string_element} records
		 * @return this instance for method chaining
		 */
		DnaStringInquiryBuilder withMinimumAmount(Integer minimumAmount);
	}

	/**
	 * Query builder interface for fetching {@linkplain DnaStringElementPojo DnaStringElementPojos} or {@code dna_string_element} record counts.
	 */
	public interface DnaStringElementInquiryBuilder {

		/**
		 * Filter {@code dna_string_element} records by {@linkplain DnaSequencer}.
		 *
		 * @param sequencer the sequencer filter
		 * @return this instance for method chaining
		 */
		DnaStringElementInquiryBuilder bySequencer(DnaSequencer sequencer);

		/**
		 * Filter {@code dna_string_element} records by module.
		 *
		 * @param moduleId the module filter
		 * @return this instance for method chaining
		 */
		DnaStringElementInquiryBuilder byModule(EntityId moduleId);

		/**
		 * Filter {@code dna_string_element} records by modules UUIDs.
		 *
		 * @param moduleUids the module UUIDs to filter
		 * @return this instance for method chaining
		 */
		DnaStringElementInquiryBuilder byModules(Collection<UUID> moduleUids);
		
		/**
		 * Filter {@code dna_string_element} records by project.
		 *
		 * @param projectId the project filter
		 * @return this instance for method chaining
		 */
		DnaStringElementInquiryBuilder ofProject(EntityId projectId);
	}

	/**
	 * Query builder interface for fetching {@linkplain DnaSimilarityPojo DnaSimilarityPojos} or {@code dna_similarity} record counts.
	 */
	public interface DnaSimilarityInquiryBuilder {

		/**
		 * Filter {@code dna_similarity} records by {@linkplain DnaSequencer}.
		 *
		 * @param sequencer the {@link DnaSequencer} the records have been created with
		 * @return this instance for method chaining
		 */
		DnaSimilarityInquiryBuilder bySequencer(DnaSequencer sequencer);

		/**
		 * Filter {@code dna_similarity} records by module.
		 *
		 * @param moduleId the module filter
		 * @return this instance for method chaining
		 */
		DnaSimilarityInquiryBuilder byModule(EntityId moduleId);
		
		/**
		 * Filter {@code dna_similarity} records by project.
		 *
		 * @param projectId the ID of the {@code Project} the records have been created with
		 * @return this instance for method chaining
		 */
		DnaSimilarityInquiryBuilder ofProject(EntityId projectId);

		/**
		 * Filter {@code dna_similarity} records by {@linkplain DnaSimilarityAlgorithm}.
		 *
		 * @param algorithm the {@linkplain DnaSimilarityAlgorithm} the records have been created with
		 * @return this instance for method chaining
		 */
		DnaSimilarityInquiryBuilder withAlgorithm(DnaSimilarityAlgorithm algorithm);

		/**
		 * Filter {@code dna_similarity} records by similarity threshold.
		 *
		 * @param similarityThreshold the minimum similarity threshold the {@code dna_similarity} records must have
		 * @return this instance for method chaining
		 */
		DnaSimilarityInquiryBuilder withThreshold(double similarityThreshold);

		/**
		 * Filter {@code dna_similarity} records by minimum amount of having {@code dna_string_element} records.
		 *
		 * @param minAmount the minimum amount of {@code dna_string_element} records the {@code dna_similarity} records must have
		 * @return this instance for method chaining
		 */
		DnaSimilarityInquiryBuilder withMinimumAmount(int minAmount);
	}

	/**
	 * Creates a new {@code dna_string} record for the given {@code dnaString}.
	 *
	 * @param dnaString the {@linkplain DnaStringPojoPrototype} to create the record from.
	 * @param dnaStringElements the list of {@linkplain DnaStringElementPojoPrototype DnaStringElementPojoPrototypes} the {@code dna_string_element} 
	 * records are created from
	 */
	void createDnaString(DnaStringPojoPrototype dnaString, List<DnaStringElementPojoPrototype> dnaStringElements);

	/**
	 * Creates new {@code dna_similarity} records out of the given {@code dnaSimilarities}.
	 *
	 * @param dnaSimilarities a {@linkplain DnaSimilarityPojoPrototype DnaSimilarityPojoPrototypes} to create
	 * @param batchConsumer a callback after each batch operation completes, that is called with the number of inserted records
	 */
	void create(List<DnaSimilarityPojoPrototype> dnaSimilarities, Optional<Consumer<Integer>> batchConsumer);

	/**
	 * Deletes all {@code dna_string}, {@code dna_string_element} and {@code dna_similarity} records for the given {@code projectId} and {@code generated}
	 * timestamp.
	 *
	 * @param projectId the ID of the {@code Project} the records have been created with
	 * @param generated the {@linkplain Instant} when the records were created
	 */
	void deleteDnaStrings(EntityId projectId, Instant generated);

	/**
	 * Returns the {@link UUID UUIDs} of all matching {@code dna_string} records for the filters in the given {@code builder}.
	 *
	 * @param builder the {@link DnaStringInquiryBuilder} containing the filter criteria and sorting options.
	 * @return list of matching {@linkplain DnaStringPojo DnaStringPojos}
	 */
	List<UUID> findAllDnaStringUuids(BuildingConsumer<DnaStringInquiryBuilder> builder);

	/**
	 * Returns all {@code module} {@link UUID UUIDs} and {@code values} of all {@code dna_string_element} entities that match with the filters in the given
	 * {@code builder}.
	 *
	 * @param builder the {@linkplain DnaStringElementInquiryBuilder} containing the filter criteria
	 * @return list of {@link Tuple2} with module {@link UUID UUIDs} and string values
	 */
	List<Tuple2<UUID, String>> findDnaStringElementValues(BuildingConsumer<DnaStringElementInquiryBuilder> builder);

	/**
	 * Returns the {@linkplain DnaSequencer DnaSequencers} that were used for creating {@code dna_string} records in the given {@code projectId}.
	 *
	 * @param projectId the ID of the {@code Project} the {@linkplain DnaStringPojo DnaStringPojos} were created for
	 * @return list of {@linkplain DnaSequencer DnaSequencers}
	 */
	List<DnaSequencer> getSequencerIdsFromDnaString(EntityId projectId);

	/**
	 * Returns all {@code module} {@linkplain UUID UUIDs} for which {@code dna_string} records with the given {@code sequencer} and {@code project} exists but
	 * which are not present in any {@code dna_community} for the given {@code snapshotId} and {@code sequencer}.
	 *
	 * @param projectId the ID of the {@code Project} the records have been created with
	 * @param sequencer the {@link DnaSequencer} the records have been created with
	 * @param snapshotId the ID of their DNA snapshot
	 * @return list of {@code module} {@linkplain UUID UUIDs}
	 */
	List<UUID> getUnassignedModules(EntityId projectId, DnaSequencer sequencer, UUID snapshotId);

	/**
	 * Returns the number of {@code dna_string} records that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@link DnaStringInquiryBuilder} containing the filter criteria.
	 * @return number of matching {@code dna_string} records
	 */
	long getDnaStringCount(BuildingConsumer<DnaStringInquiryBuilder> builder);

	/**
	 * Returns the number of {@code dna_string_element} records that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@link DnaStringInquiryBuilder} containing the filter criteria.
	 * @return number of matching {@code dna_string} records
	 */
	long getDnaStringElementCount(BuildingConsumer<DnaStringElementInquiryBuilder> builder);

	/**
	 * @return number of matching {@code dna_community} records
	 */
	long getDnaCommunityCount();
	
	/**
	 * @return number of matching {@code dna_community_modules} records
	 */
	long getDnaCommunityModulesCount();

	/**
	 * Returns all {@linkplain DnaSimilarityPojo DnaSimilarityPojos} that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@link DnaSimilarityInquiryBuilder} containing the filter criteria
	 * @return List of matching similarities
	 */
	List<DnaSimilarityPojo> findDnaSimilarities(BuildingConsumer<DnaSimilarityInquiryBuilder> builder);

	/**
	 * Returns the number of {@code dna_similarity} records that match with the filters in the given {@code builder}.
	 *
	 * @param builder the {@link DnaSimilarityInquiryBuilder} containing the filter criteria
	 * @return List of matching similarities
	 */
	long getDnaSimilarityCount(BuildingConsumer<DnaSimilarityInquiryBuilder> builder);

	/**
	 * Returns a list of {@linkplain Triple Triples} of the the top three most similar {@code Module} IDs for the given {@code moduleId}, {@code sequencer} and
	 * {@code algorithm}.
	 * <p>
	 * The {@linkplain Triple Triples} are created of (in this order) the "of" Module path, the "to" Module path and the similarity value of the two modules
	 * </p>
	 * 
	 * @param moduleId the {@link UUID} of the {@code Module} the records have been created with
	 * @param sequencer the {@linkplain DnaSequencer}he records have been created with
	 * @param algorithm the {@linkplain DnaSimilarityAlgorithm} records have been created with
	 *
	 * @return list the top three most similar {@code Module} IDs
	 */
	List<Triple<String, String, Double>> findNeighboringModules(UUID moduleId, DnaSequencer sequencer, DnaSimilarityAlgorithm algorithm);

	/**
	 * Searches for all {@code dna_community} records that match with the given {@code similarity}, {@code sequencer} and {@code snapshotId} and returns a map
	 * of their {@code Module} {@link UUID} as keys and their cluster index as values.
	 *
	 * @param algorithm the {@linkplain DnaSimilarityAlgorithm} the records have been created with
	 * @param sequencer the {@link DnaSequencer} the records have been created with
	 * @param snapshotId the ID of their DNA snapshot
	 * @return map of{@code Module} {@link UUID} as keys and cluster index as values
	 */
	Map<UUID, Integer> getClusterIndexAndModuleRidBySnapshot(DnaSimilarityAlgorithm algorithm, DnaSequencer sequencer, UUID snapshotId);

	/**
	 * Deletes all DNA data for the given {@code projectId}.
	 * 
	 * @param projectId ID of the Project for which DNA data must be deleted
	 */
	void deleteDnaData(EntityId projectId);

	/**
	 * Deletes all {@code dna_string}, {@code dna_string_element} and {@code dna_similarity} records for the given {@code moduleIds}.
	 * 
	 * @param moduleUids the uids of the modules for which DNA data must be deleted
	 * @return the number of deleted {@code dna_string} records
	 */
	int deleteDnaStrings(Collection<UUID> moduleUids);

	/**
	 * Deletes all DNA data for the given {@code moduleIds}.
	 * 
	 * @param moduleUids the uids of the modules for which DNA data must be deleted
	 */
	void deleteDnaData(Collection<UUID> moduleUids);

	/**
	 * Returns if DNA was collected for the module with the given {@code moduleId}.
	 * 
	 * @param moduleId ID of the Module to check.
	 * @return {@code true} if DNA was collected. Otherwise {@code false}
	 */
	boolean hasDnaData(EntityId moduleId);
}
