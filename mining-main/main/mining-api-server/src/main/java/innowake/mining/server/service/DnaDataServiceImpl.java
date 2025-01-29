/*
 * Copyright (c) 2023 innoWake gmbh Germany. All rights reserved.
 */
package innowake.mining.server.service;

import java.time.Instant;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Consumer;

import org.apache.commons.lang3.time.StopWatch;
import org.apache.commons.lang3.tuple.Triple;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.data.access.postgres.DnaClusterPgDao;
import innowake.mining.data.access.postgres.DnaSimilarityPgDao;
import innowake.mining.data.error.MiningEntityNotFoundException;
import innowake.mining.shared.access.DnaDataService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.access.EntitySetMap;
import innowake.mining.shared.entities.dna.DnaCommunityPojo;
import innowake.mining.shared.entities.dna.DnaCommunityPojoPrototype;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.mining.shared.entities.dna.DnaSimilarityAlgorithm;
import innowake.mining.shared.entities.dna.DnaSimilarityPojo;
import innowake.mining.shared.entities.dna.DnaSimilarityPojoPrototype;
import innowake.mining.shared.entities.dna.DnaSnapshotPojo;
import innowake.mining.shared.entities.dna.DnaSnapshotPojoPrototype;
import innowake.mining.shared.entities.dna.DnaStringElementPojoPrototype;
import innowake.mining.shared.entities.dna.DnaStringPojoPrototype;
import innowake.mining.shared.lang.BuildingConsumer;

/**
 * Access to DNA related data.
 */
@Service
public class DnaDataServiceImpl implements DnaDataService {

	private static final Logger LOG = LoggerFactory.getLogger(DnaDataServiceImpl.class);

	private final DnaClusterPgDao clusterDao;
	private final DnaSimilarityPgDao dnaSimilarityPgDao;

	@Autowired
	public DnaDataServiceImpl(@Qualifier("postgres") final JdbcTemplate jdbcTemplate) {
		clusterDao = new DnaClusterPgDao(jdbcTemplate);
		dnaSimilarityPgDao = new DnaSimilarityPgDao(jdbcTemplate);
	}
	
	@Override
	public void createDnaString(final DnaStringPojoPrototype dnaString, final List<DnaStringElementPojoPrototype> dnaStringElements) {
		dnaSimilarityPgDao.createDnaString(dnaString);
		if ( ! dnaStringElements.isEmpty()) {
			dnaSimilarityPgDao.createDnaStringElements(dnaString, dnaStringElements);
		}
	}

	@Override
	public void create(final List<DnaSimilarityPojoPrototype> dnaSimilarities, final Optional<Consumer<Integer>> batchConsumer) {
		dnaSimilarityPgDao.create(dnaSimilarities, batchConsumer);
	}

	@Override
	public void deleteDnaStrings(final EntityId projectId, final Instant generated) {
		LOG.debug(() -> "Begin deletion of DnaString, DnaStringElements and DnaSimilarity...");

		final StopWatch stopWatch = new StopWatch();
		stopWatch.start();

		final int cnt = dnaSimilarityPgDao.deleteDnaStrings(projectId, generated);

		LOG.debug(() -> String.format("Deletion of %d DNA strings took %s (H:mm:ss.SSS)", Integer.valueOf(cnt), stopWatch.toString()));
	}
	
	@Override
	public List<UUID> findAllDnaStringUuids(final BuildingConsumer<DnaStringInquiryBuilder> builder) {
		return dnaSimilarityPgDao.findAllDnaStringUuids(builder);
	}

	@Override
	public List<Tuple2<UUID, String>> findDnaStringElementValues(final BuildingConsumer<DnaStringElementInquiryBuilder> builder) {
		return dnaSimilarityPgDao.findDnaStringElementValues(builder);
	}

	@Override
	public List<DnaSequencer> getSequencerIdsFromDnaString(final EntityId projectId) {
		return dnaSimilarityPgDao.getSequencerIdsFromDnaString(projectId);
	}

	@Override
	public List<UUID> getUnassignedModules(final EntityId projectId, final DnaSequencer sequencer, final UUID snapshotId) {
		return dnaSimilarityPgDao.getUnassignedModules(projectId, sequencer, snapshotId);
	}

	@Override
	public long getDnaStringCount(final BuildingConsumer<DnaStringInquiryBuilder> builder) {
		return dnaSimilarityPgDao.getDnaStringCount(builder);
	}

	@Override
	public long getDnaStringElementCount(final BuildingConsumer<DnaStringElementInquiryBuilder> builder) {
		return dnaSimilarityPgDao.getDnaStringElementCount(builder);
	}
	
	@Override
	public long getDnaCommunityCount() {
		return clusterDao.getDnaCommunityCount();
	}

	@Override
	public long getDnaCommunityModulesCount() {
		return clusterDao.getDnaCommunityModulesCount();
	}

	@Override
	public List<DnaSimilarityPojo> findDnaSimilarities(final BuildingConsumer<DnaSimilarityInquiryBuilder> builder) {
		return dnaSimilarityPgDao.findDnaSimilarities(builder);
	}

	@Override
	public long getDnaSimilarityCount(final BuildingConsumer<DnaSimilarityInquiryBuilder> builder) {
		return dnaSimilarityPgDao.getDnaSimilarityCount(builder);
	}

	@Override
	public List<Triple<String, String, Double>> findNeighboringModules(final UUID moduleId, final DnaSequencer sequencer, final DnaSimilarityAlgorithm algorithm) {
		return dnaSimilarityPgDao.findNeighboringModules(moduleId, sequencer, algorithm);
	}

	@Override
	public List<DnaCommunityPojo> findCommunities(final BuildingConsumer<DnaCommunityInquiryBuilder> builder) {
		return clusterDao.findCommunities(builder);
	}

	@Override
	public UUID createCommunity(final DnaCommunityPojoPrototype community, final boolean isNew) {
		return clusterDao.putCommunity(community, isNew);
	}

	@Override
	public void putCommunityModules(final UUID communityId, final List<UUID> modules) {
		clusterDao.putCommunityModules(communityId, modules);
	}

	@Override
	public Optional<DnaSnapshotPojo> latestSnapshot(final EntityId projectId) {
		return clusterDao.latestSnapshot(projectId);
	}
	
	@Override
	public List<DnaSnapshotPojo> findSnapshots(final BuildingConsumer<DnaSnapshotInquiryBuilder> builder) {
		return clusterDao.findSnapshots(builder);
	}
	
	@Override
	public UUID createSnapshot(final DnaSnapshotPojoPrototype snapshot) {
		return clusterDao.putSnapshot(snapshot, true);
	}
	
	@Override
	public UUID updateSnapshot(final DnaSnapshotPojoPrototype snapshot) {
		return clusterDao.putSnapshot(snapshot, false);
	}
	
	@Override
	public void deleteSnapshot(final UUID id) {
		clusterDao.deleteSnapshot(id);
	}
	
	@Override
	public void deleteCommunity(final UUID id) {
		clusterDao.deleteCommunity(id);
	}
	
	@Override
	public DnaSnapshotPojo getSnapshot(UUID id) {
		return clusterDao.findSnapshot(id).orElseThrow(() -> new MiningEntityNotFoundException(DnaSnapshotPojo.class, id.toString()));
	}
	
	@Override
	public EntitySetMap<DnaCommunityPojo> findModuleClusters(BuildingConsumer<DnaCommunityInquiryBuilder> builder) {
		return new EntitySetMap<DnaCommunityPojo>().addAll(clusterDao.findCommunities(builder), DnaCommunityPojo::getModules);
	}

	@Override
	public Map<UUID, Integer> getClusterIndexAndModuleRidBySnapshot(final DnaSimilarityAlgorithm similarityAlgorithm, final DnaSequencer sequencer, final UUID snapshotId) {
		return clusterDao.getClusterIndexAndModuleRidBySnapshot(similarityAlgorithm, sequencer, snapshotId);
	}

	@Override
	public void deleteDnaData(final EntityId projectId) {
		clusterDao.deleteSnapshots(projectId);
		dnaSimilarityPgDao.deleteDnaStrings(projectId);
	}

	@Override
	public void deleteDnaData(final Collection<UUID> moduleNIds) {
		clusterDao.deleteDnaData(moduleNIds);
		dnaSimilarityPgDao.deleteDnaStrings(moduleNIds);
	}

	@Override
	public int deleteDnaStrings(final Collection<UUID> moduleUids) {
		return dnaSimilarityPgDao.deleteDnaStrings(moduleUids);
	}

	@Override
	public boolean hasDnaData(final EntityId moduleId) {
		return clusterDao.hasDnaCommunity(moduleId);
	}
}
