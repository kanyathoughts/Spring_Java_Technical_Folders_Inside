/*
 * Copyright (c) 2023 Deloitte. All rights reserved.
 */
package innowake.mining.server.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;

import innowake.mining.server.config.Profiles;
import innowake.mining.server.controller.discovery.DiscoveryController;
import innowake.mining.server.integration.DatabaseRelatedTest;
import innowake.mining.shared.access.DnaDataService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.dna.DnaClusterAlgorithm;
import innowake.mining.shared.entities.dna.DnaCommunityPojo;
import innowake.mining.shared.entities.dna.DnaCommunityPojoPrototype;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.mining.shared.entities.dna.DnaSimilarityAlgorithm;
import innowake.mining.shared.entities.dna.DnaSnapshotPojoPrototype;

/**
 * Test {@link DiscoveryController} end points
 */
@AutoConfigureMockMvc
@ActiveProfiles(value = Profiles.NO_AUTH, inheritProfiles = false )
class DiscoveryControllerTests extends DatabaseRelatedTest {

	private final static Long PROJECT_ID = Long.valueOf(1);
	final static String DNA_TITLE = "Dna title";
	final static String DNA_DESCRIPTION = "Dna description";
	final static String UPDATED_DNA_TITLE = "It DNA community title";
	final static String UPDATED_DNA_DESCRIPTION = "It DNA community description";

	@Autowired
	private MockMvc mvc;

	@Autowired
	private DnaDataService dnaData;

	/**
	 * Parameterized test, to verify updated dna_community title and description.
	 * 
	 * @param title the title used while creation of {@link DnaCommunityPojo}
	 * @param description the description used while creation of {@link DnaCommunityPojo}
	 * @param updateTitle the title used for updating {@link DnaCommunityPojo}
	 * @param updateDescription the description used for updating {@link DnaCommunityPojo}
	 */
	private void test(final Optional<String> title, final Optional<String> description, final String updatedTitle,
			final Optional<String> updatedDescription) throws Exception {
		final UUID communityUuid = createDnaCommunity(title, description);
		mvc.perform(put("/api" + DiscoveryController.DNA_COMMUNITY_CLUSTER_NAME_DESCRIPTION_UPDATE_URL, PROJECT_ID, communityUuid.toString())
				.param("title", updatedTitle).param("description", updatedDescription.isPresent() ? updatedDescription.get() : null)).andDo(print())
				.andExpect(status().isOk()).andReturn();
		final List<DnaCommunityPojo> dnaCommunityList = dnaData.findCommunities(builder -> {
			builder.withIds(Collections.singleton(communityUuid));
		});
		assertEquals(communityUuid, dnaCommunityList.get(0).getId());
		assertEquals(UPDATED_DNA_TITLE, dnaCommunityList.get(0).getTitle());
		if (updatedDescription.isPresent()) {
			assertEquals(UPDATED_DNA_DESCRIPTION, dnaCommunityList.get(0).getDescription());
		}
		assertEquals(DnaSequencer.COBOL_METHOD_RULE, dnaCommunityList.get(0).getSequencerId());
		assertEquals(DnaSimilarityAlgorithm.WEIGHTED_LEVENSHTEIN, dnaCommunityList.get(0).getSimilarityAlgorithm());
		assertEquals(DnaClusterAlgorithm.LOUVAIN, dnaCommunityList.get(0).getClusterAlgorithm());
		assertEquals(Integer.valueOf(1), dnaCommunityList.get(0).getClusterIndex());
	}
	
	void testUpdateExistingDnaCommunityTitle() throws Exception {
		test(Optional.of(DNA_TITLE), Optional.empty(), UPDATED_DNA_TITLE, Optional.empty());
	}
	
	void testUpdateExistingDnaCommunityTitleWithDescription() throws Exception {
		test(Optional.empty(), Optional.of(DNA_DESCRIPTION), UPDATED_DNA_TITLE, Optional.of(UPDATED_DNA_DESCRIPTION));
	}
	
	void testUpdateDnaCommunityTitleAndDescription() throws Exception {
		test(Optional.empty(), Optional.empty(), UPDATED_DNA_TITLE, Optional.of(UPDATED_DNA_DESCRIPTION));
	}

	@Test
	void testDnaCommunityUuidNotFound() throws Exception {
		mvc.perform(put("/api" + DiscoveryController.DNA_COMMUNITY_CLUSTER_NAME_DESCRIPTION_UPDATE_URL, PROJECT_ID, UUID.randomUUID().toString()).param("title",
				DNA_TITLE)).andDo(print()).andExpect(status().isNotFound());
	}

	private UUID createDnaCommunity(final Optional<String> title, final Optional<String> description) {
		final UUID idOfSnapshot = createDnaSnapshot(EntityId.of(PROJECT_ID));

		final DnaCommunityPojoPrototype dnaCommunity =  new DnaCommunityPojoPrototype();
		dnaCommunity.setSnapshot(idOfSnapshot);
		dnaCommunity.setSequencerId(DnaSequencer.COBOL_METHOD_RULE);
		dnaCommunity.setSimilarityId(DnaSimilarityAlgorithm.WEIGHTED_LEVENSHTEIN);
		dnaCommunity.setClusterAlgorithmId(DnaClusterAlgorithm.LOUVAIN);
		if (title.isPresent()) {
			dnaCommunity.setTitle(title.get());
		}
		if (description.isPresent()) {
			dnaCommunity.setDescription(description.get());
		}
		dnaCommunity.setClusterIndex(Integer.valueOf(1));

		return dnaData.createCommunity(dnaCommunity, true);
	}

	private UUID createDnaSnapshot(final EntityId projectId) {
		final Map<String, Object> dnaConfig = new HashMap<>();
		dnaConfig.put("maxLevels", Integer.valueOf(5));
		dnaConfig.put("minDNALength", Integer.valueOf(20));
		dnaConfig.put("maxIterations", Integer.valueOf(10));
		dnaConfig.put("defaultTolerance", Double.valueOf(0.0001));
		dnaConfig.put("similarityThreshold", Double.valueOf(0.85));

		final DnaSnapshotPojoPrototype dnaSnapshot = new DnaSnapshotPojoPrototype()
							.setTotalModuleCount(Integer.valueOf(12))
							.setProjectId(projectId)
							.setDnaConfig(dnaConfig);

		return dnaData.createSnapshot(dnaSnapshot);
	}
}