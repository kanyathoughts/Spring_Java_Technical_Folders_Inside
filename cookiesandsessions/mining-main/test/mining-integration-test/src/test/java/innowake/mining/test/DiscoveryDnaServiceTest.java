/* Copyright (c) 2020 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.test;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static innowake.mining.test.util.RestTemplateUtil.getHttpHeaders;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import com.google.gson.Gson;

import innowake.mining.client.MiningApiClient;
import innowake.mining.client.service.Result;
import innowake.mining.client.service.RouteConfiguration;
import innowake.mining.data.model.discovery.ModelAlgorithmOption;
import innowake.mining.data.model.discovery.dna.ModelBelongsToClusters;
import innowake.mining.data.model.discovery.dna.ModelBelongsToClusters.ModelDnaCluster;
import innowake.mining.data.model.discovery.dna.ModelCluster;
import innowake.mining.data.model.discovery.dna.ModelClustering;
import innowake.mining.data.model.discovery.dna.ModelDna;
import innowake.mining.data.model.discovery.dna.ModelDnaAlgorithm;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.dna.DnaClusterAlgorithm;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.mining.shared.entities.dna.DnaSimilarityAlgorithm;

/**
 * Integration unit tests for the Discovery service.
 */
class DiscoveryDnaServiceTest extends IntegrationTest {
	private static final Long ZERO = Long.valueOf(0);
	private static final Long ONE = Long.valueOf(1);
	private static final Long TWO = Long.valueOf(2);
	
	private static final String LIST_OF_DNA_SNAPSHOT_URL = RouteConfiguration.API_BASE + "/v1/projects/{projectId}/discovery/dna-snapshots";

	private static final String DNA_SNAPSHOT_FOR_SELECTED_TIMESTAMP_URL =
			RouteConfiguration.API_BASE + "/v1/projects/{projectId}/discovery/dna-snapshots/{timestamp}";

	private static final String DNA_SNAPSHOT_FOR_LATEST_TIMESTAMP_URL = RouteConfiguration.API_BASE + "/v1/projects/{projectId}/discovery/dna-snapshots/latest";

	public static final String BELONGS_TO_CLUSTER_URL = RouteConfiguration.API_BASE + "/v1/projects/{projectId}/discovery/dna-clusters/modules/{moduleId}";
	
	private final RestTemplate restTemplate = new RestTemplate();

	private final ParameterizedTypeReference<List<String>> responseType = new ParameterizedTypeReference<List<String>>() {};
	
	/**
	 * Test case to test if findCommunitiesJob is executing properly and returning the Job Id.
	 *
	 * @throws IOException if an I/O error occurs
	 */
	@Test
	void testDnaFindCommunities() throws IOException {
		final Result<String> result = MiningApiClient.discoveryService(assertNotNull(getConnectionInfo()))
				.findCommunities()
				.setProjectId(assertNotNull(ONE))
				.execute();
		assertEquals(202, result.getStatusCode());	
	}
	
	/**
	 * Test case to check the empty list of the DNA snapshots.
	 *
	 * @throws IOException if an I/O error occurs
	 */
	@Test
	void testEmptyListOfDnaSnapshots() throws IOException {
		final HttpEntity<String> request = new HttpEntity<>(getHttpHeaders(getConnectionInfo()));
		final ResponseEntity<List<String>> responseEntity =
				restTemplate.exchange(getConnectionInfo().getUrl() + LIST_OF_DNA_SNAPSHOT_URL, HttpMethod.GET, request, responseType, ZERO);
		assertNotNull(responseEntity);
		assertEquals(HttpStatus.OK.value(), responseEntity.getStatusCodeValue());
		final List<String> listOfSnapshots = responseEntity.getBody();
		assertTrue("The DNA snapshot list is not empty", assertNotNull(listOfSnapshots).isEmpty());
	}

	/**
	 * Test case to check the list of the DNA snapshots.
	 *
	 * @throws IOException if an I/O error occurs
	 */
	@Test
	void testListOfDnaSnapshots() throws IOException {
		final HttpEntity<String> request = new HttpEntity<>(getHttpHeaders(getConnectionInfo()));
		final List<String> listOfSnapshots = getListOfDnaSnapshots(request);
		assertEquals(2, listOfSnapshots.size());
	}

	/**
	 * Test case to check response of ModelDna for the non existing project.
	 *
	 * @throws IOException if an I/O error occurs
	 */
	@Test
	void testListOfDnaSnapshotsForNonExistingProject() throws IOException {
		try {
			final HttpEntity<String> request = new HttpEntity<>(getHttpHeaders(getConnectionInfo()));
			restTemplate.exchange(getConnectionInfo().getUrl() + LIST_OF_DNA_SNAPSHOT_URL, HttpMethod.GET, request, responseType, Long.valueOf(10));
		} catch (final HttpClientErrorException e) {
			assertEquals(HttpStatus.NOT_FOUND.value(), e.getRawStatusCode());
		}
	}

	/**
	 * Test case to check ModelDna for selected DNA snapshot .
	 *
	 * @throws IOException if an I/O error occurs
	 */
	@Test
	void testModelDnaForSelectedDnaSnapshot() throws IOException {
		final HttpEntity<String> request = new HttpEntity<>(getHttpHeaders(getConnectionInfo()));
		final List<String> listOfSnapshots = getListOfDnaSnapshots(request);
		assertFalse(listOfSnapshots.isEmpty());
		final ResponseEntity<ModelDna> response = restTemplate.exchange(getConnectionInfo().getUrl() + DNA_SNAPSHOT_FOR_SELECTED_TIMESTAMP_URL, HttpMethod.GET,
				request, ModelDna.class, ONE, listOfSnapshots.get(0));
		
		assertNotNull(response);
		assertEquals(HttpStatus.OK.value(), response.getStatusCodeValue());
		final ModelDna actualModelDna = assertNotNull(response.getBody());
		checkModelDnaContent(actualModelDna);
	}

	/**
	 * Test case to check ModelDna for selected DNA snapshot with non existing project.
	 *
	 * @throws IOException if an I/O error occurs
	 */
	@Test
	void testModelDnaForSelectedDnaSnapshotForNonExistingProject() throws IOException {
		final HttpEntity<String> request = new HttpEntity<>(getHttpHeaders(getConnectionInfo()));
		try {
			restTemplate.exchange(getConnectionInfo().getUrl() + DNA_SNAPSHOT_FOR_SELECTED_TIMESTAMP_URL, HttpMethod.GET, request, ModelDna.class,
					Long.valueOf(10), "1634113584262");
		} catch (final HttpClientErrorException e) {
			assertEquals(HttpStatus.NOT_FOUND.value(), e.getRawStatusCode());
		}
	}

	/**
	 * Test case to check ModelDna for selected DNA snapshot with non existing timestamp.
	 *
	 * @throws IOException if an I/O error occurs
	 */
	@Test
	void testModelDnaForSelectedDnaSnapshotForNonExistingTimestamp() throws IOException {
		final HttpEntity<String> request = new HttpEntity<>(getHttpHeaders(getConnectionInfo()));
		try {
			restTemplate.exchange(getConnectionInfo().getUrl() + DNA_SNAPSHOT_FOR_SELECTED_TIMESTAMP_URL, HttpMethod.GET, request, ModelDna.class, ONE,
					"1634113584262");
		} catch (final HttpClientErrorException e) {
			assertEquals(HttpStatus.NOT_FOUND.value(), e.getRawStatusCode());
		}
	}

	/**
	 * Test case to check ModelDna for latest DNA snapshot.
	 *
	 * @throws IOException if an I/O error occurs
	 */
	@Test
	void testModelDnaForLatestDnaSnapshot() throws IOException {
		final HttpEntity<String> request = new HttpEntity<>(getHttpHeaders(getConnectionInfo()));
		final ResponseEntity<ModelDna> response =
				restTemplate.exchange(getConnectionInfo().getUrl() + DNA_SNAPSHOT_FOR_LATEST_TIMESTAMP_URL, HttpMethod.GET, request, ModelDna.class, ONE);
		assertNotNull(response);
		assertEquals(HttpStatus.OK.value(), response.getStatusCodeValue());
		final ModelDna actualModelDna = assertNotNull(response.getBody());
		checkModelDnaContent(actualModelDna);
	}

	/**
	 * Test case to check ModelDna for non existing project.
	 *
	 * @throws IOException if an I/O error occurs
	 */
	@Test
	void testModelDnaForNonExistingProject() throws IOException {
		final HttpEntity<String> request = new HttpEntity<>(getHttpHeaders(getConnectionInfo()));
		try {
			restTemplate.exchange(getConnectionInfo().getUrl() + DNA_SNAPSHOT_FOR_LATEST_TIMESTAMP_URL, HttpMethod.GET, request, ModelDna.class, Long.valueOf(10));
		} catch (final HttpClientErrorException e) {
			assertEquals(HttpStatus.NOT_FOUND.value(), e.getRawStatusCode());
		}
	}

	/**
	 * Test case to check ModelDna for a project which is not having DNA snapshots.
	 *
	 * @throws IOException if an I/O error occurs
	 */
	@Test
	void testModelDnaForProjectWithoutSnapshot() throws IOException {
		final HttpEntity<String> request = new HttpEntity<>(getHttpHeaders(getConnectionInfo()));
		try {
			restTemplate.exchange(getConnectionInfo().getUrl() + DNA_SNAPSHOT_FOR_LATEST_TIMESTAMP_URL, HttpMethod.GET, request, ModelDna.class, TWO);
		} catch (final HttpClientErrorException e) {
			assertEquals(HttpStatus.NOT_FOUND.value(), e.getRawStatusCode());
		}
	}
	
	/**
	 * Test case to check response is NOT_FOUND when project does not exist.
	 *
	 * @throws IOException if an I/O error occurs
	 */
	@Test
	void testBelongsToClusterForNonExistingProject() throws IOException {
		checkBelongsToClusterNotFound(ZERO, Long.valueOf(2020));
	}

	
	/**
	 * Test case to check response is NOT_FOUND when module does not exist.
	 *
	 * @throws IOException if an I/O error occurs
	 */
	@Test
	void testBelongsToClusterForNonExistingModule() throws IOException {
		checkBelongsToClusterNotFound(ONE, ZERO);
	}
	
	/**
	 * Test case to check response is NOT_FOUND when no DNA data present for the given project.
	 *
	 * @throws IOException if an I/O error occurs
	 */
	@Test
	void testBelongsToClusterForNoSnapshot() throws IOException {
		checkBelongsToClusterNotFound(Long.valueOf(3), Long.valueOf(2040));		
	}
	
	/**
	 * Test case to check response is NOT_FOUND when module is not a part of any cluster for the given project.
	 *
	 * @throws IOException if an I/O error occurs
	 */
	@Test
	void testNoBelongsToClusterForModule() throws IOException {
		checkBelongsToClusterNotFound(ONE, Long.valueOf(2005));
	}
	
	/**
	 * Test case to check list of clusters for the latest snapshot of the given module.
	 *
	 * @throws IOException if an I/O error occurs
	 */
	@Test
	void testBelongsToCluster() throws IOException {
		final HttpEntity<String> request = new HttpEntity<>(getHttpHeaders(getConnectionInfo()));
		final ResponseEntity<String> responseEntity = restTemplate.exchange(getConnectionInfo().getUrl() + BELONGS_TO_CLUSTER_URL, HttpMethod.GET, request,
				String.class, ONE, Long.valueOf(2020));
		final ModelBelongsToClusters expectedResult = getExpectedModelBelongsToClusters(DnaSequencer.COBOL_METHOD_RULE, Integer.valueOf(1), EntityId.of(2020L));
		checkModelBelongsToCluster(new Gson().fromJson(responseEntity.getBody(), ModelBelongsToClusters.class), expectedResult);
	}
	
	/**
	 * Test case to check ModelBelongsToClusters when module is present in unassigned cluster.
	 *
	 * @throws IOException if an I/O error occurs
	 */
	@Test
	void testBelongsToClusterUnassignedCluster() throws IOException {
		final HttpEntity<String> request = new HttpEntity<>(getHttpHeaders(getConnectionInfo()));
		final ResponseEntity<String> responseEntity = restTemplate.exchange(getConnectionInfo().getUrl() + BELONGS_TO_CLUSTER_URL, HttpMethod.GET, request,
				String.class, ONE, Long.valueOf(2022));
		final ModelBelongsToClusters expectedResult = getExpectedModelBelongsToClusters(DnaSequencer.COBOL_METHOD_RULE, Integer.valueOf(-1),
				EntityId.of(2022L));
		checkModelBelongsToCluster(new Gson().fromJson(responseEntity.getBody(), ModelBelongsToClusters.class), expectedResult);
	}
	
	private List<String> getListOfDnaSnapshots(final HttpEntity<String> request) {
		final ResponseEntity<List<String>> responseEntity =
				restTemplate.exchange(getConnectionInfo().getUrl() + LIST_OF_DNA_SNAPSHOT_URL, HttpMethod.GET, request, responseType, ONE);
		assertNotNull(responseEntity);
		assertEquals(HttpStatus.OK.value(), responseEntity.getStatusCodeValue());
		final List<String> listOfSnapshots = assertNotNull(responseEntity.getBody());
		return listOfSnapshots;
	}
	
	private ModelDna getExpectedModelDna() {
		final ModelDna modelDna = new ModelDna();
		modelDna.setModuleCount(10);
		final ModelClustering modelClustering = new ModelClustering();
		final Map<String, String> algorithm = new HashMap<>();
		algorithm.put("Sequencer", "COBOL Methods");
		algorithm.put("Similarity", "Weighted Levenshtein");
		algorithm.put("Clustering", "Louvain");
		modelClustering.setAlgorithm(algorithm);
		final ModelAlgorithmOption maxLevels = new ModelAlgorithmOption("maxLevels", "Maximum Levels", "5");
		final ModelAlgorithmOption maxIterations = new ModelAlgorithmOption("maxIterations", "Maximum Iterations", "10");
		final ModelAlgorithmOption defaultTolerance = new ModelAlgorithmOption("defaultTolerance", "Default Tolerance", "0.0001");
		final ModelAlgorithmOption minDnaLength = new ModelAlgorithmOption("minDNALength", "Minimum DNA Length", "20");
		final ModelAlgorithmOption similarityThreshold = new ModelAlgorithmOption("similarity threshold", "Similarity Threshold", "0.85");
		modelClustering.setOptions(Arrays.asList(maxLevels, maxIterations, defaultTolerance, minDnaLength, similarityThreshold));
		modelClustering.setClusters(Arrays.asList(new ModelCluster(1, "", -1, "", "Unassigned Modules"), new ModelCluster(5, "", 1, "", "")));
		modelDna.setClusterings(Collections.singletonList(modelClustering));
		return modelDna;
	}
	
	private void checkModelDnaContent(final ModelDna actualModelDna) {
		final ModelDna expectedModelDna = getExpectedModelDna();
		final List<ModelClustering> expectedModelclusteringsList = expectedModelDna.getClusterings();
		assertFalse(expectedModelclusteringsList.isEmpty());
		final List<ModelClustering> actualModelclusteringsList = actualModelDna.getClusterings();
		assertFalse(actualModelclusteringsList.isEmpty());
		final ModelClustering expectedModelClustering = expectedModelclusteringsList.get(0);
		final ModelClustering actualModelClustering = actualModelclusteringsList.get(0);
		final List<ModelAlgorithmOption> expectedOptionsList = expectedModelClustering.getOptions();
		final List<ModelAlgorithmOption> actualOptionsList = actualModelClustering.getOptions();
		final List<ModelCluster> expectedClustersList = expectedModelClustering.getClusters();
		final List<ModelCluster> actualClustersList = actualModelClustering.getClusters().stream()
				.sorted(Comparator.comparingInt(ModelCluster::getClusterIndex))
				.collect(Collectors.toList());
		final ModelAlgorithmOption expectedModelAlgorithmOption = expectedOptionsList.get(0);
		final ModelAlgorithmOption actualModelAlgorithmOption = actualOptionsList.get(0);
		final ModelCluster expectedModelCluster = expectedClustersList.get(0);
		final ModelCluster actualModelCluster = actualClustersList.get(0);

		assertEquals(expectedModelDna.getModuleCount(), actualModelDna.getModuleCount());
		assertEquals(expectedModelclusteringsList.size(), actualModelclusteringsList.size());
		assertEquals(expectedModelClustering.getAlgorithm(), actualModelClustering.getAlgorithm());
		assertEquals(expectedOptionsList.size(), actualOptionsList.size());
		assertEquals(expectedModelAlgorithmOption.getName(), actualModelAlgorithmOption.getName());
		assertEquals(expectedModelAlgorithmOption.getTitle(), actualModelAlgorithmOption.getTitle());
		assertEquals(expectedModelAlgorithmOption.getValue(), actualModelAlgorithmOption.getValue());
		assertEquals(expectedClustersList.size(), actualClustersList.size());
		assertEquals(expectedModelCluster.getClusterDescription(), actualModelCluster.getClusterDescription());
		assertEquals(expectedModelCluster.getClusterTitle(), actualModelCluster.getClusterTitle());
		assertEquals(expectedModelCluster.getModuleCount(), actualModelCluster.getModuleCount());
	}
	
	private void checkBelongsToClusterNotFound(final Long projectId, final Long moduleId) {
		final HttpEntity<String> request = new HttpEntity<>(getHttpHeaders(getConnectionInfo()));
		try {
			restTemplate.exchange(getConnectionInfo().getUrl() + BELONGS_TO_CLUSTER_URL, HttpMethod.GET, request, String.class, projectId, moduleId);
		}catch (final HttpClientErrorException e) {
			assertEquals(HttpStatus.NOT_FOUND.value(), e.getRawStatusCode());
		}		
	}
	
	private ModelBelongsToClusters getExpectedModelBelongsToClusters(final DnaSequencer sequencerId, final Integer clusterIndex, final EntityId moduleId) {
		final List<ModelBelongsToClusters.ModelDnaCluster> modelDnaClusterList = new ArrayList<>();
		final ModelDnaAlgorithm modelDnaAlgorithm = new ModelDnaAlgorithm(sequencerId, DnaSimilarityAlgorithm.WEIGHTED_LEVENSHTEIN,
				DnaClusterAlgorithm.LOUVAIN);
		final ModelBelongsToClusters.ModelDnaCluster modelDnaCluster = new ModelBelongsToClusters().new ModelDnaCluster(clusterIndex, modelDnaAlgorithm);
		modelDnaClusterList.add(modelDnaCluster);
		return new ModelBelongsToClusters(moduleId, modelDnaClusterList);
	}

	private void checkModelBelongsToCluster(final ModelBelongsToClusters actual, final ModelBelongsToClusters expected) {
		final List<ModelDnaCluster> expectedModelDnaClusterList = expected.getDnaCluster();
		final ModelDnaCluster expectedModelDnaCluster = expectedModelDnaClusterList.get(0);
		final ModelDnaAlgorithm expectedModelDnaAlgorithm = expectedModelDnaCluster.getDnaAlgorithm();		
		final List<ModelDnaCluster> actualModelDnaClusterList = actual.getDnaCluster();		
		final ModelDnaCluster actualModelDnaCluster = actualModelDnaClusterList.get(0);		
		final ModelDnaAlgorithm actualModelDnaAlgorithm = actualModelDnaCluster.getDnaAlgorithm();
		
		assertEquals(expected.getModuleId(), actual.getModuleId());
		assertEquals(expectedModelDnaClusterList.size(), actualModelDnaClusterList.size());
		assertEquals(expectedModelDnaCluster.getClusterIndex(), actualModelDnaCluster.getClusterIndex());
		assertEquals(expectedModelDnaAlgorithm.getSequencerId(), actualModelDnaAlgorithm.getSequencerId());
		assertEquals(expectedModelDnaAlgorithm.getSimilarityId(), actualModelDnaAlgorithm.getSimilarityId());
		assertEquals(expectedModelDnaAlgorithm.getClusterAlgorithmId(), actualModelDnaAlgorithm.getClusterAlgorithmId());
	}
}
