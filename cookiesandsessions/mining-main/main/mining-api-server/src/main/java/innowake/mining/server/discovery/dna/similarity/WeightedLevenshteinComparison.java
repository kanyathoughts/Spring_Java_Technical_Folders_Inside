/* Copyright (c) 2022 Deloitte. All rights reserved. */
package innowake.mining.server.discovery.dna.similarity;

import static innowake.lib.core.lang.Assert.assertNotNull;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.mapping;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.log.Logger;
import innowake.lib.core.log.LoggerFactory;
import innowake.lib.job.api.ProgressMonitor;
import innowake.lib.job.api.Result;
import innowake.mining.server.discovery.Logging;
import innowake.mining.server.properties.GenericConfigProperties;
import innowake.mining.shared.access.DnaDataService;
import innowake.mining.shared.access.EntityId;
import innowake.mining.shared.entities.dna.DnaSequencer;
import innowake.mining.shared.entities.dna.DnaSimilarityAlgorithm;
import innowake.mining.shared.entities.dna.DnaSimilarityPojoPrototype;

/**
 * Implementation of a weighted levenshtein algorithm. <br>
 * <br>
 * The computation happens in segments to avoid loading all the dna strings into memory.
 * This task must be called multiple times based on the size of the dna strings and the partition size.
 * This partition size decides how the dna strings must be split and processed. Processing is done as follows. <br>
 *
 * Consider the list as {1, 2, 3, 4, 5, 6, 7, 8} and partition size as 2. Now this task must be called 4 times with {@code startPosition} as 0, 2, 4 and 6.<br>
 * In the first call the similarity is computed for the following pairs. Start position: 0<br>
 * <b>pair self:</b> (1, 2)<br>
 * <b>pair two:</b> (1, 3), (1, 4), (2, 3), (2, 4), (1, 5), (1, 6), (2, 5), (2, 6), (1, 7), (1, 8), (2, 7), (2, 8)<br>
 * <br>
 * In the second call the similarity is computed for the following pairs. Start position: 2<br>
 * <b>pair self:</b> (3, 4)<br>
 * <b>pair two:</b> (3, 5), (3, 6), (4, 5), (4, 6), (3, 7), (3, 8), (4, 7), (4, 8)<br>
 * <br>
 * In the third call the similarity is computed for the following pairs. Start position: 4<br>
 * <b>pair self:</b> (5, 6)<br>
 * <b>pair two:</b> (5, 7), (5, 8), (6, 7), (6, 8)<br>
 * <br>
 * In the fourth call the similarity is computed for <b>pair self:</b> (7, 8) only.
 */
public class WeightedLevenshteinComparison extends SimilarityCalculatorTask<Integer> {

	private static final Logger LOG = LoggerFactory.getLogger(Logging.DNA);
	@Nullable
	private transient WeightedLevenshtein weightedLevenshtein;
	@Autowired
	private transient DnaDataService dnaDataService;
	@Autowired
	private transient GenericConfigProperties configProperties;

	private final DnaSequencer sequencerId;
	private final List<UUID> dnaStringUuids;
	private final int startPosition;
	private final int partitionSize;
	private final ProgressMonitor progressMonitor;
	private final double similarityThreshold;

	/**
	 * Constructor.
	 *
	 * @param progressMonitor the {@link ProgressMonitor} that should be used for this task created by {@link ProgressMonitor#subMonitor(int)}
	 * @param jobId the Id of the job this task belongs to
	 * @param sequencerId the sequencer id to process
	 * @param dnaStringUuids the list of dna string {@link UUID UUIDs} to process
	 * @param startPosition the start index from which the similarity must be computed
	 * @param partitionSize the partition size to split the DNA string list for DNA similarity computation
	 * @param similarityThreshold the similarity threshold value to persist into database
	 */
	public WeightedLevenshteinComparison(final ProgressMonitor progressMonitor, final String jobId, final DnaSequencer sequencerId,
			final List<UUID> dnaStringUuids, final int startPosition, final int partitionSize ,final double similarityThreshold) {
		super(progressMonitor, jobId);
		this.sequencerId = sequencerId;
		this.dnaStringUuids = dnaStringUuids;
		this.startPosition = startPosition;
		this.partitionSize = partitionSize;
		this.progressMonitor = progressMonitor;
		this.similarityThreshold = similarityThreshold;
	}

	@Override
	public DnaSimilarityAlgorithm getId() {
		return DnaSimilarityAlgorithm.WEIGHTED_LEVENSHTEIN;
	}

	@Override
	protected Result<Integer> run(final ProgressMonitor progressMonitor) {
		LOG.trace(() -> "Processing DNA similarities for the dna strings at the index: " + startPosition);

		final Map<UUID, List<String>> dnaStringsMap = populateDnaStringElements(startPosition);
		int processedCount = pairSelf(dnaStringsMap);

		for (int i = startPosition + partitionSize; i < dnaStringUuids.size(); i += partitionSize) {
			processedCount += pairTwoList(dnaStringsMap, populateDnaStringElements(i));
		}

		LOG.trace(() -> "Processed DNA similarities for the dna strings at the index: " + startPosition);
		return new Result<>(Integer.valueOf(processedCount));
	}

	@Autowired
	public void setWeightedLevenshtein(final WeightedLevenshtein weightedLevenshtien) {
		this.weightedLevenshtein = weightedLevenshtien;
	}

	private Map<UUID, List<String>> populateDnaStringElements(final int startPos) {
		if (startPos < 0 || startPos >= dnaStringUuids.size()) {
			return Collections.emptyMap();
		}

		final var partition = dnaStringUuids.subList(startPos, Math.min(startPos + partitionSize, dnaStringUuids.size()));
		return dnaDataService.findDnaStringElementValues(builder -> builder.byModules(partition).bySequencer(sequencerId)).stream()
								.collect(groupingBy(t -> t.a, mapping(t -> t.b, Collectors.toList())));
		
	}
	
	private int pairSelf(final Map<UUID, List<String>> dnaStringsMap) {
		LOG.info(() -> String.format("Computing self similarities for %d modules", Integer.valueOf(dnaStringsMap.size())));

		final List<UUID> identifier = new ArrayList<>(dnaStringsMap.keySet());
		Collections.sort(identifier);
		final List<DnaSimilarityPojoPrototype> similarities = new ArrayList<>(determineCombination(identifier.size()));

		final DnaSimilarityAlgorithm similarityAlgorithm = getId();
		for (int i = 0; i < identifier.size() - 1; i++) {
			final UUID leftIdentifier = identifier.get(i);
			final List<String> leftDnaStringList = dnaStringsMap.get(leftIdentifier);
			final var leftDna = leftDnaStringList.size();
			final var fromModule = EntityId.of(leftIdentifier);
			for (int j = i + 1; j < identifier.size(); j++) {
				final UUID rightIdentifier = identifier.get(j);
				final List<String> rightDnaStringList = dnaStringsMap.get(rightIdentifier);
				final int rightDna = rightDnaStringList.size();
				final double distance = distance(leftDnaStringList, rightDnaStringList, Double.MAX_VALUE);
				final double ratio = 1.0 - (distance / (Math.max(leftDna, rightDna) + 1));

				similarities.add(new DnaSimilarityPojoPrototype()
											.setSequencer(sequencerId)
											.setSimilarityAlgorithm(similarityAlgorithm)
											.setSimilarity(ratio)
											.setAModule(fromModule)
											.setBModule(EntityId.of(rightIdentifier)));
			}
		}

		persist(similarities);

		return similarities.size();
	}

	private int pairTwoList(final Map<UUID, List<String>> dnaStringsMap1, final Map<UUID, List<String>> dnaStringsMap2) {
		LOG.info(() -> String.format("Computing pair similarities for %d with %d modules", Integer.valueOf(dnaStringsMap1.size()), Integer.valueOf(dnaStringsMap2.size())));
		final int total = dnaStringsMap1.size() * dnaStringsMap2.size();
		if (total < 1) {
			return 0;
		}

		final List<UUID> identifier1 = new ArrayList<>(dnaStringsMap1.keySet());
		Collections.sort(identifier1);
		final List<UUID> identifier2 = new ArrayList<>(dnaStringsMap2.keySet());
		Collections.sort(identifier2);

		final List<DnaSimilarityPojoPrototype> similarities = new ArrayList<>(total);
		final DnaSimilarityAlgorithm similarityAlgorithm = getId();

		for (int i = 0; i < identifier1.size(); i++) {
			final UUID leftIdentifier = identifier1.get(i);
			final List<String> leftDnaStringList = dnaStringsMap1.get(leftIdentifier);
			final int leftDna = leftDnaStringList.size();
			final var fromModule = EntityId.of(leftIdentifier);
			for (int j = 0; j < identifier2.size(); j++) {
				final UUID rightIdentifier = identifier2.get(j);
				final List<String> rightDnaStringList = dnaStringsMap2.get(rightIdentifier);
				final int rightDna = rightDnaStringList.size();
				final double distance = distance(leftDnaStringList, rightDnaStringList, Double.MAX_VALUE);
				final double ratio = 1.0 - (distance / (Math.max(leftDna, rightDna) + 1));
				similarities.add(new DnaSimilarityPojoPrototype()
											.setSequencer(sequencerId)
											.setSimilarityAlgorithm(similarityAlgorithm)
											.setSimilarity(ratio)
											.setAModule(fromModule)
											.setBModule(EntityId.of(rightIdentifier)));
			}
		}

		persist(similarities);

		return similarities.size();
	}

	private void persist(final List<DnaSimilarityPojoPrototype> similarities) {
		LOG.trace(() -> "Similarities computed. Now persisting the data into database");
		final List<DnaSimilarityPojoPrototype> filteredDnaSimilarities;
		if (configProperties.isDiscoveryDnaOptimalPersist()) {
			filteredDnaSimilarities = similarities.stream().filter(s -> s.similarity.getNonNull().doubleValue() >= similarityThreshold).collect(Collectors.toList());
			progressMonitor.worked(similarities.size() - filteredDnaSimilarities.size());
		} else {
			filteredDnaSimilarities = similarities;
		}
		dnaDataService.create(filteredDnaSimilarities, Optional.of(progressMonitor::worked));
		LOG.trace(() -> "Persisting the data into database completed");
	}

	private static int determineCombination(final int size) {
		return size * (size - 1) / 2;
	}

	/**
	 * Compute Levenshtein distance using provided weights for substitution.
	 *
	 * @param dnaStringList1 The first list of DNA strings to compare.
	 * @param dnaStringList2 The second list of DNA strings to compare.
	 * @param limit The maximum result to compute before stopping. This
	 *              means that the calculation can terminate early if you
	 *              only care about strings with a certain similarity.
	 *              Set this to Double.MAX_VALUE if you want to run the
	 *              calculation to completion in every case.
	 * @return The computed weighted Levenshtein distance.
	 * @throws NullPointerException if s1 or s2 is null.
	 */
	public final double distance(final List<String> dnaStringList1, final List<String> dnaStringList2, final double limit) {
		final var localWeightedLevenshtein = assertNotNull(this.weightedLevenshtein);
		if (dnaStringList1.equals(dnaStringList2)) {
			return 0;
		}

		if (dnaStringList1.isEmpty() || dnaStringList2.isEmpty()) {
			return Math.max(dnaStringList1.size(), dnaStringList2.size());
		}

		/* create two work vectors of floating point (i.e. weighted) distances */
		var v0 = new double[dnaStringList2.size() + 1];
		var v1 = new double[dnaStringList2.size() + 1];
		double[] vtemp;

		/*
		 * initialize v0 (the previous row of distances)
		 * this row is A[0][i]: edit distance for an empty s1
		 *  the distance is the cost of inserting each character of s2
		 */
		v0[0] = 0;
		for (int i = 1; i < v0.length; i++) {
			v0[i] = v0[i - 1] + localWeightedLevenshtein.insertionCost(dnaStringList2.get(i - 1));
		}

		for (final String s1i : dnaStringList1) {
			final double deletion_cost = localWeightedLevenshtein.deletionCost(s1i);

			/*
			 *  calculate v1 (current row distances) from the previous row v0
			 *  first element of v1 is A[i+1][0]
			 *  Edit distance is the cost of deleting characters from s1
			 *  to match empty t.
			 */
			v1[0] = v0[0] + deletion_cost;

			double minv1 = v1[0];

			/* use formula to fill in the rest of the row */
			for (var j = 0; j < dnaStringList2.size(); j++) {
				final var s2j = dnaStringList2.get(j);
				v1[j + 1] = Math.min(v1[j] + localWeightedLevenshtein.insertionCost(s2j), // Cost of insertion
						Math.min(v0[j + 1] + deletion_cost, // Cost of deletion
								v0[j] + ( ! s1i.equals(s2j) ? localWeightedLevenshtein.substitutionCost(s1i, s2j) : 0))); // Cost of substitution

				minv1 = Math.min(minv1, v1[j + 1]);
			}

			if (minv1 >= limit) {
				return limit;
			}

			/*
			 *  copy v1 (current row) to v0 (previous row) for next iteration
			 *  System.arraycopy(v1, 0, v0, 0, v0.length)
			 *  Flip references to current and previous row
			 *
			 */
			vtemp = v0;
			v0 = v1;
			v1 = vtemp;
		}

		return v0[dnaStringList2.size()];
	}

}
