/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.server.discovery.dna.similarity;

import org.springframework.stereotype.Service;

/**
 * Example implementation for a weighted Levenshtein usage.
 * Per default no costs are modified.
 */
@Service
public class DefaultWeightedLevenshtein implements WeightedLevenshtein {

	private static final double DEFAULT_WEIGHT = 1;
	//private static final Map<String, Double> finalWeightMap;

	static {
		//final Map<String, Double> weightMap = new HashMap<String, Double>();
		//weightMap.put("move", 0.5);
		//finalWeightMap = Collections.unmodifiableMap(weightMap);
	}

	@Override
	public double deletionCost(final String s) {
		return DEFAULT_WEIGHT;
		//return finalWeightMap.containsKey(s) ? finalWeightMap.get(s).doubleValue() : DEFAULT_WEIGHT;
	}

	@Override
	public double insertionCost(final String s) {
		return DEFAULT_WEIGHT;
		//return finalWeightMap.containsKey(s) ? finalWeightMap.get(s).doubleValue() : DEFAULT_WEIGHT;
	}

	@Override
	public double substitutionCost(final String s1, final String s2) {
		return DEFAULT_WEIGHT;
		//final double w1 = finalWeightMap.containsKey(s1) ? finalWeightMap.get(s1).doubleValue() : DEFAULT_WEIGHT;
		//final double w2 = finalWeightMap.containsKey(s2) ? finalWeightMap.get(s2).doubleValue() : DEFAULT_WEIGHT;
		//return Math.max(w1, w2);
	}

}
