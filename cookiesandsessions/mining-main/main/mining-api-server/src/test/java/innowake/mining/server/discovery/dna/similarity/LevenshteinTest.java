/* Copyright (c) 2019 innoWake gmbh Germany. All rights reserved. */
package innowake.mining.server.discovery.dna.similarity;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.lang.StringUtils;
import org.junit.Test;
import org.mockito.Mockito;

import innowake.lib.job.api.ProgressMonitor;
import innowake.mining.shared.entities.dna.DnaSequencer;

/**
 * Tests the levenshtein implementation.
 *
 */
public class LevenshteinTest {

	private final static String TEST_STRING_1 = "Hello Levenshtein!";
	private final static String TEST_STRING_2 = "This string is different..";

	@Test
	public void testWeightedLevenshtein() {
		final WeightedLevenshteinComparison comp = getWeightedLevenshteinComparison(Optional.empty());

		assertEquals(0.0, comp.distance(Arrays.asList("a", "b"), Arrays.asList("a", "b"), Double.MAX_VALUE), 0.0);
		assertEquals(1.0, comp.distance(Arrays.asList("a", "b"), Arrays.asList("a", "c"), Double.MAX_VALUE), 0.1);
		assertEquals(2.0, comp.distance(Arrays.asList("a", "d", "c"), Arrays.asList("a", "d", "f", "f"), Double.MAX_VALUE), 0.1);
		assertEquals(2.0, comp.distance(Arrays.asList("a", "d", "f", "f"), Arrays.asList("a", "d", "c"), Double.MAX_VALUE), 0.1);
	}

	@Test
	public void testLevenshtein() {
		final WeightedLevenshteinComparison comp = getWeightedLevenshteinComparison(Optional.empty());

		assertEquals(0, comp.distance(Arrays.asList("a", "b"), Arrays.asList("a", "b"), Double.MAX_VALUE), 0.1);
		assertEquals(1.0, comp.distance(Arrays.asList("a", "b"), Arrays.asList("a", "c"), Double.MAX_VALUE), 0.1);
	}

	@Test
	public void compareToExternalImplementation() {
		final WeightedLevenshtein weight = new WeightedLevenshtein() {

			@Override
			public double substitutionCost(final String s1, final String s2) {
				return 2;
			}

			@Override
			public double insertionCost(final String s) {
				return 2;
			}

			@Override
			public double deletionCost(final String s) {
				return 2;
			}
		};

		final WeightedLevenshteinComparison comp = getWeightedLevenshteinComparison(Optional.of(weight));

		final double nativeDist = StringUtils.getLevenshteinDistance(TEST_STRING_1, TEST_STRING_2);
		final double customDist = comp.distance(covertToListOfStrings(TEST_STRING_1), covertToListOfStrings(TEST_STRING_2), Double.MAX_VALUE);
		assertEquals(nativeDist * 2, customDist, 0.01);
	}

	private WeightedLevenshteinComparison getWeightedLevenshteinComparison(final Optional<WeightedLevenshtein> weight) {
		final WeightedLevenshteinComparison comp = new WeightedLevenshteinComparison(Mockito.mock(ProgressMonitor.class), "", DnaSequencer.COBOL_METHOD_RULE,
				Collections.emptyList(), 0, 0, 0.0);
		comp.setWeightedLevenshtein(weight.orElse(new DefaultWeightedLevenshtein()));

		return comp;
	}

	private List<String> covertToListOfStrings(final String s) {
		final String[] splitted = s.split("");
		return Arrays.stream(splitted).collect(Collectors.toList());
	}

}
