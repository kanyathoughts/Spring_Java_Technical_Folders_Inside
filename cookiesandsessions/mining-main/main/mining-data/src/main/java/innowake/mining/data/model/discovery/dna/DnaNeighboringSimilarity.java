/*
 * Copyright (c) 2022 Deloitte. All rights reserved.
 */
package innowake.mining.data.model.discovery.dna;

import static innowake.lib.core.lang.Assert.assertNotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang3.tuple.Triple;

import innowake.lib.core.api.lang.Nullable;
import innowake.lib.core.util.tuple.Tuple2;
import innowake.mining.shared.model.ModuleUnit;

/**
 * Represents {@link ModuleUnit} and its neighboring {@linkplain ModuleUnit}s along with their similarities.
 */
public class DnaNeighboringSimilarity {
	
	@Nullable
	private String modulePath;
	
	@Nullable
	private Integer clusterIndex;

	private List<Tuple2<String, Double>> neighboringModules;
	
	/**
	 * creates a {@link DnaNeighboringSimilarity} from similarity list.
	 *
	 * @param neighboringModules the list consisting of neighboring modules and the similarity between them
	 * @param clusterIndex the cluster index of the DNA community
	 * @return {@link DnaNeighboringSimilarity}
	 */
	@Nullable
	public static DnaNeighboringSimilarity create(final List<Triple<String, String, Double>> neighboringModules, final Integer clusterIndex) {
		if (neighboringModules.isEmpty()) {
			return null;
		}
		final DnaNeighboringSimilarity neighboringSimilarity = new DnaNeighboringSimilarity();
		neighboringSimilarity.setClusterIndex(clusterIndex);
		neighboringSimilarity.setModulePath(determineSourcePath(neighboringModules));
		for (final var neighboringModule : neighboringModules) {
			final String fromPath = neighboringModule.getLeft();
			final String toPath = neighboringModule.getMiddle();
			final Double similarity = neighboringModule.getRight();
			if (fromPath.equals(neighboringSimilarity.getModulePath())) {
				neighboringSimilarity.addNeighbour(new Tuple2<>(toPath, similarity));
			} else {
				neighboringSimilarity.addNeighbour(new Tuple2<>(fromPath, similarity));
			}
		}
		return neighboringSimilarity;
	}
	
	/* Here we are trying to determine the path of the source module , to which we are trying to get the neighboring similarities. 
	 * For example, if we trying to get neighboring similarities for module1 then this module1 path will either be in from DnaString or to DnaString in a the 
	 * current neighboring similarity, so we are taking the common path as source path.
	 * */
	private static String determineSourcePath(final List<Triple<String, String, Double>> neighboringModules) {
		if (neighboringModules.size() == 1) {
			return neighboringModules.get(0).getLeft();
		}

		final String firstRowFromPath = neighboringModules.get(0).getLeft();
		final String firstRowToPath = neighboringModules.get(0).getMiddle();
			if (firstRowFromPath.equals(neighboringModules.get(1).getLeft()) || firstRowFromPath.equals(neighboringModules.get(1).getMiddle())) {
				return firstRowFromPath;
			} else {
				return firstRowToPath;
			}
	}
	
	/**
	 * Constructor for the {@link DnaNeighboringSimilarity}.
	 */
	public DnaNeighboringSimilarity() {
		this.neighboringModules = new ArrayList<>();
	}
	
	private double getAverageSimilarity(){
		return neighboringModules.stream().mapToDouble(tuple -> tuple.b).average().getAsDouble();
	}
	
	/**
	 * Returns the path of the {@link ModuleUnit}.
	 *
	 * @return the path of the {@link ModuleUnit}
	 */
	public String getModulePath() {
		return assertNotNull(modulePath);
	}
	
	/**
	 * Sets the path of the {@link ModuleUnit}.
	 *
	 * @param modulePath the path of the ModuleUnit
	 */
	public void setModulePath(final String modulePath) {
		this.modulePath = modulePath;
	}

	/**
	 * Adds neighboring similarity.
	 *
	 * @param neighbor neighboring module path and its similarity
	 */
	public void addNeighbour(final Tuple2<String, Double> neighbor) {
		this.neighboringModules.add(neighbor);
	}
	
	/**
	 * Sets the latest clusterIndex to which {@link ModuleUnit} belongs to.
	 * 
	 * @param clusterIndex the cluster index of the DNA community
	 */
	public void setClusterIndex(final Integer clusterIndex) {
		this.clusterIndex = clusterIndex;
	}
	
	/**
	 * Gets the clusterIndex to which {@link ModuleUnit} belongs to.
	 * 
	 * @return the cluster index of the DNA community
	 */
	public Integer getClusterIndex() {
		return assertNotNull(clusterIndex);
	}

	@Override
	public String toString() {
		return  clusterIndex + "," + modulePath + ","  
				+ neighboringModules.stream().map(neighbour -> neighbour.a + "," + neighbour.b).collect(Collectors.joining(","))
				+ "," + getAverageSimilarity();
	}
}
